library(lfe)
library(tidyverse)
library(igraph)
library(zipcodeR)
library(tidyverse)
library(tigris) # for fips_codes data
library(rvest) #for web scrapping
library(zipcodeR) #for ZIP codes
library(fuzzyjoin)
library(tidycensus)
library(geodist)
library(haven)
library(scales)
library(tidycensus)
library(tigris)### for lat and lng
library(sf)### for lat and lng 
library(readxl)
library(spdep)
library(lmtest)
library(sandwich)
library(did)
library(DRDID)
library(lubridate)
library(zoo)
library(SDPDmod)
library(readr)
library(stargazer)
library(modelsummary)
library(broom)      
library(dplyr)      
library(ggplot2)    


### read csv ###

my_data_with_spatial_g <- read_csv("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/spatial_social_proximity_suicide_mortality_2010_2020_cdc_wonder_data_gravity_weights.csv")
my_data_with_spatial_g <- my_data_with_spatial_g[,-1]


years <- 2010:2020



### red flag law social exposure ### 
policy_data <- data.frame(
  state = c("CT", "MA", "IN", "IL", "CA", "DC", "WA", "NY", "OR", "CO",
            "FL", "NV", "VT", "HI", "MD", "VA", "RI", "NM", "NJ", "MN", "DE", "MI"),
  start_year = c(1999, 2018, 2005, 2018, 2014, 2018, 2016, 2019, 2017, 2020,
                 2018, 2020, 2018, 2020, 2018, 2020, 2018, 2020, 2018, 2023, 2018, 2024)
)

### out of state social spillover ###
t_1 <- left_join(my_data_with_spatial_g, policy_data, by = "state") %>%
  mutate(start_year = replace_na(start_year, 0))

# 2) Create key DID indicators
t_1 <- t_1 %>%
  mutate(
    ever_treated = if_else(start_year > 0, 1, 0),
    post         = if_else(year >= start_year & start_year > 0, 1, 0)
  )
# Create a 0/1 policy variable called D_it
# This indicates that county i, in year t, is treated if (ever_treated==1 & post==1).
t_1 <- t_1 %>%
  mutate(D_it = as.numeric(ever_treated * post))





## creating SCI matrix ###
# Reading and processing the TSV file
df_0 <- read_tsv("C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv")
df_1 <- df_0 %>%
  filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc) %>%
  filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))

# Preparing data for adjacency matrix
df_for_matrix_weights <- df_1 %>% select(user_loc, fr_loc, scaled_sci)
nodes <- social_df %>% select(fr_loc) %>% distinct()

# Create adjacency matrix
k <- graph.data.frame(df_for_matrix_weights, directed = FALSE, vertices = nodes)
cumulative_sci_weighted <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE)
cumulative_sci_weighted <- as.matrix(cumulative_sci_weighted)
diag(cumulative_sci_weighted) <- 0

# Adjusting weights by population
population <- suicide_mortality %>%
  group_by(FIPS) %>%
  summarise(population = round(mean(ACS_TOT_POP_WT, na.rm = TRUE))) %>%
  filter(FIPS %in% nodes$fr_loc) %>%
  arrange(match(FIPS, nodes$fr_loc))  # Ensure order matches the adjacency matrix

pop_vector <- population$population

# Scale adjacency matrix by population
cumulative_sci_weighted <- sweep(cumulative_sci_weighted, 2, pop_vector, `*`)

# Normalize rows
row_sums_cumulative_sci_weighted <- rowSums(cumulative_sci_weighted)
cumulative_sci_weighted_normalized <- sweep(cumulative_sci_weighted, 1, row_sums_cumulative_sci_weighted, `/`)
cumulative_sci_weighted_normalized[is.na(cumulative_sci_weighted_normalized)] <- 0
diag(cumulative_sci_weighted_normalized) <- 0  # Ensure no self-loops

# Store the normalized matrix as `w_i_j`
sci_matrix <- cumulative_sci_weighted_normalized
### redflag law net exposure ###
# Define the relevant years
years <- 2010:2020

# Step 1: Create Policy Matrix for All Counties
# Ensure all states from `t_1` are represented
county_states <- unique(t_1$state)

# Create a policy matrix for all states
policy_matrix <- matrix(0, nrow = length(county_states), ncol = length(years),
                        dimnames = list(county_states, years))
for (i in 1:nrow(policy_data)) {
  state <- policy_data$state[i]
  start_year <- policy_data$start_year[i]
  if (state %in% rownames(policy_matrix)) {
    policy_matrix[state, years >= start_year] <- 1
  }
}

##############################################################################
# Step 2: Map Counties (GEOID) to States in SCI Matrix
##############################################################################
county_to_state <- t_1 %>%
  select(GEOID, state) %>%
  distinct() %>%
  arrange(GEOID)

# --- NEW: impose s(i) != s(j)  ----------------------------------------------
# Vector of state labels in the same order as rows of sci_matrix
state_vec <- county_to_state$state[match(rownames(sci_matrix),
                                         county_to_state$GEOID)]

# Identify within‑state dyads (off‑diagonal)
same_state_mask <- outer(state_vec, state_vec, `==`)
diag(same_state_mask) <- FALSE            # diagonal already zero

# Zero out within‑state social ties
sci_matrix[same_state_mask] <- 0

# Re‑normalise each row so Σ_j w_ij = 1 (or 0 if no out‑state neighbours)
row_sums_sci <- rowSums(sci_matrix)
sci_matrix    <- sweep(sci_matrix, 1, row_sums_sci, `/`)
sci_matrix[is.na(sci_matrix)] <- 0
# ---------------------------------------------------------------------------

# Check if SCI matrix rows align with counties in the dataset
if (!all(rownames(sci_matrix) %in% county_to_state$GEOID)) {
  stop("Mismatch between SCI matrix rows and GEOIDs in data!")
}

##############################################################################
# Step 3: Compute PolicyNetExposure for Each County‑Year
##############################################################################
# Initialize the exposure matrix
policy_net_exposure <- matrix(0, nrow = nrow(sci_matrix), ncol = length(years),
                              dimnames = list(rownames(sci_matrix), years))

# Loop through each year and compute exposure
for (t in 1:length(years)) {
  # Create policy status for the year t
  policy_status <- rep(0, nrow(sci_matrix))
  
  # Map state‑level policies to counties (GEOIDs) in the SCI matrix
  for (i in seq_len(nrow(sci_matrix))) {
    county <- rownames(sci_matrix)[i]  # GEOID for the county
    state  <- county_to_state %>% filter(GEOID == county) %>% pull(state)
    
    if (length(state) > 0 && state %in% rownames(policy_matrix)) {
      policy_status[i] <- policy_matrix[state, t]
    }
  }
  
  # Compute weighted exposure using the corrected SCI matrix
  policy_net_exposure[, t] <- sci_matrix %*% policy_status
}

# Step 4: Convert PolicyNetExposure to Long Format
policy_net_exposure_df <- as.data.frame(policy_net_exposure)
policy_net_exposure_df$GEOID <- rownames(policy_net_exposure)
policy_net_exposure_long <- tidyr::pivot_longer(
  policy_net_exposure_df,
  cols = -GEOID,
  names_to = "year",
  values_to = "RedFlagLawExposure"
)
policy_net_exposure_long$year <- as.numeric(policy_net_exposure_long$year)

# Step 5: Merge with Original `t_1` Data
result <- t_1 %>%
  left_join(policy_net_exposure_long, by = c("GEOID", "year"))

# View the resulting dataset
head(result)

### gravity weights exposure ####
# Extract unique counties for the distance matrix
unique_counties <- my_data_with_spatial_g %>%
  dplyr::distinct(GEOID, Longitude, Latitude) %>%
  dplyr::arrange(GEOID)

# Compute geodesic distance in kilometers
distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")],
                                measure = "geodesic") / 1000

# Initialize a matrix of the same size
gravity_matrix <- InvDistMat(distance_km)

# Optionally assign row/column names
rownames(gravity_matrix) <- unique_counties$GEOID
colnames(gravity_matrix) <- unique_counties$GEOID

# 4) Normalize each row to create A_{i,j}
row_sums <- rowSums(gravity_matrix)
gravity_matrix <- sweep(gravity_matrix, 1, row_sums, FUN = "/")
diag(gravity_matrix) <- 0



# Create a policy matrix for all states
policy_matrix <- matrix(0, nrow = length(county_states), ncol = length(years),
                        dimnames = list(county_states, years))

for (i in 1:nrow(policy_data)) {
  state <- policy_data$state[i]
  start_year <- policy_data$start_year[i]
  if (state %in% rownames(policy_matrix)) {
    policy_matrix[state, years >= start_year] <- 1
  }
}

# Step 2: Map Counties (GEOID) to States
county_to_state <- t_1 %>%
  select(GEOID, state) %>%
  distinct() %>%
  arrange(GEOID)

# ---------- NEW: drop within‑state gravity links ----------
# vector of states in the same order as gravity_matrix rows / cols
state_vec <- county_to_state$state[match(rownames(gravity_matrix),
                                         county_to_state$GEOID)]

# logical mask: TRUE if pair of counties lies in the same state (off‑diagonal)
same_state_mask <- outer(state_vec, state_vec, `==`)
diag(same_state_mask) <- FALSE       # diagonal already zero

# zero out within‑state ties
gravity_matrix[same_state_mask] <- 0

# re‑normalise rows  ⇒  Σ_j w_ij = 1   (or 0 if no out‑state neighbours)
row_sums <- rowSums(gravity_matrix)
gravity_matrix <- sweep(gravity_matrix, 1, row_sums, "/")
gravity_matrix[is.na(gravity_matrix)] <- 0
# ----------------------------------------------------------


# Step 3: Compute Distance-Based Policy Exposure
# Initialize the exposure matrix
distance_policy_netexposure <- matrix(0, nrow = nrow(gravity_matrix), ncol = length(years),
                                      dimnames = list(rownames(gravity_matrix), years))

# Loop through each year and compute exposure
for (t in 1:length(years)) {
  policy_status <- rep(0, nrow(gravity_matrix))
  
  # Map state-level policies to counties in the gravity matrix
  for (i in seq_len(nrow(gravity_matrix))) {
    county <- rownames(gravity_matrix)[i]
    state <- county_to_state %>% filter(GEOID == county) %>% pull(state)
    
    if (length(state) > 0 && state %in% rownames(policy_matrix)) {
      policy_status[i] <- policy_matrix[state, t]
    }
  }
  
  # Compute weighted exposure using the gravity distance matrix
  distance_policy_netexposure[, t] <- gravity_matrix %*% policy_status
}

# Step 4: Convert Distance-Based Policy Exposure to Long Format
distance_policy_netexposure_df <- as.data.frame(distance_policy_netexposure)
distance_policy_netexposure_df$GEOID <- rownames(distance_policy_netexposure)

distance_policy_netexposure_long <- tidyr::pivot_longer(
  distance_policy_netexposure_df,
  cols = -GEOID,
  names_to = "year",
  values_to = "DistancePolicyNetExposure"
)
distance_policy_netexposure_long$year <- as.numeric(distance_policy_netexposure_long$year)
t_1
# Step 5: Merge with Original `t_1` Data
result <- t_1 %>%
  left_join(distance_policy_netexposure_long, by = c("GEOID", "year"))

# View the resulting dataset
head(result)


# Step 5: Merge Both Exposure Measures
# Ensure both datasets (SCI-based and Distance-based) are aligned
combined_exposure <- policy_net_exposure_long %>%
  full_join(distance_policy_netexposure_long, by = c("GEOID", "year"))

# Merge with Original `t_1` Data
final_result <- t_1 %>%
  left_join(combined_exposure, by = c("GEOID", "year"))

# View the resulting dataset
head(final_result)

final_result$state_year <- interaction(final_result$state, final_result$year, drop = TRUE)
### all three did models ###
## no spill over ####
did_no_spill <- felm(
  death_rates_per_100_k ~ D_it+ population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + year        # Fixed Effects
  | 0                    # No instrumental variables
  | GEOID,               # Cluster by state
  data =final_result,
  weights = final_result$ACS_TOT_POP_WT
)
summary(did_no_spill)

### spatial spillover

did_spill_spatial <- felm(
  death_rates_per_100_k ~ DistancePolicyNetExposure+
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + state_year | 0 | GEOID, 
  data = final_result,
  weights = final_result$ACS_TOT_POP_WT
)

summary(did_spill_spatial)



### social spillover ###
did_spill_social <- felm(
  death_rates_per_100_k ~  RedFlagLawExposure +
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + state_year | 0 | GEOID, 
  data = final_result,
  weights = final_result$ACS_TOT_POP_WT
)

summary(did_spill_social)


### spatial and social spillover ####
did_spill_social_spatial <- felm(
  death_rates_per_100_k ~ RedFlagLawExposure +  DistancePolicyNetExposure+  population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + state_year | 0 | GEOID, 
 data = final_result,
 weights = final_result$ACS_TOT_POP_WT
)

summary(did_spill_social_spatial)

### updated variables ####

stargazer(
  did_no_spill, 
  did_spill_spatial, 
  did_spill_social, 
  did_spill_social_spatial,
  type = "latex",
  title = "Effect of Policy Exposure on Death Rates (No Spillover, Spatial Spillover, Social Spillover, Both)",
  column.labels = c("No Spillover", "Spatial Spillover", "Social Spillover", "Social + Spatial"),
  dep.var.labels = "Deaths per 100K",
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt"
)


write.csv(final_result, 'policy_data_2010_2020.csv')

### drawing the plot ### 

# 1. Extract estimate and 95 % CI for the target coefficient
coef_df <- bind_rows(
  tidy(did_spill_social,          conf.int = TRUE) %>% 
    filter(term == "RedFlagLawExposure") %>% 
    mutate(model = "Social"),
  
  tidy(did_spill_social_spatial,  conf.int = TRUE) %>% 
    filter(term == "RedFlagLawExposure") %>% 
    mutate(model = "Social + Spatial")
)

# 2. Plot
ggplot(coef_df,
       aes(x = estimate, y = model, color = model)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.15, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("Social" = "red",
                                "Social + Spatial" = "blue")) +
  labs(x = "deaths per 100,000 people", y = "")+
  theme_minimal(base_size = 11) +
  theme(
    legend.position      = "none",
    panel.grid.major.x   = element_blank(),   # remove vertical major grid lines
    panel.grid.minor.x   = element_blank()    # remove vertical minor grid lines
  )



# ### extension of spatial spillover ###
# ## 1.  Translate the ERPO state list to FIPS codes -------------------
# erpo_fips <- fips_codes |>
#   dplyr::filter(state %in% erpo_states) |>
#   dplyr::distinct(state_code) |>
#   dplyr::pull(state_code)               # vector like "01","05",…
# 
# ## 2.  Identify counties *in* ERPO states ---------------------------
# erpo_counties <- cnty_sf$GEOID[ cnty_sf$STATEFP %in% erpo_fips ]
# 
# ## 3.  Flag cross‑state neighbours ----------------------------------
# nb_queen <- spdep::poly2nb(cnty_sf, queen = TRUE, row.names = cnty_sf$GEOID)
# 
# border_flag <- logical(length(nb_queen))
# for(i in seq_along(nb_queen)){
#   neigh <- nb_queen[[i]]
#   border_flag[i] <- any(cnty_sf$STATEFP[neigh] %in% erpo_fips) &&
#     !(cnty_sf$STATEFP[i]    %in% erpo_fips)
# }
# cnty_sf$border_ERPO <- border_flag
# 
# ## 4.  Drop border counties and create trimmed sample ---------------
# border_drop <- cnty_sf |>
#   sf::st_drop_geometry() |>
#   dplyr::filter(border_ERPO) |>
#   dplyr::pull(GEOID)
# 
# sample_rd <- final_result |>
#   dplyr::filter(!(GEOID %in% border_drop))
# 
# ### re building the redflaglaw exposure ###
# ## 1 ─── Trim SCI matrix to the kept counties and renormalise ────────────────
# keep_idx  <- which(!(rownames(sci_matrix) %in% border_drop))
# sci_trim  <- sci_matrix[keep_idx, keep_idx]
# 
# row_sums  <- rowSums(sci_trim)
# sci_trim  <- sweep(sci_trim, 1, row_sums, "/")
# sci_trim[is.na(sci_trim)] <- 0          # isolated counties → 0‑row
# diag(sci_trim) <- 0                     # no self‑loops
# 
# ## 2 ─── Mapping GEOID → state for the trimmed sample ───────────────────────
# county_to_state <- sample_rd %>%            # uses trimmed outcome data
#   select(GEOID, state) %>%
#   distinct()
# 
# ## 3 ─── State‑level policy matrix (only states present in sample_rd) ───────
# county_states <- unique(county_to_state$state)
# policy_matrix <- matrix(
#   0, nrow = length(county_states), ncol = length(years),
#   dimnames = list(county_states, years)
# )
# 
# for(i in seq_len(nrow(policy_data))){
#   st <- policy_data$state[i]
#   if(st %in% county_states){
#     start_y <- policy_data$start_year[i]
#     policy_matrix[st, years >= start_y] <- 1
#   }
# }
# 
# ## 4 ─── Compute Red‑Flag‑Law social exposure for each county‑year ───────────
# policy_net_exposure <- matrix(
#   0, nrow = nrow(sci_trim), ncol = length(years),
#   dimnames = list(rownames(sci_trim), years)
# )
# 
# for(t in seq_along(years)){
#   policy_status <- rep(0, nrow(sci_trim))
#   for(i in seq_len(nrow(sci_trim))){
#     county <- rownames(sci_trim)[i]
#     st     <- county_to_state$state[match(county, county_to_state$GEOID)]
#     if(!is.na(st)) policy_status[i] <- policy_matrix[st, t]
#   }
#   policy_net_exposure[, t] <- sci_trim %*% policy_status
# }
# 
# ## 5 ─── Long format and merge into the analysis data set ────────────────────
# policy_net_exposure_long <- as.data.frame(policy_net_exposure) %>%
#   mutate(GEOID = rownames(.)) %>%
#   pivot_longer(-GEOID, names_to = "year",
#                values_to = "RedFlagLawExposure_dp") %>%
#   mutate(year = as.integer(year))
# 
# sample_rd  <- sample_rd %>%     # ← existing trimmed outcome frame
#   left_join(policy_net_exposure_long, by = c("GEOID", "year"))
# 
# ## 6 ─── Sanity check --------------------------------------------------------
# stopifnot(max(abs(rowSums(sci_trim) - 1)) < 1e-12)  # rows sum to 1
# 
# sample_rd$state_year <- interaction(sample_rd$state, sample_rd$year, drop = TRUE)
# did_spill_social_nb <- felm(
#   death_rates_per_100_k ~  RedFlagLawExposure_dp +
#     population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
#     prop_black + prop_asian + prop_other + prop_hispanic +     
#     ACS_MEDIAN_HH_INC + 
#     ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
#     ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
#     Poor.mental.health.days.raw.value
#   | GEOID + state_year | 0 | GEOID, 
#   data = sample_rd,
#   weights = sample_rd $ACS_TOT_POP_WT
# )
# 
# summary(did_spill_social_nb)
# 
# 
# ### creating the event study plots for additional casual interpreations ### 
# ## --- 1·1  county ↦ home‑state mapping (already have)
# county_to_state <- final_result |>                      # GEOID, state
#   distinct(GEOID, state)
# 
# event_window <- -5:10                       
# 
# ##############################################################################
# # 1.  county‑to‑state social weights  w_ij  (unchanged)
# ##############################################################################
# state2cnty <- split(final_result$GEOID, final_result$state)
# 
# w_long <- map_dfr(names(state2cnty), function(st_j){
#   numer <- rowSums(sci_matrix[, state2cnty[[st_j]], drop = FALSE])
#   denom <- rowSums(sci_matrix[, unlist(state2cnty), drop = FALSE])
#   tibble(GEOID = rownames(sci_matrix),
#          state_j = st_j,
#          w_ij = ifelse(denom > 0, numer / denom, 0))
# })
# 
# ##############################################################################
# # 2.  Stack event‑time slices  (CORRECTED)
# ##############################################################################
# stacked <- map2_dfr(policy_data$state, policy_data$start_year,
#                     function(st_ev, tau_ev){
#                       
#                       final_result %>%
#                         filter(year  %in% (tau_ev + event_window),
#                                state != st_ev) %>%          # keep *out‑of‑state* only
#                         left_join(w_long %>% filter(state_j == st_ev),
#                                   by = "GEOID") %>%         # adds w_ij
#                         mutate(event_id = paste0("EV_", st_ev),
#                                k        = year - tau_ev)
#                     })
# 
# # Drop baseline event time k = −1
# stacked <- stacked %>%
#   mutate(k = factor(k, levels = event_window)) %>%
#   filter(k != -1) %>%
#   mutate(fe_cnty = interaction(event_id, GEOID, drop = TRUE),
#          fe_sy   = interaction(event_id, state, year, drop = TRUE))
# 
# ##############################################################################
# # 3.  Re‑run the Wilson event‑study
# ##############################################################################
# es_mod <- feols(
#   death_rates_per_100_k ~ w_ij:k +
#     population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
#     prop_black + prop_asian + prop_other + prop_hispanic +
#     ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
#     ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + `Poor.mental.health.days.raw.value`
#   | fe_cnty + fe_sy,
#   data    = stacked,
#   weights = ~ ACS_TOT_POP_WT,
#   cluster = ~ GEOID)
# 
# summary(es_mod, se = "cluster")
# coef_df <- broom::tidy(es_mod, conf.int = TRUE) |>
#   filter(grepl("w_ij:k", term)) |>
#   mutate(k = as.numeric(sub("w_ij:k", "", term)))
# 
# ggplot(coef_df, aes(x = k, y = estimate)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_point() +
#   geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = .15) +
#   scale_x_continuous(breaks = event_window[event_window != -1]) +
#   labs(x = "Years since adoption (k)", y = expression(hat(alpha)[k]),
#        title = "Wilson‑style event‑study: social spillover from out‑of‑state Red‑Flag laws")
