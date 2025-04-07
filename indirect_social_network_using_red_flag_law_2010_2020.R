library(flem)
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
# Create the adjacency matrix from df_for_matrix_weights
k <- graph.data.frame(df_for_matrix_weights, directed = FALSE, vertices = nodes)
sci_matrix <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE)
sci_matrix <- as.matrix(cumulative_sci_weighted)
sci_matrix_sum <- rowSums(sci_matrix)
sci_matrix <- sweep(sci_matrix, 1, sci_matrix_sum, `/`)
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

# Step 2: Map Counties (GEOID) to States in SCI Matrix
# Ensure alignment of SCI matrix rows (GEOID) with `county_to_state`
county_to_state <- t_1 %>%
  select(GEOID, state) %>%
  distinct() %>%
  arrange(GEOID)

# Check if SCI matrix rows align with counties in the dataset
if (!all(rownames(sci_matrix) %in% county_to_state$GEOID)) {
  stop("Mismatch between SCI matrix rows and GEOIDs in data!")
}

# Step 3: Compute PolicyNetExposure for Each County-Year
# Initialize the exposure matrix
policy_net_exposure <- matrix(0, nrow = nrow(sci_matrix), ncol = length(years),
                              dimnames = list(rownames(sci_matrix), years))

# Loop through each year and compute exposure
for (t in 1:length(years)) {
  # Create policy status for the year t
  policy_status <- rep(0, nrow(sci_matrix))
  
  # Map state-level policies to counties (GEOIDs) in the SCI matrix
  for (i in seq_len(nrow(sci_matrix))) {
    county <- rownames(sci_matrix)[i]  # Get the GEOID for the county
    state <- county_to_state %>% filter(GEOID == county) %>% pull(state)
    
    if (length(state) > 0 && state %in% rownames(policy_matrix)) {
      policy_status[i] <- policy_matrix[state, t]
    }
  }
  
  # Compute weighted exposure using the SCI matrix
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


### all three did models ###
## no spill over ####
did_no_spill <- felm(
  death_rates_per_100_k ~ D_it + ACS_MEDIAN_HH_INC+
    ACS_PCT_AGE_18_44+ACS_PCT_AGE_45_64+ACS_PCT_ENGL_NOT_WELL+percentage_uninsured+
    race_cat+ACS_PCT_UNEMPLOY+ACS_PCT_LT_HS+population_density+Poor.mental.health.days.raw.value
  | GEOID + year         # Fixed Effects
  | 0                    # No instrumental variables
  | GEOID,               # Cluster by state
  data =final_result,
  weights = final_result$ACS_TOT_POP_WT
)
summary(did_no_spill)

### spatial spillover

did_spill_spatial <- felm(
  death_rates_per_100_k ~ D_it +  DistancePolicyNetExposure+
    ACS_PCT_AGE_18_44+ACS_PCT_AGE_45_64+ACS_PCT_ENGL_NOT_WELL+percentage_uninsured+
    race_cat+ACS_PCT_UNEMPLOY+ACS_PCT_LT_HS+population_density+Poor.mental.health.days.raw.value
  | GEOID + year | 0 | GEOID, 
  data = final_result,
  weights = final_result$ACS_TOT_POP_WT
)

summary(did_spill_spatial)



### social spillover ###
did_spill_social <- felm(
  death_rates_per_100_k ~ D_it + RedFlagLawExposure +
    ACS_PCT_AGE_18_44+ACS_PCT_AGE_45_64+ACS_PCT_ENGL_NOT_WELL+percentage_uninsured+
    race_cat+ACS_PCT_UNEMPLOY+ACS_PCT_LT_HS+population_density+Poor.mental.health.days.raw.value
  | GEOID + year | 0 | GEOID, 
  data = final_result,
  weights = final_result$ACS_TOT_POP_WT
)

summary(did_spill_social)


### spatial and social spillover ####
did_spill_social_spatial <- felm(
  death_rates_per_100_k ~ D_it + RedFlagLawExposure + DistancePolicyNetExposure+ACS_PCT_AGE_18_44+ACS_PCT_AGE_45_64+
 ACS_PCT_ENGL_NOT_WELL+percentage_uninsured+
    race_cat+ACS_PCT_UNEMPLOY+ACS_PCT_LT_HS+population_density+Poor.mental.health.days.raw.value
  | GEOID + year | 0 | GEOID, 
 data = final_result,
 weights = final_result$ACS_TOT_POP_WT
)

summary(did_spill_social_spatial)


write.csv(final_result, 'policy_data_2010_2020.csv')


