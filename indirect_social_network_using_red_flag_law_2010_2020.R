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
library(data.table)
library(dplyr)


### read csv ###
#### loading the data set ### 
df_suicide_mortality <- read.csv('C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/suicide_ideation_cdc_Wonder_1999_2020.csv',colClasses = c(FIPS = "character"))
### df suicide mortality for years 2010 2020 ###
df_suicide_mortality_2010_2020 <- df_suicide_mortality %>% filter(year %in% c(2010:2020))
### df only mortality related data points ####
df_suicide_mortality_2010_2020 <- df_suicide_mortality_2010_2020[,c(2,3,4,5)]
### adding year wise population in the data set ####
# Define the range of years
years <- 2010:2020


# Define the data path
data_path <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/SDOH_Covariates/sdoh_csvs"

# 1) Enumerate yearly SDOH files
files <- list.files(
  path = data_path,
  pattern = "^sdoh_\\d{4}\\.csv$",          # sdoh_2010.csv, sdoh_2011.csv, ...
  full.names = TRUE
)

# 2) Loop over files, construct race & ethnicity proportions
for (f in files) {
  
  year <- sub(".*sdoh_(\\d{4}).*", "\\1", basename(f))
  
  # Read as data frame with COUNTYFIPS preserved as character
  tmp <- read.csv(f, colClasses = c(COUNTYFIPS = "character"))
  
  # ---- Create proportion (0–1) covariates ----------------------------------
  tmp$prop_black     <- tmp$ACS_PCT_BLACK          / 100
  tmp$prop_asian     <- tmp$ACS_PCT_ASIAN_NONHISP  / 100
  tmp$prop_other     <- (tmp$ACS_PCT_AIAN_NONHISP +
                           tmp$ACS_PCT_NHPI_NONHISP +
                           tmp$ACS_PCT_MULT_RACE_NONHISP) / 100
  tmp$prop_hispanic  <- tmp$ACS_PCT_HISPANIC       / 100
  
  # Optional quality check: ensure shares do not exceed 1
  tmp$share_check <- tmp$prop_black + tmp$prop_asian + tmp$prop_other +
    (1 - tmp$prop_hispanic)  # adds White_NH implicitly
  if (any(tmp$share_check > 1.001, na.rm = TRUE)) {
    warning(sprintf("Share constraint violated in %s (year %s)", f, year))
  }
  tmp$share_check <- NULL
  
  # 3) Store processed data frame in the workspace
  assign(paste0("df_", year), tmp)
}



# 2) Combine into a single panel dataset
years <- sub(".*sdoh_(\\d{4}).*", "\\1", basename(files))

my_panel <- do.call(
  bind_rows,
  lapply(years, function(yr) {
    df_name <- paste0("df_", yr)
    d <- get(df_name)
    
    if (!"YEAR" %in% names(d)) {
      d$YEAR <- as.numeric(yr)
    }
    
    required_vars <- c(
      "COUNTYFIPS", "YEAR", "ACS_TOT_POP_WT","prop_black","prop_asian", "prop_other", "prop_hispanic",
      "ACS_MEDIAN_HH_INC", "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS", 
      "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64", 
      "ACS_PCT_ENGL_NOT_WELL", "SAHIE_PCT_UNINSURED64", "ACS_PCT_UNINSURED"
    )
    
    vars_present <- intersect(required_vars, names(d))
    
    for (v in setdiff(required_vars, vars_present)) {
      d[[v]] <- NA
    }
    
    d %>% select(all_of(required_vars))
  })
)

# Confirm COUNTYFIPS is character
str(my_panel$COUNTYFIPS)

# 3) Filter to contiguous US counties
my_panel <- my_panel %>% 
  filter(COUNTYFIPS %in% df_suicide_mortality_2010_2020$FIPS)

### merging the data ###
suicide_mortality <- df_suicide_mortality_2010_2020 %>%
  left_join(
    my_panel,
    by = c( "FIPS"="COUNTYFIPS" , "year"= "YEAR")
  )
### adding latitude and longitude in the data ###
counties <- counties(cb = TRUE, class = "sf")
entire_american_fips_vector <- unique(suicide_mortality$FIPS)
### filtering the east_american_fips_Vector from counties###
selected_counties <- counties[counties$GEOID %in% entire_american_fips_vector, ]

#### getting the centroids ###
centroids <- st_centroid(selected_counties)
centroids <- centroids[order(centroids$GEOID ),]
coords <- st_coordinates(centroids)
### ordering the counties by geoid ###
selected_counties <- selected_counties[order(selected_counties$GEOID), ]
### getting the lat lng ###
geoid_lat_lng<- data.frame(
  GEOID = selected_counties$GEOID,
  Longitude = coords[,1],
  Latitude = coords[,2]
)
### adding lat and lng information to the county information ####
suicide_mortality <- left_join(suicide_mortality,geoid_lat_lng, by=c("FIPS"="GEOID"))
### creating deaths per 100,000 population ####
suicide_mortality <- suicide_mortality %>% mutate(death_rates_per_100_k = (deaths/ACS_TOT_POP_WT)*100000)
### verifying the panel data is complete ##### 
# Define FIPS codes to remove (historical changes)
# 15005 does not have complete set of neighbors necessary for our analysis
fips_to_remove <- c("02158", "02261", "46102", "15005")

# Remove these FIPS from the dataset
suicide_mortality <- suicide_mortality %>% 
  filter(!FIPS %in% fips_to_remove)

# Re-run the completeness check
panel_completeness <- suicide_mortality %>%
  group_by(FIPS) %>%
  summarise(n_years = n_distinct(year))

# Check if panel is now balanced
table(panel_completeness$n_years)  # Should ideally be 11 for all
### deaths social proximity ####
# Subset and rename columns
#Preparing `social_df` with necessary columns
social_df <- suicide_mortality[, c("FIPS", "ACS_TOT_POP_WT", "death_rates_per_100_k")]
colnames(social_df)[1] <- "fr_loc"
colnames(social_df)[3] <- "deaths_per_capita"
social_df <- social_df[order(social_df$fr_loc), ]

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
w_i_j <- cumulative_sci_weighted_normalized


# Loop over each unique year and compute `s_minus_i`
results_list <- list()  # Store results for each year

for (yr in unique(suicide_mortality$year)) {
  
  # Subset data for this year
  sub_data <- suicide_mortality %>% filter(year == yr)
  
  # Align the subset with the rows/columns of `w_i_j`
  idx <- match(sub_data$FIPS, rownames(w_i_j))  # Match row order
  
  # Create a submatrix that includes only relevant counties for this year
  w_ij_sub <- w_i_j[idx, idx, drop = FALSE]
  
  # Extract the outcome vector (e.g., death rates per 100k)
  y <- sub_data$death_rates_per_100_k
  
  # Ensure alignment of outcome vector
  y <- as.matrix(y)
  
  # Compute s_minus_i as the weighted sum of neighbors' outcomes
  s_minus_i_vec <- w_ij_sub %*% y
  
  # Add the computed `s_minus_i` back to the subset data
  sub_data$s_minus_i <- s_minus_i_vec
  
  # Store the updated subset data for this year
  results_list[[as.character(yr)]] <- sub_data
}





#### defining all data ###
# 1) Combine all yearly subsets (stored in results_list) into one data frame
all_data <- bind_rows(results_list)

# 2) Rename FIPS -> GEOID so the calculateSpatialProximity_by_year() function can find it
all_data <- all_data %>%
  rename(GEOID = FIPS)

#### geoids that do not have lat and lng ###
missing_coords <- all_data %>%
  filter(is.na(Longitude) | is.na(Latitude)) %>%
  distinct(GEOID)

missing_coords


# Extract unique counties for the distance matrix
unique_counties <- all_data %>%
  dplyr::distinct(GEOID, Longitude, Latitude) %>%
  dplyr::arrange(GEOID)

# Compute geodesic distance in kilometers
distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")],
                                measure = "geodesic") / 1000

# 2) Gravity-weighted function: w(d_{ij}) = 1/(d_{ij}^alpha)
#    If distance is zero (for the diagonal or otherwise), we handle it separately below.
#    We'll first transform only off-diagonal entries.

# Initialize a matrix of the same size
gravity_matrix <- InvDistMat(distance_km)

# Optionally assign row/column names
rownames(gravity_matrix) <- unique_counties$GEOID
colnames(gravity_matrix) <- unique_counties$GEOID

# 4) Normalize each row to create A_{i,j}
row_sums <- rowSums(gravity_matrix)
A_ij <- sweep(gravity_matrix, 1, row_sums, FUN = "/")
diag(A_ij) <- 0


### calculating d_minus_i

results_list_spatial <- list()  # Store results for each year

for (yr in unique(suicide_mortality$year)) {
  
  # Subset data for this year
  sub_data <- all_data %>% filter(year == yr)
  
  # Align the subset with the rows/columns of `A_ij`
  idx <- match(sub_data$GEOID, rownames(A_ij))  # Match row order
  
  # Create a submatrix that includes only relevant counties for this year
  A_ij_sub <- A_ij[idx, idx, drop = FALSE]
  
  # Extract the outcome vector (e.g., death rates per 100k)
  y <- sub_data$death_rates_per_100_k
  
  # Ensure alignment of outcome vector
  y <- as.matrix(y)
  
  # Compute d_minus_i as the weighted sum of neighbors' outcomes
  d_minus_i_vec <- A_ij_sub %*% y
  
  # Add the computed `d_minus_i` back to the subset data
  sub_data$d_minus_i <- as.numeric(d_minus_i_vec)
  
  # Store the updated subset data for this year in results list
  results_list_spatial[[as.character(yr)]] <- sub_data
}

# Combine all yearly subsets into one final dataset
my_data_with_spatial_g <- bind_rows(results_list_spatial)


### population density ###
counties <- counties(year = 2018, cb = TRUE)
# Calculate area in square kilometers
counties <- st_transform(counties, crs = 5070)  # Transform to Albers Equal Area for accurate area calculation
counties <- counties %>%
  mutate(area_sq_km = as.numeric(st_area(geometry)) / 1e6)  # Convert area to square kilometers

counties <- counties %>% filter(GEOID %in% my_data_with_spatial_g$GEOID) %>% select(c("GEOID", "area_sq_km"))
my_data_with_spatial_g <- merge(my_data_with_spatial_g,counties,by="GEOID")
my_data_with_spatial_g <- my_data_with_spatial_g %>% mutate(population_density=ACS_TOT_POP_WT/area_sq_km)


### lets us see the year wise variation ##




#### frequent mental health distress rate data #####

# Define a list of years and their corresponding URLs
mhd_2010 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2010.csv")
mhd_2011 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2011.csv")
mhd_2012 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2012.csv")
mhd_2013 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2013.csv")
mhd_2014 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2014.csv")
mhd_2015 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2015.csv")
mhd_2016 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2016.csv")
mhd_2017 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2017.csv")
mhd_2018 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/analytic_data2018_0.csv")
mhd_2019 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2019.csv")
mhd_2020 <- read.csv("https://www.countyhealthrankings.org/sites/default/files/media/document/analytic_data2020_0.csv")

# List of datasets with corresponding years
data_list <- list(
  "2010" = mhd_2010, "2011" = mhd_2011, "2012" = mhd_2012, "2013" = mhd_2013, 
  "2014" = mhd_2014, "2015" = mhd_2015, "2016" = mhd_2016, "2017" = mhd_2017, 
  "2018" = mhd_2018, "2019" = mhd_2019, "2020" = mhd_2020
)

# Initialize empty list to store processed data
panel_data_mhd <- list()

# Loop through each dataset
for (year in names(data_list)) {
  df <- data_list[[year]]
  
  # Check if the required column exists
  if ("Poor.mental.health.days.raw.value" %in% names(df)) {
    df <- df %>%
      select(X5.digit.FIPS.Code, Poor.mental.health.days.raw.value) %>%
      mutate(Release.Year = as.integer(year))
    
    panel_data_mhd[[year]] <- df
  }
}

# Combine all years into a single dataframe
final_panel_data_mhd <- bind_rows(panel_data_mhd)

#### final data mhd ####
final_panel_data_mhd <- final_panel_data_mhd %>% filter(X5.digit.FIPS.Code %in% my_data_with_spatial_g$GEOID) %>% select(c("X5.digit.FIPS.Code", "Release.Year", "Poor.mental.health.days.raw.value",))
colnames(final_panel_data_mhd)[1] <- "GEOID"
colnames(final_panel_data_mhd)[2] <- "year"
### merging the data ###3
my_data_with_spatial_g <- merge(my_data_with_spatial_g,final_panel_data_mhd, by=c("GEOID","year"), all=TRUE)
my_data_with_spatial_g$Poor.mental.health.days.raw.value <- as.numeric(my_data_with_spatial_g$Poor.mental.health.days.raw.value)


### finding out for what year and what data points the mental heatlh distress value are not there

missing_summary <- my_data_with_spatial_g %>%
  select(
    GEOID, 
    year, 
    state, 
    ACS_MEDIAN_HH_INC,
    ACS_PCT_UNEMPLOY,
    Poor.mental.health.days.raw.value
  ) %>%
  pivot_longer(
    cols = c(ACS_MEDIAN_HH_INC, ACS_PCT_UNEMPLOY, Poor.mental.health.days.raw.value),
    names_to = "variable",
    values_to = "value"
  ) %>%
  filter(is.na(value))

missing_summary
### imputing the data by mean value per year ###

my_data_with_spatial_g <- my_data_with_spatial_g %>%
  group_by(year) %>%
  mutate(
    ACS_MEDIAN_HH_INC = ifelse(
      is.na(ACS_MEDIAN_HH_INC),
      mean(ACS_MEDIAN_HH_INC, na.rm = TRUE),
      ACS_MEDIAN_HH_INC
    ),
    ACS_PCT_UNEMPLOY = ifelse(
      is.na(ACS_PCT_UNEMPLOY),
      mean(ACS_PCT_UNEMPLOY, na.rm = TRUE),
      ACS_PCT_UNEMPLOY
    ),
    Poor.mental.health.days.raw.value = ifelse(
      is.na(Poor.mental.health.days.raw.value),
      mean(Poor.mental.health.days.raw.value, na.rm = TRUE),
      Poor.mental.health.days.raw.value
    )
  ) %>%
  ungroup()


### regression ####
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(percentage_uninsured = coalesce(ACS_PCT_UNINSURED, SAHIE_PCT_UNINSURED64))

years <- 2010:2020

 
## selected covariates ###

new_data_set <- my_data_with_spatial_g %>%
  select(GEOID, year,state, ACS_TOT_POP_WT, prop_black,
    prop_asian, prop_other, prop_hispanic, ACS_MEDIAN_HH_INC,
    ACS_PCT_UNEMPLOY, ACS_PCT_LT_HS, ACS_PCT_AGE_18_44,
    ACS_PCT_AGE_45_64, ACS_PCT_ENGL_NOT_WELL, SAHIE_PCT_UNINSURED64,
    ACS_PCT_UNINSURED, Longitude, Latitude, population_density,
    Poor.mental.health.days.raw.value, percentage_uninsured
  )




### red flag law social exposure ### 
policy_data <- data.frame(
  state      = c("CT","IN","CA","WA","OR","FL","VT","MD","RI","DE",
                 "MA","NJ","IL","NY","DC","CO","NV","HI","NM","VA"),
  start_year = c(1999,2005,2016,2016,2018,2018,2018,2018,2018,2018,
                 2018,2019,2019,2019,2019,2020,2020,2020,2020,2020)
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

### ERPO exposure ###
# ==========================================================
#  ERPO social-exposure   (robust version)
# ==========================================================

pad5 <- function(x) sprintf("%05d", as.integer(x))

# ---------- 1.  load raw files ------------------------------------------
sci  <- fread("C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv")
meta <- fread("C:/Users/kusha/Downloads/county_data.csv",
              select = c("GEOID", "state")) |> unique()

# ---------- 2.  ensure FIPS columns exist -------------------------------
if (!all(c("i_fips","j_fips") %in% names(sci))) {
  setnames(sci,
           old = names(sci)[1:2],          # assume first two columns = FIPS
           new = c("i_fips","j_fips"))
}
sci[,  `:=`(i_fips = pad5(i_fips),
            j_fips = pad5(j_fips))]
meta[, GEOID := pad5(GEOID)]

# ---------- 3.  identify / rename SCI column ----------------------------
if (!"SCI" %in% names(sci)) {
  num_cols <- names(Filter(is.numeric, sci))
  num_cols <- setdiff(num_cols, c("i_fips","j_fips"))
  if (length(num_cols) != 1)
    stop("Cannot unambiguously identify SCI column.")
  setnames(sci, num_cols, "SCI")
}

# ---------- 4.  origin / destination states -----------------------------
# (rename if present, add via joins if absent)
if ("i.state" %in% names(sci)) setnames(sci, "i.state", "i_state")
if ("state"   %in% names(sci)) setnames(sci, "state",   "j_state")

if (!"i_state" %in% names(sci)) {
  sci <- sci[meta, on = .(i_fips = GEOID), nomatch = 0]
  setnames(sci, "state", "i_state")
}
if (!"j_state" %in% names(sci)) {
  sci <- sci[meta, on = .(j_fips = GEOID), nomatch = 0, allow.cartesian = TRUE]
  setnames(sci, "state", "j_state")
}

# ---------- 5.  weight normalisation & collapse -------------------------
sci[, totalSCI := sum(SCI), by = i_fips]
sci[, w_ij     := SCI / totalSCI]

w_i_state <- sci[i_state != j_state,
                 .(w_is = sum(w_ij)),          # Σ weights to state j
                 by = .(i_fips, j_state)]

# ---------- 6.  ERPO adoption calendar ---------------------------------
policy_data <- data.table(
  state      = c("CT","IN","CA","WA","OR","FL","VT","MD","RI","DE",
                 "MA","NJ","IL","NY","DC","CO","NV","HI","NM","VA"),
  start_year = c(1999,2005,2016,2016,2018,2018,2018,2018,2018,2018,
                 2018,2019,2019,2019,2019,2020,2020,2020,2020,2020)
)

years_panel <- sort(unique(my_data_with_spatial_g$year))

state_year <- CJ(state = unique(meta$state), year = years_panel)[
  policy_data, on = "state"][
    , ERPO_active := as.integer(!is.na(start_year) &
                                  year >= start_year)][
                                    , start_year := NULL]

# ---------- 7.  county-year exposure ------------------------------------
expo <- w_i_state[state_year,
                  on = .(j_state = state),    # match destination state
                  allow.cartesian = TRUE][
                    ERPO_active == 1L,
                    .(ERPO_exposure = sum(w_is)),
                    by = .(i_fips, year)]

# ---------- 8.  merge into analysis panel -------------------------------
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(GEOID = pad5(GEOID)) %>%
  left_join(expo, by = c("GEOID" = "i_fips", "year")) %>%
  mutate(ERPO_exposure = replace_na(ERPO_exposure, 0))

# ------------------------------------------------------------------------
# my_data_with_spatial_g now contains a fully-computed ERPO_exposure
# ------------------------------------------------------------------------


# Extract unique counties for the distance matrix
unique_counties <- my_data_with_spatial_g %>%
  dplyr::distinct(GEOID, Longitude, Latitude) %>%
  dplyr::arrange(GEOID)

# Compute geodesic distance in kilometers
distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")],
                                measure = "geodesic") / 1000

# Initialize a matrix of the same size
gravity_matrix <- InvDistMat(distance_km,powr=0.1)

# Optionally assign row/column names
rownames(gravity_matrix) <- unique_counties$GEOID
colnames(gravity_matrix) <- unique_counties$GEOID

# 4) Normalize each row to create A_{i,j}
row_sums <- rowSums(gravity_matrix)
gravity_matrix <- sweep(gravity_matrix, 1, row_sums, FUN = "/")
diag(gravity_matrix) <- 0


## --- 1.  gravity-matrix → long table ---------------------------------
gravity_dt <- as.data.table(as.table(gravity_matrix))
setnames(gravity_dt, c("i_fips","j_fips","w_ij"))

gravity_dt[, `:=`( i_fips = pad5(as.character(i_fips)),
                   j_fips = pad5(as.character(j_fips)) )]

## --- 2.  destination-state membership ---------------------------------
gravity_dt <- gravity_dt[meta, on = .(j_fips = GEOID), nomatch = 0]     # add j_state
setnames(gravity_dt, "state", "j_state")

## --- 3.  attach yearly ERPO status ------------------------------------
expo_dist <- gravity_dt[state_year,
                        on = .(j_state = state), allow.cartesian = TRUE][
                          ERPO_active == 1L,                       # keep active years
                          .(InvDist_exposure = sum(w_ij)),         # Σ weights to ERPO states
                          by = .(i_fips, year)]

## --- 4.  merge into analysis panel ------------------------------------
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(GEOID = pad5(GEOID)) %>%
  left_join(expo_dist, by = c("GEOID" = "i_fips", "year")) %>%
  mutate(InvDist_exposure = replace_na(InvDist_exposure, 0))


# -----------------------------------------------------------------------
# my_data_with_spatial_g now contains InvDist_exposure (inverse-distance
# based ERPO exposure) alongside ERPO_exposure from SCI weights.
# -----------------------------------------------------------------------

## --- Add ERPO treatment indicator D_it ---------------------------------
my_data_with_spatial_g <- my_data_with_spatial_g %>% 
  left_join(policy_data, by = "state") %>%                 # attach adoption year
  mutate(
    start_year   = replace_na(start_year, 0L),             # 0 ⇒ never adopts
    ever_treated = as.integer(start_year > 0L),            # county’s state adopts at any time
    post         = as.integer(year >= start_year &         # observation occurs after adoption
                                start_year > 0L),
    D_it         = ever_treated * post                     # DiD indicator
  )


### all three did models ###
my_data_with_spatial_g$state_year <- interaction(my_data_with_spatial_g$state, my_data_with_spatial_g$year, drop = TRUE)

## experimenting with network exposure ####

covariates <- c(
  "s_minus_i", "d_minus_i","ERPO_exposure","InvDist_exposure",
  "population_density", "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64",
  "prop_black", "prop_asian", "prop_other", "prop_hispanic",
  "ACS_MEDIAN_HH_INC", "ACS_PCT_ENGL_NOT_WELL", "percentage_uninsured",
  "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS", "Poor.mental.health.days.raw.value"
)

normalised <- function(x)
{
  (x - mean(x)) / sd(x)
}

# Apply Min-Max scaling after Z-score standardization
my_data_with_spatial_g[covariates] <- as.data.frame(lapply(my_data_with_spatial_g[covariates], normalised))




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
  | state,               # Cluster by state
  data =my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(did_no_spill)

### spatial spillover

did_spill_spatial <- felm(
  death_rates_per_100_k ~ InvDist_exposure+
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + state_year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(did_spill_spatial)



### social spillover ###
did_spill_social <- felm(
  death_rates_per_100_k ~  ERPO_exposure +
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + state_year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(did_spill_social)


### spatial and social spillover ####
did_spill_social_spatial <- felm(
  death_rates_per_100_k ~ ERPO_exposure +  InvDist_exposure +  population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + state_year | 0 |state ,
 data = my_data_with_spatial_g,
 weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(did_spill_social_spatial)


### event plot design ###
# ------------------------------------------------------------
# 0.  Libraries
# ------------------------------------------------------------
library(data.table)      # fast stacking / joins
library(fixest)          # high-dim FE + cluster
library(ggplot2)         # plotting
library(broom)           # tidy() for fixest

# ------------------------------------------------------------
# 1.  Inputs
# ------------------------------------------------------------
panel         <- as.data.table(my_data_with_spatial_g)   # county–year panel
wstate        <- as.data.table(w_i_state)                # i_fips, j_state, w_is
setnames(wstate, c("i_fips","j_state","w_is"),
         c("GEOID" , "event_state","exposure"))

events        <- as.data.table(policy_data)[start_year > 0,
                                            .(event_state = state,
                                              t0          = start_year)]

K_pre  <- 3    # 2010 → 2014 gap
K_post <- 2   # 2019 → 2020 gap

# ------------------------------------------------------------
# 2.  Build stacked event–study data
# ------------------------------------------------------------
stacked_es <- rbindlist(lapply(1:nrow(events), function(j){
  
  st  <- events$event_state[j]
  t0  <- events$t0[j]
  
  # copy relevant columns only (keeps memory low)
  tmp <- panel[, .(
    GEOID, year, state, death_rates_per_100_k,InvDist_exposure,
    population_density, ACS_PCT_AGE_18_44, ACS_PCT_AGE_45_64,
    prop_black, prop_asian, prop_other, prop_hispanic,
    ACS_MEDIAN_HH_INC, ACS_PCT_ENGL_NOT_WELL, percentage_uninsured,
    ACS_PCT_UNEMPLOY, ACS_PCT_LT_HS, Poor.mental.health.days.raw.value,
    ACS_TOT_POP_WT, state_year                   # FE string
  )]
  
  tmp[, `:=`(
    event_id  = st,                 # identifier for fixed effects
    rel_year  = year - t0           # event time
  )]
  
  tmp <- tmp[rel_year >= -K_pre & rel_year <= K_post]
  
  # attach *time-invariant* exposure w_i^(st)
  tmp <- merge(tmp,
               wstate[event_state == st, .(GEOID, exposure)],
               by = "GEOID", all.x = TRUE)
  tmp[is.na(exposure), exposure := 0]   # counties with no ties to st
  return(tmp)
}))

# --- after stacked_es is built ---------------------------------
stacked_es[, exposure := scale(exposure)[,1]]   # global z-score



# ------------------------------------------------------------
# 3.  Estimate Wilson-style specification
# ------------------------------------------------------------
covars <- c("population_density", "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64",
            "prop_black", "prop_asian", "prop_other", "prop_hispanic",
            "ACS_MEDIAN_HH_INC", "ACS_PCT_ENGL_NOT_WELL", "percentage_uninsured",
            "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS",
            "Poor.mental.health.days.raw.value")

fml <- death_rates_per_100_k ~ exposure:i(rel_year, ref = -1) + InvDist_exposure+
  population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
  prop_black + prop_asian + prop_other + prop_hispanic +
  ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
  ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
  Poor.mental.health.days.raw.value |
  GEOID^event_id + state_year^event_id

est_es <- feols(
  fml,
  data    = stacked_es,
  weights = ~ACS_TOT_POP_WT,
  cluster = ~state
)


table(stacked_es$rel_year, stacked_es$exposure > 0)   # TRUE if some exposure




# ------------------------------------------------------------
# 4.  Coefficient table for plotting
# ------------------------------------------------------------
coef_df <- broom::tidy(est_es) |>
  filter(str_detect(term, "^exposure:rel_year")) |>
  mutate(
    rel_year = as.integer(str_extract(term, "-?\\d+"))    # pull the ±k
  ) |>
  arrange(rel_year)                                       # chronological order


# ------------------------------------------------------------
# 5.  Illustrative plot 
# ------------------------------------------------------------
ggplot(coef_df, aes(x = rel_year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .25) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25) +
  geom_line(linewidth = .4) +
  geom_pointrange(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error),
                  size = .35) +
  labs(x = "Years relative to ERPO adoption",
       y = "Coefficient Estimate") +
  theme_minimal(base_size = 9)


### fml with only social exposure ####
fml_nodist <- death_rates_per_100_k ~ exposure:i(rel_year, ref = -1) +
  population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
  prop_black + prop_asian + prop_other + prop_hispanic +
  ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
  ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
  Poor.mental.health.days.raw.value |
  GEOID^event_id + state_year^event_id


est_es_nodist <- feols(
  fml_nodist,
  data    = stacked_es,
  weights = ~ACS_TOT_POP_WT,
  cluster = ~state
)

coef_df_nodist <- broom::tidy(est_es_nodist) |>
  filter(str_detect(term, "^exposure:rel_year")) |>
  mutate(rel_year = as.integer(str_extract(term, "-?\\d+"))) |>
  arrange(rel_year)

ggplot(coef_df_nodist, aes(x = rel_year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .25) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = .25) +
  geom_line(linewidth = .4) +
  geom_pointrange(aes(ymin = estimate - 1.96*std.error,
                      ymax = estimate + 1.96*std.error),
                  size = .35) +
  labs(x = "Years relative to ERPO adoption",
       y = "Coefficient Estimate (No Distance Control)") +
  theme_minimal(base_size = 9)

#### spatia contiguency ###

pad5 <- function(x) sprintf("%05d", as.integer(x))

################################################################################
##  1.  ERPO ADOPTION CALENDAR  (<= 2020)                                     ##
################################################################################
policy_data <- data.table(
  state      = c("CT","IN","CA","WA","OR","FL","VT","MD","RI","DE",
                 "MA","NJ","IL","NY","DC","CO","NV","HI","NM","VA"),
  adopt_year = c(1999,2005,2016,2016,2018,2018,2018,2018,2018,2018,
                 2018,2019,2019,2019,2019,2020,2020,2020,2020,2020)
)
erpo_set <- policy_data$state                    # convenience vector

################################################################################
##  2.  DROP COUNTIES BORDERING ERPO STATES (Queen contiguity)                ##
################################################################################
cnty_sf <- counties(cb = TRUE, year = 2020, class = "sf") |>
  st_transform(2163) |>
  mutate(GEOID = paste0(STATEFP, COUNTYFP),
         state = STATEFP)

# build cross-state edge list -------------------------------------------------
nb_raw <- poly2nb(cnty_sf, queen = TRUE)
edge_df <- map_dfr(seq_along(nb_raw), \(i) {
  if(length(nb_raw[[i]]) == 0) return(NULL)
  tibble(GEOID = cnty_sf$GEOID[i],
         nb_GEOID = cnty_sf$GEOID[ nb_raw[[i]] ])
}) |>
  mutate(state    = substr(GEOID,    1, 2),
         nb_state = substr(nb_GEOID, 1, 2)) |>
  filter(state != nb_state)

border_drop <- edge_df |>                      # non-ERPO county bordering ERPO state
  filter(nb_state %in% erpo_set & !(state %in% erpo_set)) |>
  distinct(GEOID) |> pull()

################################################################################
##  3.  TRIM PANEL & PREP META TABLE                                          ##
################################################################################
trimmed_df <- my_data_with_spatial_g |>
  st_drop_geometry() |>                         # drop sf geometry
  filter(!(GEOID %in% border_drop)) |>
  mutate(GEOID = pad5(GEOID)) |>
  as.data.table()

setkey(trimmed_df, GEOID, year)
meta <- trimmed_df[, .(GEOID, state)] |> unique()  # GEOID→state map

## 4·1 ── Load SCI and zero-pad FIPS ──────────────────────────────────────────
sci <- fread("C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv",
             col.names = c("i_fips","j_fips","SCI"))
sci[, `:=`(i_fips = pad5(i_fips),
           j_fips = pad5(j_fips))]

## 4·2 ── Attach origin & destination states (two joins, then rename) ────────
setDT(meta); setkey(meta, GEOID)           # ensure meta is keyed on GEOID

sci <- sci[meta, on = .(i_fips = GEOID), nomatch = 0]   # add origin state
setnames(sci, "state", "i_state")

sci <- sci[meta, on = .(j_fips = GEOID), nomatch = 0, allow.cartesian = TRUE]
setnames(sci, "state", "j_state")          # add destination state

## 4·3 ── Keep trimmed-sample origins & OUT-OF-STATE ties only ───────────────
orig_keep <- trimmed_df[, unique(GEOID)]
sci <- sci[i_fips %in% orig_keep & i_state != j_state]

## 4·4 ── Row-normalise SCI and collapse to state weights  w_is ──────────────
sci[, total := sum(SCI), by = i_fips]
sci[, w_ij  := fifelse(total > 0, SCI / total, 0)]
w_i_state <- sci[, .(w_is = sum(w_ij)), by = .(i_fips, j_state)]

## 4·5 ── Build state-year ERPO activation table ─────────────────────────────
years_panel <- trimmed_df[, sort(unique(year))]
state_year  <- CJ(state = meta[, unique(state)], year = years_panel)[
  policy_data, on = "state"][
    , ERPO_active := as.integer(!is.na(adopt_year) &
                                  year >= adopt_year)][
                                    , adopt_year := NULL]

## 4·6 ── Compute county-year exposure  E_it  ────────────────────────────────
expo <- w_i_state[state_year,
                  on = .(j_state = state), allow.cartesian = TRUE][
                    ERPO_active == 1L,
                    .(ERPO_exposure = sum(w_is)), by = .(i_fips, year)]

## 4·7 ── Merge exposure back into trimmed panel ─────────────────────────────
##############################################################################
## 4·7  MERGE EXPOSURE INTO TRIMMED PANEL  (robust, no duplicate names)     ##
##############################################################################
# 0.  ensure there is no legacy column that will collide --------------------
trimmed_df[, grep("^ERPO_exposure", names(trimmed_df), value = TRUE) := NULL]

# 1.  left-join: keep all rows from trimmed_df, bring in expo  --------------
trimmed_df <- merge(
  trimmed_df,
  expo[, .(GEOID = i_fips, year, ERPO_exposure)],   # rename join key once
  by = c("GEOID", "year"),
  all.x = TRUE                                     # left join
)

# 2.  replace remaining NAs with 0  -----------------------------------------
trimmed_df[is.na(ERPO_exposure), ERPO_exposure := 0]

# 3.  regenerate state–year fixed-effect factor  ----------------------------
trimmed_df[, state_year := interaction(state, year, drop = TRUE)]

trimmed_df[, ERPO_exposure_z :=
             (ERPO_exposure - mean(ERPO_exposure)) / sd(ERPO_exposure)]


################################################################################
##  5.  POOLED SPILLOVER MODEL                                                ##
################################################################################
mod_spill <- felm(
  death_rates_per_100_k ~ ERPO_exposure_z +                       # ← scaled
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + Poor.mental.health.days.raw.value |
    GEOID + state_year | 0 | GEOID,
  data    = trimmed_df,
  weights = trimmed_df$ACS_TOT_POP_WT)
summary(mod_spill)

## ---- 1.  Inputs -----------------------------------------------------------
panel   <- as.data.table(my_data_with_spatial_g)                 # county-year panel
wstate  <- as.data.table(w_i_state)                              # i_fips, j_state, w_is
setnames(wstate, c("i_fips","j_state","w_is"),
         c("GEOID" , "event_state","exposure"))

events  <- data.table(event_state = policy_data$state,
                      t0          = policy_data$adopt_year)

K_pre  <- 3                              # leads  −3, −2, −1
K_post <- 2                             # lags    0,  +1
event_window <- -K_pre:K_post            # -3 … 1

## ---- 2.  Build stacked event-study data -----------------------------------
stacked_es <- rbindlist(lapply(1:nrow(events), function(j){
  st <- events$event_state[j];  t0 <- events$t0[j]
  
  tmp <- panel[year %between% c(t0-K_pre, t0+K_post), .(
    GEOID, year, state, death_rates_per_100_k, InvDist_exposure,
    population_density, ACS_PCT_AGE_18_44, ACS_PCT_AGE_45_64,
    prop_black, prop_asian, prop_other, prop_hispanic,
    ACS_MEDIAN_HH_INC, ACS_PCT_ENGL_NOT_WELL, percentage_uninsured,
    ACS_PCT_UNEMPLOY, ACS_PCT_LT_HS, Poor.mental.health.days.raw.value,
    ACS_TOT_POP_WT, state_year)]
  tmp[, `:=`(event_id = st,
             rel_year = year - t0)]
  tmp <- merge(tmp,
               wstate[event_state == st, .(GEOID, exposure)],
               by = "GEOID", all.x = TRUE)
  tmp[is.na(exposure), exposure := 0]
  tmp[]
}))

## ---- 3.  Standardise exposure (global z-score) ----------------------------
stacked_es[, exposure := as.numeric(scale(exposure))]

## ---- 4.  Wilson specification --------------------------------------------
fml <- death_rates_per_100_k ~ exposure:i(rel_year, ref = -1) + InvDist_exposure +
  population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
  prop_black + prop_asian + prop_other + prop_hispanic +
  ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
  ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + Poor.mental.health.days.raw.value |
  GEOID^event_id + state_year^event_id

est_es <- feols(
  fml,
  data    = stacked_es,
  weights = ~ACS_TOT_POP_WT,
  cluster = ~state) 

## ---- 5.  Extract coefficients --------------------------------------------
coef_df <- tidy(est_es, conf.int = TRUE) |>
  filter(str_detect(term, "^exposure:rel_year")) |>
  mutate(rel_year = as.integer(str_extract(term, "-?\\d+"))) |>
  arrange(rel_year)

## ---- 6.  Plot -------------------------------------------------------------
ggplot(coef_df, aes(rel_year, estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .25) +
  geom_vline(xintercept = 0,  linetype = "dotted",  linewidth = .25) +
  geom_line(linewidth = .4) +
  geom_pointrange(aes(ymin = conf.low, ymax = conf.high), size = .35) +
  scale_x_continuous(breaks = event_window[event_window != -1]) +
  labs(x = "Years relative to ERPO adoption",
       y = "Δ deaths per 100 k (per 1-SD ↑ in exposure)") +
  theme_minimal(base_size = 9)



###############################################################################
##  CI plot: baseline vs. border-trimmed model                               ##
###############################################################################
library(broom); library(dplyr); library(ggplot2)

## 1 · Extract point estimate & 95 % CI for the ERPO coefficient -------------
coef_df <- bind_rows(
  tidy(did_spill_social, conf.int = TRUE) %>%                # baseline sample
    filter(term == "ERPO_exposure") %>%
    mutate(model = "Full sample"),
  
  tidy(mod_spill,       conf.int = TRUE) %>%                 # trimmed sample
    filter(term %in% c("ERPO_exposure", "ERPO_exposure_z")) %>%  # name depends on scaling
    mutate(model = "Border-trimmed")
)

## 2 · Plot -------------------------------------------------------------------
ggplot(coef_df,
       aes(x = estimate, y = model, colour = model)) +
  geom_point(size = 3) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.15, linewidth = 1) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  scale_colour_manual(values = c("Full sample"      = "darkred",
                                 "Border-trimmed"   = "steelblue")) +
  labs(x = "Deaths per 100 000 (per 1-SD ↑ exposure)", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position    = "none",
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


### stagrgrazer #####


### updated variables ####

trimmed_df[, ERPO_exposure :=
             (ERPO_exposure - mean(ERPO_exposure)) / sd(ERPO_exposure)]


################################################################################
##  5.  POOLED SPILLOVER MODEL                                                ##
################################################################################
mod_spill <- felm(
  death_rates_per_100_k ~ ERPO_exposure +                       # ← scaled
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + Poor.mental.health.days.raw.value |
    GEOID + state_year | 0 | GEOID,
  data    = trimmed_df,
  weights = trimmed_df$ACS_TOT_POP_WT)
summary(mod_spill)






stargazer(
  did_no_spill, 
  did_spill_social, 
  mod_spill,
  type = "latex",
  title = "Effect of Policy Exposure on Death Rates",
  column.labels = c("Direct Effect", "Indirect Social Network Exposure", "Robust Indirect Social Network Exposure"),
  dep.var.labels = "Deaths per 100K",
  covariate.labels = c(
    "ERPO",                       # from did_no_spill
    "ERPO Social Exposure",       # ERPO_exposure in did_spill_social
    "Population Density",
    "Percentage Age 18-44", 
    "Percentage Age 45-64",
    "Proportion Black",
    "Proportion Asian",
    "Proportion Other",
    "Proportion Hispanic",
    "Median Household Income",
    "Percentage Population who do not speak English that well",
    "Percentage Population who are uninsured",
    "Percentage Population who are unemployed",
    "Percentage Population with Less Than High School Education",
    "Poor Mental Health Days"
  )
  ,
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "output_table.tex"  # Save the output directly to a .tex file
)

### direct effect estimation ###

# estimate group-time average treatment effects without covariates
my_data_with_spatial_g$GEOID <- as.numeric(my_data_with_spatial_g$GEOID)
my_data_with_spatial_g$year <- as.numeric(my_data_with_spatial_g$year)


mw.attgt <- att_gt(yname = "death_rates_per_100_k",
                   gname = "start_year",
                   idname = "GEOID",
                   tname = "year",
                   xformla = ~ population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
                     prop_black + prop_asian + prop_other + prop_hispanic +
                     ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
                     ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + Poor.mental.health.days.raw.value,
                   data = my_data_with_spatial_g,
                   weightsname    = "ACS_TOT_POP_WT",
                   control_group = "notyettreated",
                   est_method = "reg",
                   clustervars   = "state"
)

# summarize the results
summary(mw.attgt)


mw.dyn.balance <- aggte(
  mw.attgt,
  type   = "dynamic",
  min_e  = -3,    # three leads (years before treatment)
  max_e  =  2,    # four lags (years after treatment)
  na.rm  = TRUE
)


summary(mw.dyn.balance)

ggdid(mw.dyn.balance)
