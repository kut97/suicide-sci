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

# 1) Read in all files named sdoh_####.csv
files <- list.files(
  path = data_path, 
  pattern = "^sdoh_\\d{4}\\.csv$",  # Match files like sdoh_2010.csv
  full.names = TRUE
)

# Iterate over files and process them
for (f in files) {
  # Extract year from the filename, e.g., "sdoh_2010.csv" -> "2010"
  year <- sub(".*sdoh_(\\d{4}).*", "\\1", basename(f))  # Corrected substitution pattern
  
  # Read the CSV while ensuring COUNTYFIPS is read as a character column
  tmp <- read.csv(f, colClasses = c(COUNTYFIPS = "character"))
  
  # Create race_cat column
  if (all(c("ACS_PCT_WHITE", "ACS_PCT_BLACK", "ACS_PCT_WHITE_NONHISP", "ACS_PCT_BLACK_NONHISP", 
            "ACS_PCT_HISPANIC", "ACS_PCT_ASIAN_NONHISP",
            "ACS_PCT_AIAN_NONHISP", "ACS_PCT_NHPI_NONHISP",
            "ACS_PCT_MULT_RACE_NONHISP") %in% names(tmp))) {
    
    group_names <- c("White", "Black", "WhiteNH", "BlackNH", "Hispanic", "AsianNH", "AIANNH", "NHPINH", "MultiNH")
    
    tmp$race_cat <- apply(
      tmp[, c("ACS_PCT_WHITE", "ACS_PCT_BLACK",  "ACS_PCT_WHITE_NONHISP", "ACS_PCT_BLACK_NONHISP", 
              "ACS_PCT_HISPANIC", "ACS_PCT_ASIAN_NONHISP",
              "ACS_PCT_AIAN_NONHISP", "ACS_PCT_NHPI_NONHISP",
              "ACS_PCT_MULT_RACE_NONHISP")],
      1,
      function(x) group_names[which.max(x)]
    )
    
    tmp$race_cat[tmp$race_cat %in% c("WhiteNH", "BlackNH", "Hispanic","AsianNH", "AIANNH", "NHPINH", "MultiNH")] <- "Other"
    tmp$race_cat <- factor(tmp$race_cat, levels = c("White", "Black", "Other"))
  } else {
    tmp$race_cat <- NA
  }
  
  # Store the processed data in an environment variable
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
      "COUNTYFIPS", "YEAR", "ACS_TOT_POP_WT", "race_cat", 
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



# ### spatial proximity ####
# calculateSpatialProximity_by_year <- function(all_data) {
#   # Check for missing data
#   if (any(is.na(all_data$GEOID) | is.na(all_data$Longitude) | is.na(all_data$Latitude))) {
#     stop("Missing GEOID, Longitude, or Latitude in the data. Please clean the data.")
#   }
#   
#   # Extract unique counties for the distance matrix
#   unique_counties <- all_data %>%
#     distinct(GEOID, Longitude, Latitude) %>%
#     arrange(GEOID)
#   
#   # Compute geodesic distance in km
#   dist_km <- geodist::geodist(unique_counties[, c("Longitude", "Latitude")],
#                               measure = "geodesic") / 1000
#   
#   # Implement a_ij = 1 + 1/d_ij, for i != j
#   # (Note: if d_ij = 0 for i == j, then we set the diagonal to 0 below)
#   distance_matrix <- 1 + 1 / dist_km
#   
#   # Zero out the diagonal (no self-distance)
#   diag(distance_matrix) <- 0
#   
#   # Normalize each row so rows sum to 1
#   row_sums <- rowSums(distance_matrix)
#   if (any(row_sums == 0)) {
#     warning("Some rows in the distance matrix sum to zero. Check the input data.")
#     row_sums[row_sums == 0] <- 1  # Avoid division by zero
#   }
#   A_ij <- sweep(distance_matrix, 1, row_sums, FUN = "/")
#   rownames(A_ij) <- unique_counties$GEOID
#   
#   # Initialize d_minus_i
#   all_data$d_minus_i <- NA_real_
#   
#   # Loop over each unique year and compute d_minus_i
#   for (yr in unique(all_data$year)) {
#     # Subset data for this year
#     sub_data <- all_data %>% filter(year == yr)
#     
#     # Check for missing death rates
#     if (any(is.na(sub_data$death_rates_per_100_k))) {
#       warning(paste("Missing death_rates_per_100_k for year", yr, ". Skipping computation for this year."))
#       next
#     }
#     
#     # Align subset with A_ij
#     idx <- match(sub_data$GEOID, rownames(A_ij))
#     if (any(is.na(idx))) {
#       warning(paste("Some GEOID values for year", yr, "are missing in the distance matrix."))
#       next
#     }
#     
#     # Submatrix for the year
#     A_ij_sub <- A_ij[idx, idx, drop = FALSE]
#     
#     # Check dimensions
#     y <- sub_data$death_rates_per_100_k
#     if (nrow(A_ij_sub) != length(y)) {
#       stop(paste("Dimension mismatch for year", yr, ": A_ij_sub rows =", nrow(A_ij_sub),
#                  "and y length =", length(y)))
#     }
#     
#     # d_minus_i = A_ij_sub %*% y
#     d_minus_i_vec <- A_ij_sub %*% as.matrix(y)
#     
#     # Store in the original data frame
#     all_data$d_minus_i[which(all_data$year == yr)] <- d_minus_i_vec
#   }
#   
#   return(all_data)
# }
# 
# my_data_with_spatial <- calculateSpatialProximity_by_year(all_data)

### using gravity weights ###


# Extract unique counties for the distance matrix
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



# calculateSpatialProximity_by_year <- function(all_data, alpha = 1) {
#   # 1) Identify unique counties and build one distance matrix
#   #    We assume 'GEOID', 'Longitude', and 'Latitude' are present in 'all_data'.
#   
#   # Extract unique counties for the distance matrix
#   unique_counties <- all_data %>%
#     dplyr::distinct(GEOID, Longitude, Latitude) %>%
#     dplyr::arrange(GEOID)
#   
#   # Compute geodesic distance in kilometers
#   distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")],
#                                   measure = "geodesic") / 1000
#   
#   # 2) Gravity-weighted function: w(d_{ij}) = 1 + 1/(d_{ij}^alpha)
#   #    If distance is zero (for the diagonal or otherwise), we handle it separately below.
#   #    We'll first transform only off-diagonal entries.
#   
#   # Initialize a matrix of the same size
#   gravity_matrix <- matrix(0, nrow = nrow(distance_km), ncol = ncol(distance_km))
#   
#   # Fill off-diagonal elements with 1 + 1/d^alpha
#   # Avoid dividing by zero on the diagonal by skipping those indices
#   for (i in seq_len(nrow(distance_km))) {
#     for (j in seq_len(ncol(distance_km))) {
#       if (i != j) {
#         # distance_km[i,j] is > 0 for distinct counties
#         gravity_matrix[i,j] <- 1 + 1 / (distance_km[i,j]^alpha)
#       }
#     }
#   }
#   
#   # 3) Zero out diagonal (no self-distance weighting)
#   diag(gravity_matrix) <- 0
#   
#   # Optionally assign row/column names
#   rownames(gravity_matrix) <- unique_counties$GEOID
#   colnames(gravity_matrix) <- unique_counties$GEOID
#   
#   # 4) Normalize each row to create A_{i,j}
#   row_sums <- rowSums(gravity_matrix)
#   A_ij <- sweep(gravity_matrix, 1, row_sums, FUN = "/")
#   diag(A_ij) <- 0
#   
#   # 5) Prepare to store d_minus_i in the original data
#   all_data$d_minus_i <- NA_real_
#   
#   # 6) Loop over each unique year and compute d_minus_i
#   for (yr in unique(all_data$year)) {
#     
#     # Subset data for this year
#     sub_data <- all_data %>% dplyr::filter(year == yr)
#     
#     # Let 'death_rates_per_100_k' be the outcome
#     # (Adjust the column name if yours differs.)
#     y <- sub_data$death_rates_per_100_k
#     
#     # Align the subset with rows/columns of A_ij
#     idx <- match(sub_data$GEOID, rownames(A_ij))
#     
#     # Create a submatrix for these counties
#     A_ij_sub <- A_ij[idx, idx, drop = FALSE]
#     
#     # Multiply A_ij_sub by the outcome vector 'y'
#     # Column j in A_ij_sub aligns with sub_data$GEOID[j]
#     d_minus_i_vec <- A_ij_sub %*% as.matrix(y)
#     
#     # Insert into all_data$d_minus_i for these counties, this year
#     all_data$d_minus_i[all_data$year == yr] <- d_minus_i_vec
#   }
#   
#   return(all_data)
# }
# 
# 
# my_data_with_spatial_g <- calculateSpatialProximity_by_year(
#   all_data = my_data_with_spatial,
#   alpha = 0.1
# )

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


### standardized covariates #####

# List of covariates to standardize
covariates <- c(
  "death_rates_per_100_k", "s_minus_i", "d_minus_i", 
  "ACS_MEDIAN_HH_INC", "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS", 
  "population_density", "Poor.mental.health.days.raw.value", 
  "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64", "ACS_PCT_ENGL_NOT_WELL", 
  "percentage_uninsured"
)


# min max standardization
# Define Min-Max scaling function
min_max_scaled <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

# Apply Min-Max scaling after Z-score standardization
my_data_with_spatial_g[covariates] <- as.data.frame(lapply(my_data_with_spatial_g[covariates], min_max_scaled))

### making the correlation plot ###

library(ggplot2)
library(GGally)


# Select the variables from the dataset
variables <- c("death_rates_per_100_k", "s_minus_i", "d_minus_i")

# Create a new data frame with only the selected variables
df_selected <- my_data_with_spatial_g[variables]

# Create the conditional scatterplot matrix
ggpairs(df_selected,
        lower = list(continuous = wrap("points", alpha = 0.8, color="red")),
        diag = list(continuous = wrap("barDiag", fill="red", bins=10)),
        upper = list(continuous = wrap("smooth", method = "lm", color="red", fullrange = TRUE)))+
  theme_minimal()




### all years ###
library(lfe)
model_felm_entire_united_states<- felm(death_rates_per_100_k~ s_minus_i+ d_minus_i+ACS_MEDIAN_HH_INC+
                                         ACS_PCT_AGE_18_44+ACS_PCT_AGE_45_64+ACS_PCT_ENGL_NOT_WELL+percentage_uninsured+
                                          race_cat+ACS_PCT_UNEMPLOY+ACS_PCT_LT_HS+population_density+Poor.mental.health.days.raw.value
                                     |GEOID+year,data=my_data_with_spatial_g,weights =my_data_with_spatial_g$ACS_TOT_POP_WT)
summary(model_felm_entire_united_states,cluster = my_data_with_spatial_g[, c("state", "year")])


# Extract residuals
res <- residuals(model_felm_entire_united_states)

# Q–Q plot
qqnorm(res)
qqline(res)

# Histogram
hist(res, breaks = 30, main="Residuals", xlab="")

# Shapiro-Wilk test for normality
shapiro.test(res)



### writing the csv ####
write.csv(my_data_with_spatial_g, 'spatial_social_proximity_suicide_mortality_2010_2020_cdc_wonder_data_gravity_weights.csv')

lw_1_social <- mat2listw(w_i_j, style='W')
lw_2_spatial <- mat2listw(A_ij, style='W')


saveRDS(lw_1_social, file="lw_1_entire_us.rds")
saveRDS(lw_2_spatial, file="lw_1_entirelw_2_spatial_us.rds")

write.csv(w_i_j,'w_i_j_social_weights.csv')
write.csv(A_ij, 'a_i_j_spatial_weights.csv')
