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

# Define the years for interpolation
years <- 1999:2010

# ---------------------------------------------
# 1. Retrieve Data for the Year 2000
# ---------------------------------------------

# a. Income Data (Aggregate household income in 1999)
income_2000 <- get_decennial(
  geography = "county",
  variables = "P152A001",
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, income_2000 = value)

# b. Poverty Data (Total households and households below poverty level)
poverty_2000 <- get_decennial(
  geography = "county",
  variables = c(total = "P092001", below_poverty = "P092002"),
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, variable, value) %>%
  spread(key = variable, value = value) %>%
  rename(
    total_2000 = total,
    below_poverty_2000 = below_poverty
  )

# c. School Enrollment and Unemployment Data
school_unemployed_2000 <- get_decennial(
  geography = "county",
  variables = c(total = "P038001", unemployed = "P038012"),
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, variable, value) %>%
  spread(key = variable, value = value) %>%
  rename(
    total_2000 = total,
    unemployed_2000 = unemployed
  )

# d. Race of Householder Data
# Total Households
total_households_2000 <- get_decennial(
  geography = "county",
  variables = "H007001",
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, total_households_2000 = value)

# White Householders
white_households_2000 <- get_decennial(
  geography = "county",
  variables = "H007002",
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, white_households_2000 = value)

# Black Householders
black_households_2000 <- get_decennial(
  geography = "county",
  variables = "H007003",
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, black_households_2000 = value)

# Asian Householders
asian_households_2000 <- get_decennial(
  geography = "county",
  variables = "H007005",
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, asian_households_2000 = value)

# Hispanic Householders
hispanic_households_2000 <- get_decennial(
  geography = "county",
  variables = "H011001",
  year = 2000,
  survey = "sf3"
) %>%
  select(GEOID, NAME, hispanic_households_2000 = value)

# Combine Race Data for 2000
race_householders_2000 <- total_households_2000 %>%
  left_join(white_households_2000, by = c("GEOID", "NAME")) %>%
  left_join(black_households_2000, by = c("GEOID", "NAME")) %>%
  left_join(asian_households_2000, by = c("GEOID", "NAME")) %>%
  left_join(hispanic_households_2000, by = c("GEOID", "NAME"))

# ---------------------------------------------
# 2. Retrieve Data for the Year 2010
# ---------------------------------------------

# a. Income Data
income_2010 <- get_acs(
  geography = "county",
  variables = "B19025_001",
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, income_2010 = estimate)

# b. Poverty Data
poverty_2010 <- get_acs(
  geography = "county",
  variables = c(total = "B17001_001", below_poverty = "B17001_002"),
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  rename(
    total_2010 = total,
    below_poverty_2010 = below_poverty
  )

# c. School Enrollment and Unemployment Data
school_unemployed_2010 <- get_acs(
  geography = "county",
  variables = c(total = "B14005_001", unemployed = "B14005_012"),
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, variable, estimate) %>%
  spread(key = variable, value = estimate) %>%
  rename(
    total_2010 = total,
    unemployed_2010 = unemployed
  )

# d. Race of Householder Data
# Total Households
total_households_2010 <- get_acs(
  geography = "county",
  variables = "B11001_001",
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, total_households_2010 = estimate)

# White Householders
white_households_2010 <- get_acs(
  geography = "county",
  variables = "B11001A_001",
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, white_households_2010 = estimate)

# Black Householders
black_households_2010 <- get_acs(
  geography = "county",
  variables = "B11001B_001",
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, black_households_2010 = estimate)

# Asian Householders
asian_households_2010 <- get_acs(
  geography = "county",
  variables = "B11001D_001",
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, asian_households_2010 = estimate)

# Hispanic Householders
hispanic_households_2010 <- get_acs(
  geography = "county",
  variables = "B11001I_001",
  year = 2010,
  survey = "acs5"
) %>%
  select(GEOID, NAME, hispanic_households_2010 = estimate)

# Combine Race Data for 2010
race_householders_2010 <- total_households_2010 %>%
  left_join(white_households_2010, by = c("GEOID", "NAME")) %>%
  left_join(black_households_2010, by = c("GEOID", "NAME")) %>%
  left_join(asian_households_2010, by = c("GEOID", "NAME")) %>%
  left_join(hispanic_households_2010, by = c("GEOID", "NAME"))

# ---------------------------------------------
# 3. Merge 2000 and 2010 Data
# ---------------------------------------------

# a. Income Data
income_combined <- full_join(income_2000, income_2010, by = c("GEOID", "NAME"))

# b. Poverty Data
poverty_combined <- full_join(poverty_2000, poverty_2010, by = c("GEOID", "NAME"))

# c. School Enrollment and Unemployment Data
school_unemployed_combined <- full_join(
  school_unemployed_2000,
  school_unemployed_2010,
  by = c("GEOID", "NAME")
)

# d. Race of Householder Data
race_householders_combined <- full_join(
  race_householders_2000,
  race_householders_2010,
  by = c("GEOID", "NAME")
)

# ---------------------------------------------
# 4. Calculate Proportions and Rates
# ---------------------------------------------

# a. Poverty Rates
poverty_combined <- poverty_combined %>%
  mutate(
    total_2000 = ifelse(total_2000 == 0, NA, total_2000),
    total_2010 = ifelse(total_2010 == 0, NA, total_2010),
    poverty_rate_2000 = below_poverty_2000 / total_2000,
    poverty_rate_2010 = below_poverty_2010 / total_2010
  )

# b. Unemployment Rates for School Enrollment
school_unemployed_combined <- school_unemployed_combined %>%
  mutate(
    total_2000 = ifelse(total_2000 == 0, NA, total_2000),
    total_2010 = ifelse(total_2010 == 0, NA, total_2010),
    unemployment_rate_2000 = unemployed_2000 / total_2000,
    unemployment_rate_2010 = unemployed_2010 / total_2010
  )

# c. Race Proportions
race_householders_combined <- race_householders_combined %>%
  mutate(
    total_households_2000 = ifelse(total_households_2000 == 0, NA, total_households_2000),
    total_households_2010 = ifelse(total_households_2010 == 0, NA, total_households_2010),
    white_prop_2000 = white_households_2000 / total_households_2000,
    black_prop_2000 = black_households_2000 / total_households_2000,
    asian_prop_2000 = asian_households_2000 / total_households_2000,
    hispanic_prop_2000 = hispanic_households_2000 / total_households_2000,
    white_prop_2010 = white_households_2010 / total_households_2010,
    black_prop_2010 = black_households_2010 / total_households_2010,
    asian_prop_2010 = asian_households_2010 / total_households_2010,
    hispanic_prop_2010 = hispanic_households_2010 / total_households_2010
  )

# ---------------------------------------------
# 5. Handle Missing Data by Imputing Mean Values
# ---------------------------------------------

# a. Income Data
# Calculate mean incomes excluding NAs
mean_income_2000 <- mean(income_combined$income_2000, na.rm = TRUE)
mean_income_2010 <- mean(income_combined$income_2010, na.rm = TRUE)

# Impute missing values
income_combined <- income_combined %>%
  mutate(
    income_2000 = ifelse(is.na(income_2000), mean_income_2000, income_2000),
    income_2010 = ifelse(is.na(income_2010), mean_income_2010, income_2010)
  )

# b. Poverty Rates
# Calculate mean poverty rates excluding NAs
mean_poverty_rate_2000 <- mean(poverty_combined$poverty_rate_2000, na.rm = TRUE)
mean_poverty_rate_2010 <- mean(poverty_combined$poverty_rate_2010, na.rm = TRUE)

# Impute missing values
poverty_combined <- poverty_combined %>%
  mutate(
    poverty_rate_2000 = ifelse(is.na(poverty_rate_2000), mean_poverty_rate_2000, poverty_rate_2000),
    poverty_rate_2010 = ifelse(is.na(poverty_rate_2010), mean_poverty_rate_2010, poverty_rate_2010)
  )

# c. Unemployment Rates
# Calculate mean unemployment rates excluding NAs
mean_unemployment_rate_2000 <- mean(school_unemployed_combined$unemployment_rate_2000, na.rm = TRUE)
mean_unemployment_rate_2010 <- mean(school_unemployed_combined$unemployment_rate_2010, na.rm = TRUE)

# Impute missing values
school_unemployed_combined <- school_unemployed_combined %>%
  mutate(
    unemployment_rate_2000 = ifelse(is.na(unemployment_rate_2000), mean_unemployment_rate_2000, unemployment_rate_2000),
    unemployment_rate_2010 = ifelse(is.na(unemployment_rate_2010), mean_unemployment_rate_2010, unemployment_rate_2010)
  )

# d. Race Proportions
# Calculate mean proportions excluding NAs
mean_white_prop_2000 <- mean(race_householders_combined$white_prop_2000, na.rm = TRUE)
mean_white_prop_2010 <- mean(race_householders_combined$white_prop_2010, na.rm = TRUE)
mean_black_prop_2000 <- mean(race_householders_combined$black_prop_2000, na.rm = TRUE)
mean_black_prop_2010 <- mean(race_householders_combined$black_prop_2010, na.rm = TRUE)
mean_asian_prop_2000 <- mean(race_householders_combined$asian_prop_2000, na.rm = TRUE)
mean_asian_prop_2010 <- mean(race_householders_combined$asian_prop_2010, na.rm = TRUE)
mean_hispanic_prop_2000 <- mean(race_householders_combined$hispanic_prop_2000, na.rm = TRUE)
mean_hispanic_prop_2010 <- mean(race_householders_combined$hispanic_prop_2010, na.rm = TRUE)

# Impute missing values
race_householders_combined <- race_householders_combined %>%
  mutate(
    white_prop_2000 = ifelse(is.na(white_prop_2000), mean_white_prop_2000, white_prop_2000),
    white_prop_2010 = ifelse(is.na(white_prop_2010), mean_white_prop_2010, white_prop_2010),
    black_prop_2000 = ifelse(is.na(black_prop_2000), mean_black_prop_2000, black_prop_2000),
    black_prop_2010 = ifelse(is.na(black_prop_2010), mean_black_prop_2010, black_prop_2010),
    asian_prop_2000 = ifelse(is.na(asian_prop_2000), mean_asian_prop_2000, asian_prop_2000),
    asian_prop_2010 = ifelse(is.na(asian_prop_2010), mean_asian_prop_2010, asian_prop_2010),
    hispanic_prop_2000 = ifelse(is.na(hispanic_prop_2000), mean_hispanic_prop_2000, hispanic_prop_2000),
    hispanic_prop_2010 = ifelse(is.na(hispanic_prop_2010), mean_hispanic_prop_2010, hispanic_prop_2010)
  )

# ---------------------------------------------
# 6. Interpolate Values for 1999-2010
# ---------------------------------------------

# a. Income Interpolation
income_interpolated <- income_combined %>%
  rowwise() %>%
  do(data.frame(
    GEOID = .$GEOID,
    NAME = .$NAME,
    year = years,
    income = approx(
      x = c(2000, 2010),
      y = c(.$income_2000, .$income_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y
  ))

# b. Poverty Rate Interpolation
poverty_interpolated <- poverty_combined %>%
  rowwise() %>%
  do(data.frame(
    GEOID = .$GEOID,
    NAME = .$NAME,
    year = years,
    poverty_rate = approx(
      x = c(2000, 2010),
      y = c(.$poverty_rate_2000, .$poverty_rate_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y
  ))

# c. Unemployment Rate Interpolation
school_unemployed_interpolated <- school_unemployed_combined %>%
  rowwise() %>%
  do(data.frame(
    GEOID = .$GEOID,
    NAME = .$NAME,
    year = years,
    unemployment_rate = approx(
      x = c(2000, 2010),
      y = c(.$unemployment_rate_2000, .$unemployment_rate_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y
  ))

# d. Race Proportion Interpolation
race_interpolated <- race_householders_combined %>%
  rowwise() %>%
  do(data.frame(
    GEOID = .$GEOID,
    NAME = .$NAME,
    year = years,
    white_prop = approx(
      x = c(2000, 2010),
      y = c(.$white_prop_2000, .$white_prop_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y,
    black_prop = approx(
      x = c(2000, 2010),
      y = c(.$black_prop_2000, .$black_prop_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y,
    asian_prop = approx(
      x = c(2000, 2010),
      y = c(.$asian_prop_2000, .$asian_prop_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y,
    hispanic_prop = approx(
      x = c(2000, 2010),
      y = c(.$hispanic_prop_2000, .$hispanic_prop_2010),
      xout = years,
      rule = 2  # Allow extrapolation
    )$y
  ))

# ---------------------------------------------
# 7. Combine All Interpolated Data
# ---------------------------------------------

# Merge datasets by GEOID, NAME, and year
final_data <- reduce(
  list(
    income_interpolated,
    poverty_interpolated,
    school_unemployed_interpolated,
    race_interpolated
  ),
  full_join,
  by = c("GEOID", "NAME", "year")
)

# ---------------------------------------------
# 8. Verify and Clean the Final Dataset
# ---------------------------------------------

# Remove any remaining rows with missing GEOID or NAME
final_data <- final_data %>%
  filter(!is.na(GEOID) & !is.na(NAME))

# View the first few rows of the final dataset
head(final_data)

years_2010_2020 <- 2010:2020

# ---------------------------------------------
# 1. Retrieve Data for the Years 2010-2020
# ---------------------------------------------

# Define a function to retrieve data for a given year
get_annual_data <- function(year) {
  cat("Retrieving data for year:", year, "\n")
  
  # a. Income Data
  income <- get_acs(
    geography = "county",
    variables = "B19025_001",
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, income = estimate) %>%
    mutate(year = year)
  
  # b. Poverty Data
  poverty <- get_acs(
    geography = "county",
    variables = c(total = "B17001_001", below_poverty = "B17001_002"),
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    spread(key = variable, value = estimate) %>%
    rename(
      total = total,
      below_poverty = below_poverty
    ) %>%
    mutate(
      poverty_rate = below_poverty / total,
      year = year
    ) %>%
    select(GEOID, NAME, poverty_rate, year)
  
  # c. School Enrollment and Unemployment Data
  school_unemployed <- get_acs(
    geography = "county",
    variables = c(total = "B14005_001", unemployed = "B14005_012"),
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, variable, estimate) %>%
    spread(key = variable, value = estimate) %>%
    rename(
      total = total,
      unemployed = unemployed
    ) %>%
    mutate(
      unemployment_rate = unemployed / total,
      year = year
    ) %>%
    select(GEOID, NAME, unemployment_rate, year)
  
  # d. Race of Householder Data
  # Total Households
  total_households <- get_acs(
    geography = "county",
    variables = "B11001_001",
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, total_households = estimate)
  
  # White Householders
  white_households <- get_acs(
    geography = "county",
    variables = "B11001A_001",
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, white_households = estimate)
  
  # Black Householders
  black_households <- get_acs(
    geography = "county",
    variables = "B11001B_001",
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, black_households = estimate)
  
  # Asian Householders
  asian_households <- get_acs(
    geography = "county",
    variables = "B11001D_001",
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, asian_households = estimate)
  
  # Hispanic Householders
  hispanic_households <- get_acs(
    geography = "county",
    variables = "B11001I_001",
    year = year,
    survey = "acs5"
  ) %>%
    select(GEOID, NAME, hispanic_households = estimate)
  
  # Combine Race Data
  race_data <- total_households %>%
    left_join(white_households, by = c("GEOID", "NAME")) %>%
    left_join(black_households, by = c("GEOID", "NAME")) %>%
    left_join(asian_households, by = c("GEOID", "NAME")) %>%
    left_join(hispanic_households, by = c("GEOID", "NAME")) %>%
    mutate(
      white_prop = white_households / total_households,
      black_prop = black_households / total_households,
      asian_prop = asian_households / total_households,
      hispanic_prop = hispanic_households / total_households,
      year = year
    ) %>%
    select(GEOID, NAME, white_prop, black_prop, asian_prop, hispanic_prop, year)
  
  # Combine all data for the year
  annual_data <- income %>%
    left_join(poverty, by = c("GEOID", "NAME", "year")) %>%
    left_join(school_unemployed, by = c("GEOID", "NAME", "year")) %>%
    left_join(race_data, by = c("GEOID", "NAME", "year"))
  
  return(annual_data)
}

# Retrieve data for each year from 2010 to 2020
annual_data_list <- lapply(2010:2020, get_annual_data)

# Combine annual data into one dataframe
annual_data_2010_2020 <- bind_rows(annual_data_list)

# ---------------------------------------------
# 2. Handle Missing Data by Imputing Mean Values
# ---------------------------------------------

# Calculate mean values for each variable (excluding NAs)
mean_values <- annual_data_2010_2020 %>%
  summarise(
    income_mean = mean(income, na.rm = TRUE),
    poverty_rate_mean = mean(poverty_rate, na.rm = TRUE),
    unemployment_rate_mean = mean(unemployment_rate, na.rm = TRUE),
    white_prop_mean = mean(white_prop, na.rm = TRUE),
    black_prop_mean = mean(black_prop, na.rm = TRUE),
    asian_prop_mean = mean(asian_prop, na.rm = TRUE),
    hispanic_prop_mean = mean(hispanic_prop, na.rm = TRUE)
  )

# Impute missing values with mean values
annual_data_2010_2020 <- annual_data_2010_2020 %>%
  mutate(
    income = ifelse(is.na(income), mean_values$income_mean, income),
    poverty_rate = ifelse(is.na(poverty_rate), mean_values$poverty_rate_mean, poverty_rate),
    unemployment_rate = ifelse(is.na(unemployment_rate), mean_values$unemployment_rate_mean, unemployment_rate),
    white_prop = ifelse(is.na(white_prop), mean_values$white_prop_mean, white_prop),
    black_prop = ifelse(is.na(black_prop), mean_values$black_prop_mean, black_prop),
    asian_prop = ifelse(is.na(asian_prop), mean_values$asian_prop_mean, asian_prop),
    hispanic_prop = ifelse(is.na(hispanic_prop), mean_values$hispanic_prop_mean, hispanic_prop)
  )

# ---------------------------------------------
# 3. Combine with Interpolated Data from 1999-2010
# ---------------------------------------------

# Use the interpolated data from previous steps (assuming it's stored in 'final_data')
# If you don't have 'final_data' loaded, make sure to run the previous code to generate it.

# Remove the year 2010 from 'final_data' to avoid duplication
final_data_1999_2009 <- final_data %>% filter(year < 2010)

# Combine 'final_data_1999_2009' with 'annual_data_2010_2020'
final_combined_data <- bind_rows(final_data_1999_2009, annual_data_2010_2020)

# ---------------------------------------------
# 4. Verify and Clean the Final Dataset
# ---------------------------------------------

# Remove any remaining rows with missing GEOID or NAME
final_combined_data <- final_combined_data %>%
  filter(!is.na(GEOID) & !is.na(NAME))

# Ensure that all years from 1999 to 2020 are included
final_combined_data <- final_combined_data %>%
  arrange(GEOID, year)

# View the first few rows of the final dataset
head(final_combined_data)

# ---------------------------------------------
# 5. Save the Final Data (Optional)
# ---------------------------------------------

# Write the final data to a CSV file
write.csv(final_combined_data, "final_data_1999_2020.csv", row.names = FALSE)

