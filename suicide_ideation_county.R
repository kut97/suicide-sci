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
### read data #####
# suicide_mortality_all_years <- read.csv("C:/Users/kusha/Downloads/suicide_mortality_year_1999_2000.csv", 
#                                        colClasses = c(FIPS = "character"))
# Get FIPS codes for all states
united_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                   "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                   "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                   "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                   "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY", "AK", "HI")

Soc.2019 <-  get_estimates(geography = "county", year=2018, product = "population",state=united_states, geometry = TRUE)
# get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
#state=eastern_states, survey="acs5", geometry = TRUE)
Soc.2019 <- Soc.2019 %>% filter(variable=="POP")
Soc.2019 <- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")


fips_code <- tidycensus::fips_codes
fips_code <- fips_code %>% filter(state %in% c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                                               "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                                               "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                                               "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                                               "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY","AK","HI"))
fips_code$GEOID <- paste0(fips_code$state_code, fips_code$county_code)

Soc.2019 <- merge(fips_code,Soc.2019, by="GEOID")
### extracting state code and fips code ###
st_code_st_abb <- Soc.2019 %>% dplyr::select(state,state_code)
st_code_st_abb <- unique(st_code_st_abb)
colnames(st_code_st_abb)[1] <- "stposto"
### merging the state_fips_code with cdc_2018_2019_to_get_5_digits_fips_code###

# Define the folder path where your CSV files are stored
folder_path <- "C:/Users/kusha/Desktop/Suicide Ideation"

# Get the list of all "Multiple_Cause_of_Death" CSV files in the directory
csv_files <- list.files(path = folder_path, pattern = "Multiple_Cause_of_Death_.*\\.csv$", full.names = TRUE)

# Create an empty list to store data frames
data_frames <- list()

# Loop over each CSV file and read it into a separate data frame
for (csv_file in csv_files) {
  # Extract the year from the file name
  year <- sub(".*_(\\d{4})\\.csv$", "\\1", basename(csv_file))
  
  # Create a dynamic name using the year
  df_name <- paste0("Multiple_Cause_of_Death_", year)
  
  # Read the CSV file and store it in the list with the year-specific name
  data_frames[[df_name]] <- read_csv(csv_file)
}

# Access individual data frames using data_frames$Multiple_Cause_of_Death_YEAR
# For example, to view the data frame for 2000, you would use:
# print(data_frames$Multiple_Cause_of_Death_2000)
# Unlist and save each data frame into the global environment
list2env(data_frames, envir = .GlobalEnv)

# Define the baseline data frame from 1999 to 2020

baseline_df <- `Multiple_Cause_of_Death_Multiple_Cause_of_Death_1999-2020.csv` %>% 
  select("County Code", "Deaths")
colnames(baseline_df)[1] <- "FIPS"

baseline_df <- na.omit(baseline_df)
# Pad FIPS codes in baseline_df

mcd_2000 <- Multiple_Cause_of_Death_2000 %>% select( "County Code", "Deaths")
colnames(mcd_2000)[1] <- "FIPS"
missing_county_codes <- setdiff(baseline_df$County,mcd_2000$County)
mcd_2000 <- na.omit(mcd_2000)
suicide_mortality_2000 <- merge(baseline_df,mcd_2000, by="FIPS")
suicide_mortality_2000$Deaths.x <- as.numeric(suicide_mortality_2000$Deaths.x)
suicide_mortality_2000$Deaths.y <- as.numeric(suicide_mortality_2000$Deaths.y)
suicide_mortality_2000 <- suicide_mortality_2000 %>% mutate(deaths=Deaths.x-Deaths.y)
suicide_mortality_2000 <- na.omit(suicide_mortality_2000)
suicide_mortality_2000$FIPS <- sprintf("%05d", as.numeric(suicide_mortality_2000$FIPS))
missing_county_codes <- setdiff(suicide_mortality_2000$FIPS,fips_code$GEOID)
missing_county_data <- suicide_mortality_2000 %>% filter(suicide_mortality_2000$FIPS %in% missing_county_codes )
suicide_mortality_2000 <- suicide_mortality_2000 %>% filter(suicide_mortality_2000$FIPS %in% fips_code$GEOID)
suicide_mortality_2000 <- suicide_mortality_2000 %>% select("FIPS", "deaths")
missing_fips_from_2019 <- setdiff(fips_code$GEOID,suicide_mortality_2000$FIPS)
missing_data <- data.frame(
  FIPS = missing_fips_from_2019,
  deaths = 0
)
suicide_mortality_2000 <- rbind(suicide_mortality_2000,missing_data)
suicide_mortality_2000$year <- 2000

#### getting fips related information for the eastern states using tidycensus ###
process_suicide_mortality <- function(year, baseline_df, fips_code) {
  # Load the data frame for the given year
  df_name <- paste0("Multiple_Cause_of_Death_", year)
  mcd_year <- get(df_name)
  
  # Select necessary columns and rename 'County Code' to 'FIPS'
  mcd_year <- mcd_year %>% select("County Code", "Deaths")
  colnames(mcd_year)[1] <- "FIPS"
  
  # Remove missing values
  mcd_year <- na.omit(mcd_year)
  
  # Merge baseline data with the year's data on 'FIPS'
  suicide_mortality_year <- merge(baseline_df, mcd_year, by = "FIPS")
  
  # Convert Deaths columns to numeric
  suicide_mortality_year$Deaths.x <- as.numeric(suicide_mortality_year$Deaths.x)
  suicide_mortality_year$Deaths.y <- as.numeric(suicide_mortality_year$Deaths.y)
  
  # Calculate 'deaths' as the difference between Deaths.x and Deaths.y
  suicide_mortality_year <- suicide_mortality_year %>% mutate(deaths = Deaths.x - Deaths.y)
  
  # Remove any NA values
  suicide_mortality_year <- na.omit(suicide_mortality_year)
  
  # Pad FIPS codes to be 5 digits
  suicide_mortality_year$FIPS <- sprintf("%05d", as.numeric(suicide_mortality_year$FIPS))
  
  # Filter to include only FIPS codes present in fips_code
  suicide_mortality_year <- suicide_mortality_year %>%
    filter(FIPS %in% fips_code$GEOID)
  
  # Select only the required columns
  suicide_mortality_year <- suicide_mortality_year %>% select("FIPS", "deaths")
  
  # Find missing FIPS codes and add them with deaths = 0
  missing_fips <- setdiff(fips_code$GEOID, suicide_mortality_year$FIPS)
  missing_data <- data.frame(
    FIPS = missing_fips,
    deaths = 0
  )
  
  # Combine the data frames
  suicide_mortality_year <- rbind(suicide_mortality_year, missing_data)
  
  # Add the 'year' column
  suicide_mortality_year$year <- year
  
  # Return the processed data frame
  return(suicide_mortality_year)
}

## mortality file###
# Initialize an empty list to store data frames for each year
suicide_mortality_list <- list()

# Loop over the years 2000 to 2020
for (year in 1999:2020) {
  # Process data for the current year
  mortality_data <- process_suicide_mortality(year, baseline_df, fips_code)
  
  # Store the data frame in the list with the year as the name
  suicide_mortality_list[[as.character(year)]] <- mortality_data
}

# Combine all years into one data frame
suicide_mortality_all_years <- bind_rows(suicide_mortality_list)

# View the combined data
head(suicide_mortality_all_years)

zero_deaths_per_year <- suicide_mortality_all_years %>%
  group_by(year) %>%
  summarize(total_zero_deaths = sum(deaths == 0))


# Merge state abbreviations into the suicide_mortality_all_years data frame
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  left_join(fips_code %>% select(GEOID, state), by = c("FIPS" = "GEOID"))

# View the updated data frame
head(suicide_mortality_all_years)

### DID ###
policy_data <- data.frame(
  state = c("CT", "MA", "IN", "IL", "CA", "DC", "WA", "NY", "OR", "CO",
            "FL", "NV", "VT", "HI", "MD", "VA", "RI", "NM", "NJ", "MN", "DE", "MI"),
  start_year = c(1999, 2018, 2005, 2018, 2014, 2018, 2016, 2019, 2017, 2020,
                 2018, 2020, 2018, 2020, 2018, 2020, 2018, 2020, 2018, 2023, 2018, 2024)
)

suicide_mortality_all_years <- suicide_mortality_all_years %>%
  left_join(policy_data, by = "state")


# Step 2: Create an indicator for the post-policy period
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  mutate(post_policy = ifelse(!is.na(start_year) & year >= start_year, 1, 0),
         treated = ifelse(!is.na(start_year), 1, 0))



# Create a numeric ID for each FIPS code
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  mutate(id = as.numeric(factor(FIPS)))

# Replace NA in start_year with 0 for untreated units
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  mutate(start_year = ifelse(is.na(start_year), 0, start_year))

#### 
# Check for missing values
sum(is.na(suicide_mortality_all_years$deaths))     # Should be 0
sum(is.na(suicide_mortality_all_years$year))       # Should be 0
sum(is.na(suicide_mortality_all_years$id))         # Should be 0
sum(is.na(suicide_mortality_all_years$start_year)) # Should be 0

### 
# Remove rows with missing data in key variables
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  filter(!is.na(deaths), !is.na(year), !is.na(id), !is.na(start_year))

# Step 2: Create Treatment Indicator (EPRO)
# 1 if policy is in effect, 0 otherwise
suicide_mortality_all_years  <- suicide_mortality_all_years  %>%
  mutate(EPRO = ifelse(!is.na(start_year) & year >= start_year, 1, 0))

# Step 3: Run the Two-Way Fixed Effects Model
# The model will use county fixed effects (county_id) and year fixed effects (year)

# Note: Ensure 'county_id' and 'year' are treated as factors for fixed effects
# suicide_mortality_all_years$county_id <- as.factor(suicide_mortality_all_years$county_id)
suicide_mortality_all_years$year_factor <- as.factor(suicide_mortality_all_years$year)

### post-lagged  ###
suicide_mortality_all_years$year_factor <- as.integer(suicide_mortality_all_years$year_factor)
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  mutate(
    ERPO_lagged = ifelse(!is.na(start_year) & year >= (start_year + 1), 1, 0)
  )

###suicide population ###
united_states <- c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                   "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                   "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                   "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                   "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY", "AK", "HI")

Soc.2019 <-  get_estimates(geography = "county", year=2018, product = "population",state=united_states, geometry = TRUE)
# get_acs(geography = "county", year=2019, variables = (c(pop="B01003_001")),
#state=eastern_states, survey="acs5", geometry = TRUE)
Soc.2019 <- Soc.2019 %>% filter(variable=="POP")
Soc.2019 <- Soc.2019 %>% separate(NAME, into = c("County", "State"), sep = ", ")


fips_code <- tidycensus::fips_codes
fips_code <- fips_code %>% filter(state %in% c("AL", "AR", "AZ", "CA", "CO", "CT", "DC", "DE", "FL", "GA", 
                                               "IA", "ID", "IL", "IN", "KS", "KY", "LA", "MA", "MD", "ME", 
                                               "MI", "MN", "MO", "MS", "MT", "NC", "ND", "NE", "NH", "NJ", 
                                               "NM", "NV", "NY", "OH", "OK", "OR", "PA", "RI", "SC", "SD", 
                                               "TN", "TX", "UT", "VA", "VT", "WA", "WI", "WV", "WY","AK","HI"))
fips_code$GEOID <- paste0(fips_code$state_code, fips_code$county_code)

Soc.2019 <- merge(fips_code,Soc.2019, by="GEOID")

Soc.2019_population  <- Soc.2019 %>% select('GEOID', 'value')
colnames(Soc.2019_population)[1] <- "FIPS"
suicide_mortality_all_years <- left_join(suicide_mortality_all_years,Soc.2019_population, by="FIPS")

### calculating death rates ###
suicide_mortality_all_years <- suicide_mortality_all_years %>% mutate(death_rates= (deaths/value)*100000)
# Run the model
suicide_mortality_all_years <- suicide_mortality_all_years[!is.na(suicide_mortality_all_years$value) & 
                                                             suicide_mortality_all_years$value >= 0, ]
### covariates #####


covariates <- read.csv("C:/Users/kusha/Desktop/suicide_ideation_r_compilation/final_data_1999_2020.csv", colClasses = c(GEOID = "character"))

## selecting covariates ###
covariates <- covariates %>% select("GEOID", "year", "poverty_rate", "unemployment_rate", "white_prop", 
                                             "black_prop", 'asian_prop', "hispanic_prop")
colnames(covariates)[1] <- "FIPS"

suicide_mortality_all_years <- merge(suicide_mortality_all_years,covariates, by=c("FIPS", "year"))



### epro exposure ###
county_county_sci <- read_tsv('C:/Users/kusha/Desktop/Data for Paper/SCI/county_county.tsv')


# Load state FIPS codes and abbreviations
data("fips_codes")

state_fips <- fips_codes %>%
  select(state_code, state) %>%
  distinct()
# Assume 'county_county_Sci_data' is your SCI data frame
county_county_sci <- county_county_sci %>%
  mutate(
    user_state_code = substr(user_loc, 1, 2),
    fr_state_code = substr(fr_loc, 1, 2)
  ) %>%
  left_join(state_fips, by = c('user_state_code' = 'state_code')) %>%
  rename(user_state = state) %>%
  left_join(state_fips, by = c('fr_state_code' = 'state_code')) %>%
  rename(fr_state = state)
total_SCI <- county_county_sci%>%
  group_by(user_loc) %>%
  summarise(total_SCI = sum(scaled_sci))
state_SCI <- county_county_sci %>%
  filter(user_state != fr_state) %>%
  group_by(user_loc, fr_state) %>%
  summarise(state_SCI = sum(scaled_sci)) %>%
  ungroup() %>%
  left_join(total_SCI, by = 'user_loc') %>%
  mutate(SCI_fraction = state_SCI / total_SCI)
state_EPRO <- suicide_mortality_all_years %>%
  select(state, year, EPRO) %>%
  distinct()
years <- unique(suicide_mortality_all_years$year)

state_SCI_years <- state_SCI %>%
  mutate(key = 1) %>%
  merge(data.frame(year = years, key = 1), by = 'key') %>%
  select(-key)

state_SCI_EPRO <- state_SCI_years %>%
  left_join(state_EPRO, by = c('fr_state' = 'state', 'year')) %>%
  mutate(EPRO = ifelse(is.na(EPRO), 0, EPRO))
EPRO_Exposure <- state_SCI_EPRO %>%
  group_by(user_loc, year) %>%
  summarise(EPRO_Exposure = sum(EPRO * SCI_fraction)) %>%
  ungroup()
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  left_join(EPRO_Exposure, by = c('FIPS' = 'user_loc', 'year'))

#### ERPO lagged exposure ####

# Step 2: Extract 'ERPO_lagged' variable for each state and year
state_ERPO_lagged <- suicide_mortality_all_years %>%
  select(state, year, ERPO_lagged) %>%
  distinct()

# Step 3: Prepare 'state_SCI_years' data
years <- unique(suicide_mortality_all_years$year)

state_SCI_years <- state_SCI %>%
  mutate(key = 1) %>%
  merge(data.frame(year = years, key = 1), by = 'key') %>%
  select(-key)

# Step 4: Merge 'state_SCI_years' with 'state_ERPO_lagged'
state_SCI_ERPO_lagged <- state_SCI_years %>%
  left_join(state_ERPO_lagged, by = c('fr_state' = 'state', 'year')) %>%
  mutate(ERPO_lagged = ifelse(is.na(ERPO_lagged), 0, ERPO_lagged))

# Step 5: Calculate 'ERPO_lagged_Exposure'
ERPO_lagged_Exposure <- state_SCI_ERPO_lagged %>%
  group_by(user_loc, year) %>%
  summarise(ERPO_lagged_Exposure = sum(ERPO_lagged * SCI_fraction)) %>%
  ungroup()

# Step 6: Merge 'ERPO_lagged_Exposure' into your main dataset
suicide_mortality_all_years <- suicide_mortality_all_years %>%
  left_join(ERPO_lagged_Exposure, by = c('FIPS' = 'user_loc', 'year'))


# ### adding covariates into the suicide_mortality_all_years #####
# 
# 
# # Step 4: Output the Model Summary
# ### it doesnt make sense
# suicide_data <- suicide_mortality_all_years
# suicide_data <- suicide_data[,-c(1,2)]
# 
# colnames(suicide_data)[10] <- "suicide ideation"
# colnames(suicide_data)[12] <- "ERPO Exposure"
# colnames(suicide_data)[8] <- "ERPO"
# suicide_data$ERPO <- as_factor(suicide_data$ERPO)
# 
# suicide_data$`suicide ideation` <- scale(suicide_data$`suicide ideation`)

## colnames ###

colnames(suicide_mortality_all_years)[9] <- "ERPO"
colnames(suicide_mortality_all_years)[10] <- "ERPO lagged"
colnames(suicide_mortality_all_years)[13] <- "suicide ideation"
colnames(suicide_mortality_all_years)[14] <- "poverty rate"
colnames(suicide_mortality_all_years)[15] <- "unemployement rate"
colnames(suicide_mortality_all_years)[16] <- "proportion white"
colnames(suicide_mortality_all_years)[17] <- "proportion black"
colnames(suicide_mortality_all_years)[18] <- "proportion asian"
colnames(suicide_mortality_all_years)[19] <- "proportion hispanic"
colnames(suicide_mortality_all_years)[20] <- "ERPO social exposure"
colnames(suicide_mortality_all_years)[21] <- "ERPO lagged social exposure"

### indirect effect ##
twfe_model_indirect_Effect <- felm(
  `suicide ideation` ~ as.factor(`ERPO`)+scale(`ERPO social exposure`)+`poverty rate`+`unemployement rate`+
    `proportion black`+ `proportion white`+`proportion asian`+ `proportion hispanic`| FIPS + year | 0 | FIPS,
  data = suicide_mortality_all_years, weights = suicide_mortality_all_years$value
)
summary(twfe_model_indirect_Effect)


twfe_model_indirect_Effect_lagged <- felm(
  `suicide ideation` ~ as.factor(`ERPO lagged`)+scale(`ERPO lagged social exposure`)+`poverty rate`+`unemployement rate`+
    `proportion black`+ `proportion white`+`proportion asian`+ `proportion hispanic`| FIPS + year | 0 | FIPS,
  data = suicide_mortality_all_years, weights = suicide_mortality_all_years$value
)

### model 



