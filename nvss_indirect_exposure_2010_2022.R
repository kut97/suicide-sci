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
library(viridis)
library(fixest)

# perceptually uniform colours

#census_api_key("e6460566746931aed6c241abe8a6e2425aa9c699", install = TRUE)

read_us_mortality_data <- function(file_path) {
  positions <- list(
    resident.status         = c(20, 20),
    state.occurrence        = c(21, 22),
    county.occurrence       = c(23, 25),
    state.residence         = c(29, 30),
    county.residence        = c(35, 37),
    education.1989          = c(61, 62),
    education.2003          = c(63, 63),
    education.flag          = c(64, 64),
    `month.of-death`        = c(65, 66),
    sex                     = c(69, 69),
    age                     = c(70, 73),
    age.flag                = c(74, 74),
    age.recode.52           = c(75, 76),
    age.recode.27           = c(77, 78),
    age.recode.12           = c(79, 80),
    age.recode.22           = c(81, 82),
    place.of.death          = c(83, 83),
    marital.status          = c(84, 84),
    `day.of-week.of-death`  = c(85, 85),
    data.year               = c(102, 105),
    injury.at.work          = c(106, 106),
    `manner.of-death`       = c(107, 107),
    `method.of-disposition` = c(108, 108),
    autopsy                 = c(109, 109),
    `activity.code`         = c(144, 144),
    `place.of.injury`       = c(145, 145),
    `ucod.icd.10`           = c(146, 149),
    `ucod.recode.358`       = c(150, 152),
    `ucod.recode.113`       = c(154, 156),
    `ucod.recode.130`       = c(157, 159),
    `ucod.recode.39`        = c(160, 161),
    entity.n                = c(163, 164),
    entity.1                = c(165, 171),
    entity.2                = c(172, 178),
    entity.3                = c(179, 185),
    entity.4                = c(186, 192),
    entity.5                = c(193, 199),
    entity.6                = c(200, 206),
    entity.7                = c(207, 213),
    entity.8                = c(214, 220),
    entity.9                = c(221, 227),
    entity.10               = c(228, 234),
    entity.11               = c(235, 241),
    entity.12               = c(242, 248),
    entity.13               = c(249, 255),
    entity.14               = c(256, 262),
    entity.15               = c(263, 269),
    entity.16               = c(270, 276),
    entity.17               = c(277, 283),
    entity.18               = c(284, 290),
    entity.19               = c(291, 297),
    entity.20               = c(298, 304),
    record.n                = c(341, 342),
    record.1                = c(344, 348),
    record.2                = c(349, 353),
    record.3                = c(354, 358),
    record.4                = c(359, 363),
    record.5                = c(364, 368),
    record.6                = c(369, 373),
    record.7                = c(374, 378),
    record.8                = c(379, 383),
    record.9                = c(384, 388),
    record.10               = c(389, 393),
    record.11               = c(394, 398),
    record.12               = c(399, 403),
    record.13               = c(404, 408),
    record.14               = c(409, 413),
    record.15               = c(414, 418),
    record.16               = c(419, 423),
    record.17               = c(424, 428),
    record.18               = c(429, 433),
    record.19               = c(434, 438),
    record.20               = c(439, 443),
    race                    = c(445, 446),
    race.flag.bridged       = c(447, 447),
    race.flag.imputation    = c(448, 448),
    race.recode.3           = c(449, 449),
    race.recode.5           = c(450, 450),
    hispanic                = c(484, 486),
    hispanic.recode         = c(488, 488),
    race.recode.40          = c(489, 490),
    `occupation.4`          = c(806, 809),
    `occupation.2`          = c(810, 811),
    `industry.4`            = c(812, 815),
    `industry.2`            = c(816, 817)
  )
  
  ss <- vapply(positions, function(x) as.integer(x[1]), integer(1))
  ee <- vapply(positions, function(x) as.integer(x[2]), integer(1))
  nn <- names(positions)
  
  readr::read_fwf(
    file = file_path,
    col_positions = readr::fwf_positions(start = ss, end = ee, col_names = nn)
  )
}

mort_2010 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2010.USPSAllCnty/MULT2010.USAllCnty.txt")
mort_2011 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2011.USPSAllCnty/MULT2011.USAllCnty.txt")
mort_2012 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2012.USPSAllCnty/MULT2012.USAllCnty.txt")
mort_2013 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2013.USPSAllCnty/MULT2013.USAllCnty.txt")
mort_2014 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2014.USPSAllCnty/MULT2014.USAllCnty.txt")
mort_2015 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2015.USPSAllCnty/MULT2015.USAllCnty.txt")
mort_2016 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2016.USPSAllCnty/MULT2016.USAllCnty.txt")
mort_2017 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2017.USPSAllCnty/MULT2017.USAllCnty.txt")
mort_2018  <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2018.USPSAllCnty/Mort2018US.AllCnty.txt")
mort_2019 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2019.USPSAllCnty/MULT2019US.AllCnty.txt")
mort_2020 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2020.AllCnty/MULT2020.USAllCnty.txt")
mort_2021 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2021.AllCnty/MULT2021US.AllCnty.txt")
mort_2022 <- read_us_mortality_data("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files/MULT2022_AllCnty/MULT2022US.AllCnty.txt")


# 1. Define suicide ICD-10 codes
suicide_codes <- c(sprintf("X%02d", 60:84), "Y87.0")

# 2. Build state lookup: postal abbreviations ??? 2-digit FIPS
abbs <- c(state.abb, "DC")
fips_codes <- c(
  "01","02","04","05","06","08","09","10","12","13","15","19","16",
  "17","18","20","21","22","23","24","25","26","27","28","29","30",
  "31","32","33","34","35","36","37","38","39","40","41","42","44",
  "45","46","47","48","49","50","51","53","54","55","56",
  "11"       # DC
)
state_codes <- tibble(
  state_abbr = abbs,
  state_fips = fips_codes
)

# 3. Helper to summarise one year's data, accounting for state abbreviations
summarise_suicides <- function(mort_df) {
  mort_df %>%
    filter(ucod.icd.10 %in% suicide_codes) %>%
    # county.occurrence is a 3-digit string or integer ??? pad to 3 chars
    mutate(
      county_fips = sprintf("%03d", as.integer(county.occurrence)),
      year        = data.year,
      state_abbr  = state.occurrence   # already "PA", "NY", etc.
    ) %>%
    group_by(year, state_abbr, county_fips) %>%
    summarise(total_deaths = n(), .groups = "drop") %>%
    # bring in the 2-digit state FIPS
    left_join(state_codes, by = "state_abbr") %>%
    # assemble the 5-digit GEOID
    mutate(GEOID = paste0(state_fips, county_fips)) %>%
    select(year, state = state_abbr, state_fips, county_fips, GEOID, total_deaths)
}

# 2. Collect all annual data-frames into a named list:
mort_list <- list(
  `2010` = mort_2010,
  `2011` = mort_2011,
  `2012` = mort_2012,
  `2013` = mort_2013,
  `2014` = mort_2014,
  `2015` = mort_2015,
  `2016` = mort_2016,
  `2017` = mort_2017,
  `2018` = mort_2018,
  `2019` = mort_2019,
  `2020` = mort_2020,
  `2021` = mort_2021,
  `2022` = mort_2022
)

# 3. Summarise suicides in each year and combine
suicide_panel_all <- bind_rows(
  lapply(mort_list, summarise_suicides),
  .id = "year_list"
) %>%
  # ensure 'year_list' and 'year' agree
  mutate(year = as.integer(year)) %>%
  select(year, state, state_fips, county_fips, GEOID, total_deaths) %>%
  arrange(year, GEOID)

## missing geoids imputing them with zero ###
# Assume `suicide_panel_all` exists with columns:
# year (integer), state (abbr), state_fips (2-digit string), county_fips (3-digit string), GEOID, total_deaths

# Function to identify missing GEOIDs for a given year
check_missing_geoids <- function(year_of_interest, panel_df) {
  # 1. Fetch ACS county GEOIDs for total population in that year
  acs_county <- get_acs(
    geography = "county",
    variables = "B01003_001",
    year      = year_of_interest,
    survey    = "acs5",
    geometry  = FALSE,
    cache_table = TRUE
  ) %>%
    select(GEOID)  # keep only GEOID for matching
  
  # 2. Subset panel to that year
  panel_sub <- panel_df %>%
    filter(year == year_of_interest) %>%
    select(GEOID, state_fips, county_fips, state) %>%
    distinct()
  
  # 3. Identify GEOIDs in panel not in ACS
  missing <- panel_sub %>%
    anti_join(acs_county, by = "GEOID") %>%
    mutate(year = year_of_interest) %>%
    select(year, state, state_fips, county_fips, GEOID)
  
  return(missing)
}

# Vector of years 2010 through 2022
years <- 2010:2022

# Loop over years, combine results
missing_all_years <- map_dfr(years, ~ check_missing_geoids(.x, suicide_panel_all))

# Inspect missing GEOIDs across all years
missing_all_years

# 2. Ensure GEOID in cdc_Wonder_data is character
cdc_Wonder_data <- read.csv(
  "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/cdc_wonder.csv",
  colClasses = c(GEOID = "character"),
  stringsAsFactors = FALSE
)
# Optionally verify:
# str(cdc_Wonder_data$GEOID)

# 3. Build reference GEOID sets
# If cdc_Wonder_data has a year column, use per-year sets; else use full unique set for all years.
has_year_col <- "year" %in% colnames(cdc_Wonder_data)

# Precompute reference lists:
if (has_year_col) {
  # assume year column is integer or numeric
  ref_geoids_by_year <- cdc_Wonder_data %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(GEOID)) %>%
    group_by(year) %>%
    summarize(ref_geoids = list(unique(GEOID)), .groups = "drop")
} else {
  ref_geoids_all <- unique(cdc_Wonder_data$GEOID)
}

# 4. Prepare suicide counts panel (as before)
suicide_panel_all <- bind_rows(
  lapply(mort_list, summarise_suicides),
  .id = "year_list"
) %>%
  # ensure 'year_list' and 'year' agree
  mutate(year = as.integer(year)) %>%
  select(year, state, state_fips, county_fips, GEOID, total_deaths) %>%
  arrange(year, GEOID)

# 5. Build complete panel using CDC WONDER reference
years <- 2010:2022

# After constructing ref_geoids_by_year when has_year_col == TRUE:
# ref_geoids_by_year is a tibble with columns year and list column ref_geoids.

complete_suicide_panel_ref <- map_dfr(years, function(y) {
  # Determine reference GEOIDs for year y
  if (has_year_col) {
    entry <- ref_geoids_by_year %>%
      filter(year == y) %>%
      pull(ref_geoids)
    if (length(entry) == 1) {
      geoids_ref <- entry[[1]]
    } else {
      # Fallback: if no CDC WONDER GEOIDs for this year, use empty vector
      geoids_ref <- character(0)
      warning(sprintf("No reference GEOIDs found for year %d; using empty set.", y))
    }
  } else {
    geoids_ref <- ref_geoids_all
  }
  
  # Construct reference tibble
  ref_df <- tibble(GEOID = geoids_ref) %>%
    mutate(
      year = y,
      state_fips  = substr(GEOID, 1, 2),
      county_fips = substr(GEOID, 3, 5)
    ) %>%
    left_join(state_codes, by = "state_fips") %>%
    rename(state = state_abbr)
  
  # Extract observed suicide counts for year y
  obs_df <- suicide_panel_all %>%
    filter(year == y) %>%
    select(GEOID, total_deaths)
  
  # Left join and impute missing total_deaths = 0
  ref_df %>%
    left_join(obs_df, by = "GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})

# Exclude if needed
exclude_geoids <- c("02158", "02261", "46102", "15005")
complete_suicide_panel_ref <- complete_suicide_panel_ref %>%
  filter(!GEOID %in% exclude_geoids)

# Verify
complete_suicide_panel_ref %>%
  group_by(year) %>%
  summarise(n_counties = n()) %>%
  print()
# 0. Reference GEOIDs from CDC WONDER (2010-2020)
ref_geoids_all <- unique(cdc_Wonder_data$GEOID)

# --- 1. Build and bind 2021 & 2022 panels using ref_geoids_all ---
additional_years <- c(2021L, 2022L)

additional_panel <- map_dfr(additional_years, function(y) {
  obs_df <- suicide_panel_all %>%
    filter(year == y) %>%
    select(GEOID, total_deaths)
  
  tibble(GEOID = ref_geoids_all) %>%
    mutate(
      year        = y,
      state_fips  = substr(GEOID, 1, 2),
      county_fips = substr(GEOID, 3, 5)
    ) %>%
    left_join(state_codes, by = "state_fips") %>%
    rename(state = state_abbr) %>%
    left_join(obs_df, by = "GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})

# --- 2. Combine and re-exclude if needed ---
complete_suicide_panel_ref <- bind_rows(
  complete_suicide_panel_ref,
  additional_panel
) %>%
  filter(!GEOID %in% exclude_geoids)

# --- 3. Verify full 2010-2022 panel ---
complete_suicide_panel_ref %>%
  group_by(year) %>%
  summarise(n_counties = n(), .groups = "drop") %>%
  print()

### covariates ###

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
      "ACS_PCT_ENGL_NOT_WELL"
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

### getting the GEOIDS excisting in the data set ###

# 3) Filter to contiguous US counties
my_panel <- my_panel %>% 
  filter(COUNTYFIPS %in% complete_suicide_panel_ref$GEOID)

### merging the data ###
suicide_mortality <- complete_suicide_panel_ref %>%
  left_join(
    my_panel,
    by = c( "GEOID"="COUNTYFIPS" , "year"= "YEAR")
  )
# Variable mapping (example; adjust if you have different definitions)
# 1. Specify every ACS table code you need
get_sdoh_acs <- function(year){
  
  core <- c(
    total_pop = "B01003_001",
    black     = "B02001_003",
    asian     = "B02001_005",
    aian      = "B02001_004",
    nhpi      = "B02001_006",
    multi     = "B02001_008",
    hisp      = "B03003_003",
    med_inc   = "B19013_001",
    unem      = "B23025_005",
    labor     = "B23025_003",
    lt_hs     = "B15003_017",
    tot_edu   = "B15003_001",
    eng_ltwell= "C16002_004",
    hh_total  = "C16002_001"
  )
  
  age18_44 <- c("B01001_007","B01001_008","B01001_009","B01001_010","B01001_011",
                "B01001_031","B01001_032","B01001_033","B01001_034","B01001_035")
  
  age45_64 <- c("B01001_012","B01001_013","B01001_014","B01001_015","B01001_016",
                "B01001_036","B01001_037","B01001_038","B01001_039","B01001_040")
  
  vars <- c(
    core,
    stats::setNames(age18_44, age18_44),    # name each age code by itself
    stats::setNames(age45_64, age45_64)
  )
  
  acs <- get_acs(
    geography   = "county",
    variables   = vars,
    year        = year,
    survey      = "acs5",
    output      = "wide",
    cache_table = TRUE
  )
  
  # Strip trailing "E" only from estimate columns
  acs <- acs %>% rename_with(~ sub("E$", "", .x), ends_with("E"))
  
  # Sum age buckets safely
  a18_44 <- acs %>% select(all_of(age18_44)) %>% as.matrix() %>% rowSums(na.rm = TRUE)
  a45_64 <- acs %>% select(all_of(age45_64)) %>% as.matrix() %>% rowSums(na.rm = TRUE)
  
  acs %>% transmute(
    COUNTYFIPS            = GEOID,
    YEAR                  = year,
    ACS_TOT_POP_WT        = total_pop,
    prop_black            = black      / total_pop,
    prop_asian            = asian      / total_pop,
    prop_other            = (aian + nhpi + multi) / total_pop,
    prop_hispanic         = hisp       / total_pop,
    ACS_MEDIAN_HH_INC     = med_inc,
    ACS_PCT_UNEMPLOY      = 100 * unem / labor,
    ACS_PCT_LT_HS         = 100 * lt_hs / tot_edu,
    ACS_PCT_AGE_18_44     = 100 * a18_44 / total_pop,
    ACS_PCT_AGE_45_64     = 100 * a45_64 / total_pop,
    ACS_PCT_ENGL_NOT_WELL = 100 * eng_ltwell / hh_total
  )
}

### ACS SDOH ### COVARIATES ###
acs21   <- get_sdoh_acs(2021)
acs22   <- get_sdoh_acs(2022)

# ------------------------------------------------------------------
# 0  Define the set of covariate columns that arrive from ACS
# ------------------------------------------------------------------
acs_vars <- c("ACS_TOT_POP_WT","prop_black","prop_asian","prop_other",
              "prop_hispanic","ACS_MEDIAN_HH_INC","ACS_PCT_UNEMPLOY",
              "ACS_PCT_LT_HS","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64",
              "ACS_PCT_ENGL_NOT_WELL")

# ------------------------------------------------------------------
# 1  Restrict ACS 2021-2022 to counties that are already in the panel
#    and harmonise key names ("GEOID", "year")
# ------------------------------------------------------------------
acs21_trim <- acs21 %>%                                     # from your workspace
  rename(GEOID = COUNTYFIPS, year = YEAR) %>%               # 5-digit ??? panel key
  semi_join(suicide_mortality, by = c("GEOID","year"))      # keep only matches

acs22_trim <- acs22 %>% 
  rename(GEOID = COUNTYFIPS, year = YEAR) %>% 
  semi_join(suicide_mortality, by = c("GEOID","year"))



acs_vars <- c("ACS_TOT_POP_WT","prop_black","prop_asian","prop_other",
              "prop_hispanic","ACS_MEDIAN_HH_INC","ACS_PCT_UNEMPLOY",
              "ACS_PCT_LT_HS","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64",
              "ACS_PCT_ENGL_NOT_WELL")

# 1. Pool 2021-2022 ACS data and align keys -------------------------------
acs_new <- bind_rows(acs21_trim, acs22_trim)      # already GEOID-year keyed

# 2. Inject 2021-22 covariates where available ----------------------------
sm_step1 <- suicide_mortality |>
  left_join(acs_new, by = c("GEOID","year"), suffix = c("", ".acs")) |>
  mutate(across(all_of(acs_vars),
                ~ coalesce(.x, get(paste0(cur_column(), ".acs"))))) |>
  select(-ends_with(".acs"))

# 3. County-specific historical means (2015-2020) -------------------------
county_means <- sm_step1 |>
  filter(year <= 2020) |>
  group_by(GEOID) |>
  summarise(across(all_of(acs_vars),
                   ~ mean(.x, na.rm = TRUE), .names = "{.col}_mu"),
            .groups = "drop")

# 4. Impute still-missing entries with county means -----------------------
sm_step2 <- sm_step1 |>
  left_join(county_means, by = "GEOID") |>
  mutate(across(all_of(acs_vars),
                ~ coalesce(.x, get(paste0(cur_column(), "_mu"))))) |>
  select(-ends_with("_mu"))

# 5. Optional: fill any residual NA with national means -------------------
national_means <- sm_step2 |>
  summarise(across(all_of(acs_vars), ~ mean(.x, na.rm = TRUE)))

suicide_mortality <- sm_step2 |>
  mutate(across(all_of(acs_vars),
                ~ coalesce(.x, national_means[[cur_column()]])))

### completing the data ###
### adding latitude and longitude in the data ###
counties <- counties(cb = TRUE, class = "sf")
entire_american_fips_vector <- unique(suicide_mortality$GEOID)
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
suicide_mortality <- left_join(suicide_mortality,geoid_lat_lng, by=c("GEOID"))

### population density ####
counties <- counties(year = 2018, cb = TRUE)
# Calculate area in square kilometers
counties <- st_transform(counties, crs = 5070)  # Transform to Albers Equal Area for accurate area calculation
counties <- counties %>%
  mutate(area_sq_km = as.numeric(st_area(geometry)) / 1e6)  # Convert area to square kilometers

counties <- counties %>% filter(GEOID %in% suicide_mortality$GEOID) %>% select(c("GEOID", "area_sq_km"))
suicide_mortality <- merge(suicide_mortality,counties,by="GEOID")
suicide_mortality <- suicide_mortality %>% mutate(population_density=ACS_TOT_POP_WT/area_sq_km)


###  Deaths per 100 000 population
# -------------------------------------------
suicide_mortality <- suicide_mortality%>% 
  mutate(death_rates_per_100_k = if_else(
    ACS_TOT_POP_WT > 0,
    (total_deaths / ACS_TOT_POP_WT) * 1e5,
    NA_real_))

my_data_with_spatial_g <- suicide_mortality 


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
                 .(w_is = sum(w_ij)),          # ?? weights to state j
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
gravity_matrix <- InvDistMat(distance_km)

# Optionally assign row/column names
rownames(gravity_matrix) <- unique_counties$GEOID
colnames(gravity_matrix) <- unique_counties$GEOID

# 4) Normalize each row to create A_{i,j}
diag(gravity_matrix) <- 0
row_sums <- rowSums(gravity_matrix)
gravity_matrix <- sweep(gravity_matrix, 1, row_sums, FUN = "/")



## --- 1.  gravity-matrix  long table ---------------------------------
gravity_dt <- as.data.table(as.table(gravity_matrix))
setnames(gravity_dt, c("i_fips","j_fips","w_ij"))

gravity_dt[, `:=`( i_fips = pad5(as.character(i_fips)),
                   j_fips = pad5(as.character(j_fips)) )]

## --- 2.  destination-state membership ---------------------------------
gravity_dt <- gravity_dt[meta, on = .(j_fips = GEOID), nomatch = 0]     # add j_state
setnames(gravity_dt, "state", "j_state")

gravity_dt <- gravity_dt[meta, on = .(i_fips = GEOID), nomatch = 0]     # add i_state
setnames(gravity_dt, "state", "i_state")

## --- 3.  attach yearly ERPO status  (exclude same state) --------------
expo_dist <- gravity_dt[state_year,
                        on = .(j_state = state), allow.cartesian = TRUE][
                          i_state != j_state &                       # << NEW
                            ERPO_active == 1L,                         # keep active years
                          .(InvDist_exposure = sum(w_ij)),           # Σ normalized 1/d_ij
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
    start_year   = replace_na(start_year, 0L),             # 0 ??? never adopts
    ever_treated = as.integer(start_year > 0L),            # county's state adopts at any time
    post         = as.integer(year >= start_year &         # observation occurs after adoption
                                start_year > 0L),
    D_it         = ever_treated * post                     # DiD indicator
  )


### all three did models ###
my_data_with_spatial_g$state_year <- interaction(my_data_with_spatial_g$state, my_data_with_spatial_g$year, drop = TRUE)

## experimenting with network exposure ####

covariates <- c("ERPO_exposure","InvDist_exposure",
  "population_density", "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64",
  "prop_black", "prop_asian", "prop_other", "prop_hispanic",
  "ACS_MEDIAN_HH_INC", "ACS_PCT_ENGL_NOT_WELL",
  "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS"
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
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
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
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
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
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
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
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
  | GEOID + state_year | 0 |state ,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(did_spill_social_spatial)

### confidence interval values for the ERPO Exposure ###
## 1 ▸ Assemble the CI data
ci_df <- bind_rows(
  tidy(did_spill_social,            conf.int = TRUE) |> mutate(model = "indirect social network exposure"),
  tidy(did_spill_social_spatial, conf.int = TRUE) |> mutate(model = "robust indirect social network exposure")
) |>
  filter(term == "ERPO_exposure")         # keep only the peer-exposure coefficient


## 2 ▸ Dot-whisker plot matching the mock-up
ggplot(ci_df,
       aes(x = estimate,
           y = factor(model, levels = c("indirect social network exposure", "robust indirect social network exposure")),
           colour = model)) +
  
  ## zero reference
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  
  ## 95 % CIs with end-caps
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.9) +
  
  ## point estimates
  geom_point(size = 3) +
  
  ## colour palette
  scale_colour_manual(values = c("indirect social network exposure" = "red",
                                 "robust indirect social network exposure" = "blue"),
                      guide = "none") +
  
  ## axis labels
  labs(x = "deaths per 100,000 people", y = NULL) +
  
  ## styling
  theme_classic(base_size = 12) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.ticks.y      = element_blank()
  )



### ENTIRE contiguous US map ###
# -----------------------------------------------------------
# 1 ▸ Compute the change in standardised ERPO exposure
# -----------------------------------------------------------
my_data_with_spatial_map <- my_data_with_spatial_g |>
  mutate(
    ERPO_exposure = if_else(GEOID == "31041" & is.na(ERPO_exposure),
                            0,            # imputed value
                            ERPO_exposure)
  )

delta_df <- dcast(
  setDT(my_data_with_spatial_map)[year %in% c(2010, 2022)],
  GEOID ~ paste0("y", year),
  value.var      = "ERPO_exposure",
  fun.aggregate  = mean,     # resolves duplicates
  na.rm          = TRUE
)[ , delta := y2022 - y2010 ]

# -----------------------------------------------------------
# 2 ▸ Keep only the 48 contiguous states + DC
# -----------------------------------------------------------
contig_fips <- sprintf("%02d", 1:56) |>
  setdiff(c("02","15","60","66","69","72","78"))   # drop AK, HI, territories

us_map <- my_data_with_spatial_map|>
  filter(year == 2022, state_fips %in% contig_fips) |>
  select(GEOID, geometry) |>
  left_join(delta_df, by = "GEOID") |>
  st_as_sf() |>
  st_transform(5070)        # Conterminous USA Albers Equal-Area
us_map$GEOID <- as.character(us_map$GEOID)

# -------------------------------
# 1 ▸ Reference geometry (48 + DC)
# -------------------------------
contig_fips <- sprintf("%02d", setdiff(1:56, c(2,15,60,66,69,72,78)))

ref <- counties(cb = TRUE, year = 2022) %>%          # cb = cartographic boundary
  filter(STATEFP %in% contig_fips) %>%               # drop AK, HI, territories
  st_transform(5070) %>%                             # same CRS as your map
  select(GEOID)                                      # keep only the key

# --------------------------------
# 2 ▸ Missing GEOIDs in your layer
# --------------------------------
missing <- anti_join(ref %>% st_drop_geometry(),      # reference keys
                     us_map %>% st_drop_geometry(),   # keys in your map
                     by = "GEOID")

print(missing)
#> returns a data frame; empty → all counties present
#> non-empty → GEOIDs of absent counties

# ------------------------------------------
# 3 ▸ If `missing` non-empty: bring them in
# ------------------------------------------
if (nrow(missing) > 0) {
  us_map <- bind_rows(
    us_map,
    ref %>%                    # geometry from reference
      filter(GEOID %in% missing$GEOID) %>% 
      mutate(                  # attach data needed for plotting
        y2010 = NA_real_,
        y2022 = NA_real_,
        delta = 0              # or other imputation rule
      )
  )
}

# ------------------------------------------
# 4 ▸ If `missing` empty but hole persists
#    → geometry invalid → repair:
# ------------------------------------------
us_map <- st_make_valid(us_map)

# Re-plot
ggplot(us_map) +
  geom_sf(aes(fill = delta), linewidth = .05, colour = NA) +
  scale_fill_viridis_c(option = "plasma",
                       name = "Δ ERPO\nsocial exposure",
                       na.value = "grey90") +
  coord_sf(crs = 5070, datum = NA) +
  theme_void(base_size = 12)
### saving the plot ###

p <- ggplot(us_map) +
  geom_sf(aes(fill = delta), linewidth = .05, colour = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name   =  "Δ ERPO\nsocial exposure" # line-1 | line-2
  ) +
  coord_sf(crs = 5070, datum = NA) +
  theme_void(base_size = 12)
p

# embed a font that has Δ

ggsave("Figure 2.pdf",
       plot   = p,
       device = cairo_pdf,   # <- vector PDF, embeds all glyphs
       width  = 6, height = 3.5, units = "in")

### STARGAZER PLOTS ###
stargazer(
  did_no_spill, 
  did_spill_social, 
  did_spill_social_spatial,
  type = "latex",
  title = "Effect of Policy Exposure on Death Rates",
  column.labels = c("Direct Effect", "Indirect Social Network Exposure", "Robust Indirect Social Network Exposure"),
  dep.var.labels = "Deaths per 100K",
  covariate.labels = c(
    "ERPO",                       # from did_no_spill
    "ERPO Social Exposure", # ERPO_exposure in did_spill_social
    "ERPO Spatial Exposure",
    "Population Density",
    "Percentage Age 18-44", 
    "Percentage Age 45-64",
    "Proportion Black",
    "Proportion Asian",
    "Proportion Other",
    "Proportion Hispanic",
    "Median Household Income",
    "Percentage Population who do not speak English that well",
    "Percentage Population who are unemployed",
    "Percentage Population with Less Than High School Education"
  )
  ,
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "output_table.tex"  # Save the output directly to a .tex file
)

### social proximity and spatial proximity ####
### deaths social proximity ####
# Subset and rename columns
#Preparing `social_df` with necessary columns
social_df <- suicide_mortality[, c("GEOID", "ACS_TOT_POP_WT", "death_rates_per_100_k")]
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

# Adjusting weights by population
population <- suicide_mortality %>%
  group_by(FIPS) %>%
  summarise(population = round(mean(ACS_TOT_POP_WT, na.rm = TRUE))) %>%
  filter(FIPS %in% nodes$fr_loc) %>%
  arrange(match(FIPS, nodes$fr_loc))  # Ensure order matches the adjacency matrix

pop_vector <- population$population


### social and spatial proximity ###

## --- 1A  social-network weights  w_ij  ---------------------------------
## Inputs:  scaled_sci (SCI_ij), pop_vector (n_j), nodes$GEOID

# 1.1  adjacency matrix of SCI_ij   (symmetric, zero diagonal)
W   <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE)
diag(W) <- 0

# 1.2  multiply each column j by population n_j
W   <- sweep(W, 2, pop_vector, `*`)

# 1.3  row-normalise   ⇒   Σ_{j≠i} w_ij = 1
W   <- sweep(W, 1, rowSums(W), `/`)
W[is.na(W)] <- 0                        


## --- 1B  spatial-proximity weights  a_ij  ------------------------------
## Inputs:  distance_km matrix (same ordering as nodes$GEOID)

A   <- InvDistMat(distance_km)        # element = 1/d_ij  (diag = ∞ handled)
diag(A) <- 0
A  <- sweep(A, 1, rowSums(A), '/')   # row-normalise

# convert to data.table for speed
setDT(my_data_with_spatial_g)
setkey(my_data_with_spatial_g, GEOID)

# result containers
my_data_with_spatial_g[, `:=` (s_minus_i = NA_real_,
                               d_minus_i = NA_real_)]

years <- sort(unique(my_data_with_spatial_g$year))


### calculation step ###
for (yr in years) {
  
  idx <- match(my_data_with_spatial_g[year == yr, GEOID], nodes$fr_loc)
  
  ## y_t is the vector of OOD rates for year t in matrix order
  y_t <- my_data_with_spatial_g[year == yr][order(idx), death_rates_per_100_k]
  
  ## social spill-over
  svec <- W[idx, , drop = FALSE] %*% y_t
  
  ## spatial spill-over
  dvec <- A[idx, , drop = FALSE] %*% y_t
  
  ## write back
  my_data_with_spatial_g[year == yr, `:=` (s_minus_i = as.numeric(svec),
                                           d_minus_i = as.numeric(dvec))]
}

my_data_with_spatial_g[,  s_minus_i_z := as.numeric(scale(s_minus_i))]
my_data_with_spatial_g[,  d_minus_i_z := as.numeric(scale(d_minus_i))]

## two-way fixed effect for  social and spatial proximity ###
proximity <- felm(
  death_rates_per_100_k ~ s_minus_i_z +
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
  | GEOID + year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(proximity)

socio_spatial_proximity <-  felm(
  death_rates_per_100_k ~ s_minus_i_z + d_minus_i_z+
    population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
  | GEOID + year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(socio_spatial_proximity)
### proximity plots ####

## 1 ▸ Extract 95% CIs for the two peer-exposure terms
## 1 ▸ Assemble the CI data
ci_df <- bind_rows(
  tidy(proximity,            conf.int = TRUE) |> mutate(model = "social"),
  tidy(socio_spatial_proximity, conf.int = TRUE) |> mutate(model = "socio-spatial")
) |>
  filter(term == "s_minus_i_z")         # keep only the peer-exposure coefficient


## 2 ▸ Dot-whisker plot matching the mock-up
ggplot(ci_df,
       aes(x = estimate,
           y = factor(model, levels = c("socio-spatial", "social")),
           colour = model)) +
  
  ## zero reference
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  
  ## 95 % CIs with end-caps
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.9) +
  
  ## point estimates
  geom_point(size = 3) +
  
  ## colour palette
  scale_colour_manual(values = c("social" = "red",
                                 "socio-spatial" = "blue"),
                      guide = "none") +
  
  ## axis labels
  labs(x = "deaths per 100,000 people", y = NULL) +
  
  ## styling
  theme_classic(base_size = 12) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.ticks.y      = element_blank()
  )



### creating the table for reression results relating to proximity ###

stargazer(
  proximity,
  socio_spatial_proximity,
  type = "latex",
  title = paste0(
    "Socio-spatial diffusion effects on county-level mortality rates (deaths per 100\\,000). ",
    "Coefficient estimates are from two population-weighted least-squares models with county ($i$) ",
    "and year ($t$) fixed effects. Column~(1) isolates \\emph{social diffusion} by regressing death ",
    "rates on standardized deaths in socially connected counties ($s_{-it}^{z}$). Column~(2) adds ",
    "standardized deaths in geographically adjacent counties ($d_{-it}^{z}$) to disentangle social ",
    "from spatial propagation. Both specifications control for population density, age composition, ",
    "racial/ethnic composition, median household income, limited-English proficiency, unemployment, ",
    "and educational attainment. Standard errors are clustered by state to accommodate arbitrary ",
    "spatial and temporal autocorrelation. The social-exposure coefficient remains large and significant ",
    "(\\textasciitilde3.0 additional deaths per 100\\,000) after adjusting for spatial proximity; the ",
    "spatial-exposure coefficient is positive and significant (\\textasciitilde0.9). Model fit is high ",
    "in both cases ($R^{2}_{\\text{within}} \\approx 0.946$). Significance levels: *$p<0.05$; ",
    "**$p<0.01$; ***$p<0.001$." ),
  column.labels = c("Social Proximity Only", "Socio-Spatial Proximity"),
  dep.var.labels = "Deaths per 100K",
  covariate.labels = c(
    "Deaths in Social Proximity",
    "Deaths in Spatial Proximity",
    "Population Density",
    "Percentage Age 18-44", 
    "Percentage Age 45-64",
    "Proportion Black",
    "Proportion Asian",
    "Proportion Other",
    "Proportion Hispanic",
    "Median Household Income",
    "Percentage Population who do not speak English that well",
    "Percentage Population who are unemployed",
    "Percentage Population with Less Than High School Education"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "output_table.tex"
)
### event study code ####
### event plot design ###
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

K_pre  <- 4   # 2010 → 2014 gap
K_post <- 2 # 2019 → 2020 gap

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
    ACS_MEDIAN_HH_INC, ACS_PCT_ENGL_NOT_WELL,
    ACS_PCT_UNEMPLOY, ACS_PCT_LT_HS,
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
            "ACS_MEDIAN_HH_INC", "ACS_PCT_ENGL_NOT_WELL",
            "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS")

fml <- death_rates_per_100_k ~ exposure:i(rel_year, ref = -1)+InvDist_exposure+
  population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
  prop_black + prop_asian + prop_other + prop_hispanic +
  ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL +
  ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS  |
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

### direct effect plot did ###

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
                     ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL +
                     ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS,
                   data = my_data_with_spatial_g,
                   weightsname    = "ACS_TOT_POP_WT",
                   control_group = "nevertreated",
                   est_method = "reg",
                   clustervars   = "state"
)

# summarize the results
summary(mw.attgt)


mw.dyn.balance <- aggte(
  mw.attgt,
  type   = "dynamic",
  min_e  = -4,    # three leads (years before treatment)
  max_e  =  2,    # four lags (years after treatment)
  na.rm  = TRUE
)


summary(mw.dyn.balance)

ggdid(mw.dyn.balance)


