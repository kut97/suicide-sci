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

# 2. Build state lookup: postal abbreviations -> 2-digit FIPS
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
    mutate(
      county_fips = sprintf("%03d", as.integer(county.occurrence)),
      year        = data.year,
      state_abbr  = state.occurrence
    ) %>%
    group_by(year, state_abbr, county_fips) %>%
    summarise(total_deaths = n(), .groups = "drop") %>%
    left_join(state_codes, by = "state_abbr") %>%
    mutate(GEOID = paste0(state_fips, county_fips)) %>%
    select(year, state = state_abbr, state_fips, county_fips, GEOID, total_deaths)
}

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

suicide_panel_all <- bind_rows(
  lapply(mort_list, summarise_suicides),
  .id = "year_list"
) %>%
  mutate(year = as.integer(year)) %>%
  select(year, state, state_fips, county_fips, GEOID, total_deaths) %>%
  arrange(year, GEOID)

check_missing_geoids <- function(year_of_interest, panel_df) {
  acs_county <- get_acs(
    geography = "county",
    variables = "B01003_001",
    year      = year_of_interest,
    survey    = "acs5",
    geometry  = FALSE,
    cache_table = TRUE
  ) %>%
    select(GEOID)
  
  panel_sub <- panel_df %>%
    filter(year == year_of_interest) %>%
    select(GEOID, state_fips, county_fips, state) %>%
    distinct()
  
  missing <- panel_sub %>%
    anti_join(acs_county, by = "GEOID") %>%
    mutate(year = year_of_interest) %>%
    select(year, state, state_fips, county_fips, GEOID)
  
  return(missing)
}

years <- 2010:2022
missing_all_years <- map_dfr(years, ~ check_missing_geoids(.x, suicide_panel_all))
missing_all_years

cdc_Wonder_data <- read.csv(
  "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/cdc_wonder.csv",
  colClasses = c(GEOID = "character"),
  stringsAsFactors = FALSE
)

has_year_col <- "year" %in% colnames(cdc_Wonder_data)

if (has_year_col) {
  ref_geoids_by_year <- cdc_Wonder_data %>%
    mutate(year = as.integer(year)) %>%
    filter(!is.na(GEOID)) %>%
    group_by(year) %>%
    summarize(ref_geoids = list(unique(GEOID)), .groups = "drop")
} else {
  ref_geoids_all <- unique(cdc_Wonder_data$GEOID)
}

suicide_panel_all <- bind_rows(
  lapply(mort_list, summarise_suicides),
  .id = "year_list"
) %>%
  mutate(year = as.integer(year)) %>%
  select(year, state, state_fips, county_fips, GEOID, total_deaths) %>%
  arrange(year, GEOID)

years <- 2010:2022

complete_suicide_panel_ref <- map_dfr(years, function(y) {
  if (has_year_col) {
    entry <- ref_geoids_by_year %>%
      filter(year == y) %>%
      pull(ref_geoids)
    if (length(entry) == 1) {
      geoids_ref <- entry[[1]]
    } else {
      geoids_ref <- character(0)
      warning(sprintf("No reference GEOIDs found for year %d; using empty set.", y))
    }
  } else {
    geoids_ref <- ref_geoids_all
  }
  
  ref_df <- tibble(GEOID = geoids_ref) %>%
    mutate(
      year = y,
      state_fips  = substr(GEOID, 1, 2),
      county_fips = substr(GEOID, 3, 5)
    ) %>%
    left_join(state_codes, by = "state_fips") %>%
    rename(state = state_abbr)
  
  obs_df <- suicide_panel_all %>%
    filter(year == y) %>%
    select(GEOID, total_deaths)
  
  ref_df %>%
    left_join(obs_df, by = "GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})

exclude_geoids <- c("02158", "02261", "46102", "15005")
complete_suicide_panel_ref <- complete_suicide_panel_ref %>%
  filter(!GEOID %in% exclude_geoids)

complete_suicide_panel_ref %>%
  group_by(year) %>%
  summarise(n_counties = n()) %>%
  print()

ref_geoids_all <- unique(cdc_Wonder_data$GEOID)

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

complete_suicide_panel_ref <- bind_rows(
  complete_suicide_panel_ref,
  additional_panel
) %>%
  filter(!GEOID %in% exclude_geoids)

complete_suicide_panel_ref %>%
  group_by(year) %>%
  summarise(n_counties = n(), .groups = "drop") %>%
  print()

### covariates ###

data_path <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/SDOH_Covariates/sdoh_csvs"

files <- list.files(
  path = data_path,
  pattern = "^sdoh_\\d{4}\\.csv$",
  full.names = TRUE
)

for (f in files) {
  
  year <- sub(".*sdoh_(\\d{4}).*", "\\1", basename(f))
  
  tmp <- read.csv(f, colClasses = c(COUNTYFIPS = "character"))
  
  tmp$prop_black     <- tmp$ACS_PCT_BLACK          / 100
  tmp$prop_asian     <- tmp$ACS_PCT_ASIAN_NONHISP  / 100
  tmp$prop_other     <- (tmp$ACS_PCT_AIAN_NONHISP +
                           tmp$ACS_PCT_NHPI_NONHISP +
                           tmp$ACS_PCT_MULT_RACE_NONHISP) / 100
  tmp$prop_hispanic  <- tmp$ACS_PCT_HISPANIC       / 100
  
  u18_vars <- c("ACS_PCT_AGE_0_4","ACS_PCT_AGE_5_9","ACS_PCT_AGE_10_14","ACS_PCT_AGE_15_17")
  tmp$ACS_PCT_AGE_U18    <- rowSums(tmp[, u18_vars, drop = FALSE], na.rm = FALSE)
  tmp$share_check <- tmp$prop_black + tmp$prop_asian + tmp$prop_other +
    (1 - tmp$prop_hispanic)
  if (any(tmp$share_check > 1.001, na.rm = TRUE)) {
    warning(sprintf("Share constraint violated in %s (year %s)", f, year))
  }
  tmp$share_check <- NULL
  
  assign(paste0("df_", year), tmp)
}

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
      "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64", "ACS_PCT_AGE_U18", 
      "ACS_PCT_AGE_ABOVE65", "ACS_PCT_ENGL_NOT_WELL"
    )
    
    vars_present <- intersect(required_vars, names(d))
    
    for (v in setdiff(required_vars, vars_present)) {
      d[[v]] <- NA
    }
    
    d %>% select(all_of(required_vars))
  })
)

str(my_panel$COUNTYFIPS)

my_panel <- my_panel %>% 
  filter(COUNTYFIPS %in% complete_suicide_panel_ref$GEOID)

suicide_mortality <- complete_suicide_panel_ref %>%
  left_join(
    my_panel,
    by = c( "GEOID"="COUNTYFIPS" , "year"= "YEAR")
  )

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
  
  u18_codes      <- c(sprintf("B01001_%03d",  3: 6),
                      sprintf("B01001_%03d", 27:30))
  
  age18_44_codes <- c(sprintf("B01001_%03d",  7:14),
                      sprintf("B01001_%03d", 31:38))
  
  age45_64_codes <- c(sprintf("B01001_%03d", 15:19),
                      sprintf("B01001_%03d", 39:43))
  
  age65p_codes   <- c(sprintf("B01001_%03d", 20:25),
                      sprintf("B01001_%03d", 44:49))
  
  vars <- c(
    core,
    stats::setNames(u18_codes,      u18_codes),
    stats::setNames(age18_44_codes, age18_44_codes),
    stats::setNames(age45_64_codes, age45_64_codes),
    stats::setNames(age65p_codes,   age65p_codes)
  )
  
  acs <- get_acs(
    geography   = "county",
    variables   = vars,
    year        = year,
    survey      = "acs5",
    output      = "wide",
    cache_table = TRUE
  )
  
  acs <- acs %>% dplyr::rename_with(~ sub("E$", "", .x), dplyr::ends_with("E"))
  
  u18    <- acs %>% dplyr::select(dplyr::all_of(u18_codes))       %>% as.matrix() %>% rowSums(na.rm = TRUE)
  a18_44 <- acs %>% dplyr::select(dplyr::all_of(age18_44_codes))  %>% as.matrix() %>% rowSums(na.rm = TRUE)
  a45_64 <- acs %>% dplyr::select(dplyr::all_of(age45_64_codes))  %>% as.matrix() %>% rowSums(na.rm = TRUE)
  a65p   <- acs %>% dplyr::select(dplyr::all_of(age65p_codes))    %>% as.matrix() %>% rowSums(na.rm = TRUE)
  
  acs %>% dplyr::transmute(
    COUNTYFIPS            = GEOID,
    YEAR                  = year,
    ACS_TOT_POP_WT        = total_pop,
    prop_black            = black      / total_pop,
    prop_asian            = asian      / total_pop,
    prop_other            = (aian + nhpi + multi) / total_pop,
    prop_hispanic         = hisp       / total_pop,
    ACS_MEDIAN_HH_INC     = med_inc,
    ACS_PCT_UNEMPLOY      = 100 * unem   / labor,
    ACS_PCT_LT_HS         = 100 * lt_hs  / tot_edu,
    ACS_PCT_AGE_U18       = 100 * u18    / total_pop,
    ACS_PCT_AGE_18_44     = 100 * a18_44 / total_pop,
    ACS_PCT_AGE_45_64     = 100 * a45_64 / total_pop,
    ACS_PCT_AGE_ABOVE65  = 100 * a65p   / total_pop,
    ACS_PCT_ENGL_NOT_WELL = 100 * eng_ltwell / hh_total
  )
}

acs21   <- get_sdoh_acs(2021)
acs22   <- get_sdoh_acs(2022)

acs_vars <- c("ACS_TOT_POP_WT","prop_black","prop_asian","prop_other",
              "prop_hispanic","ACS_MEDIAN_HH_INC","ACS_PCT_UNEMPLOY",
              "ACS_PCT_LT_HS","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64","ACS_PCT_AGE_U18", "ACS_PCT_AGE_ABOVE65",
              "ACS_PCT_ENGL_NOT_WELL")

acs21_trim <- acs21 %>%
  rename(GEOID = COUNTYFIPS, year = YEAR) %>%
  semi_join(suicide_mortality, by = c("GEOID","year"))

acs22_trim <- acs22 %>% 
  rename(GEOID = COUNTYFIPS, year = YEAR) %>% 
  semi_join(suicide_mortality, by = c("GEOID","year"))

acs_new <- bind_rows(acs21_trim, acs22_trim)

sm_step1 <- suicide_mortality |>
  left_join(acs_new, by = c("GEOID","year"), suffix = c("", ".acs")) |>
  mutate(across(all_of(acs_vars),
                ~ coalesce(.x, get(paste0(cur_column(), ".acs"))))) |>
  select(-ends_with(".acs"))

county_means <- sm_step1 |>
  filter(year <= 2020) |>
  group_by(GEOID) |>
  summarise(across(all_of(acs_vars),
                   ~ mean(.x, na.rm = TRUE), .names = "{.col}_mu"),
            .groups = "drop")

sm_step2 <- sm_step1 |>
  left_join(county_means, by = "GEOID") |>
  mutate(across(all_of(acs_vars),
                ~ coalesce(.x, get(paste0(cur_column(), "_mu"))))) |>
  select(-ends_with("_mu"))

national_means <- sm_step2 |>
  summarise(across(all_of(acs_vars), ~ mean(.x, na.rm = TRUE)))

suicide_mortality <- sm_step2 |>
  mutate(across(all_of(acs_vars),
                ~ coalesce(.x, national_means[[cur_column()]])))

### completing the data ###
counties <- counties(cb = TRUE, class = "sf")
entire_american_fips_vector <- unique(suicide_mortality$GEOID)
selected_counties <- counties[counties$GEOID %in% entire_american_fips_vector, ]

centroids <- st_centroid(selected_counties)
centroids <- centroids[order(centroids$GEOID ),]
coords <- st_coordinates(centroids)
selected_counties <- selected_counties[order(selected_counties$GEOID), ]
geoid_lat_lng<- data.frame(
  GEOID = selected_counties$GEOID,
  Longitude = coords[,1],
  Latitude = coords[,2]
)
suicide_mortality <- left_join(suicide_mortality,geoid_lat_lng, by=c("GEOID"))

### population density ####
counties <- counties(year = 2018, cb = TRUE)
counties <- st_transform(counties, crs = 5070)
counties <- counties %>%
  mutate(area_sq_km = as.numeric(st_area(geometry)) / 1e6)

counties <- counties %>% filter(GEOID %in% suicide_mortality$GEOID) %>% select(c("GEOID", "area_sq_km"))
suicide_mortality <- merge(suicide_mortality,counties,by="GEOID")
suicide_mortality <- suicide_mortality %>% mutate(population_density=ACS_TOT_POP_WT/area_sq_km)

###  Deaths per 100 000 population
suicide_mortality <- suicide_mortality%>% 
  mutate(death_rates_per_100_k = if_else(
    ACS_TOT_POP_WT > 0,
    (total_deaths / ACS_TOT_POP_WT) * 1e5,
    NA_real_))

my_data_with_spatial_g <- suicide_mortality 


# ============================================================================
#  R3.3 ADDITIONAL CONFOUNDERS -- UPDATED
#  Changes from the previous version:
#   (a) COVID-19: fixed to true ANNUAL deaths (was cumulative-to-date)
#   (c) CHR: simplified to poor_mental_health_days ONLY -- mental_health_
#       providers, frequent_mental_distress, excessive_drinking removed
#   (NEW) Alcohol-related deaths per 100k, computed directly from NVSS
# ============================================================================
zstd_narm <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

CHR_DIR   <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/chr_csvs"
RAND_XLSX <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/RAND_DATA/TL-A243-2-v4 State Firearm Law Database 6.0.xlsx"

pop_lookup <- my_data_with_spatial_g %>%
  transmute(GEOID = sprintf("%05d", as.integer(GEOID)),
            year  = as.integer(year), ACS_TOT_POP_WT) %>%
  distinct()
scaffold_conf <- expand.grid(GEOID = unique(pop_lookup$GEOID), year = 2010:2022,
                             stringsAsFactors = FALSE) %>% as_tibble()

## (a) COVID-19 deaths per 100k -- FIXED to true annual counts ---------------
## NYT's own documentation confirms `deaths` is a cumulative running total,
## not a period count. max() within (GEOID, year) therefore gives cumulative
## deaths THROUGH that year, not deaths DURING that year. This differences
## consecutive year-end cumulative values, per county, to get the true
## annual count. pmax(...,0) guards against NYT's documented rare downward
## revisions producing a negative difference.
covid_summary <- readr::read_csv(
  "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
  show_col_types = FALSE) %>%
  filter(!is.na(fips)) %>%
  mutate(GEOID = sprintf("%05d", as.integer(fips)), year = lubridate::year(date)) %>%
  group_by(GEOID, year) %>%
  summarise(covid_deaths_cum_year_end = max(deaths, na.rm = TRUE), .groups = "drop") %>%
  arrange(GEOID, year) %>%
  group_by(GEOID) %>%
  mutate(covid_deaths_annual = covid_deaths_cum_year_end - lag(covid_deaths_cum_year_end, default = 0),
         covid_deaths_annual = pmax(covid_deaths_annual, 0)) %>%
  ungroup() %>%
  select(GEOID, year, covid_deaths_annual)

covid_full <- scaffold_conf %>%
  left_join(covid_summary, by = c("GEOID","year")) %>%
  mutate(covid_deaths_annual = replace_na(covid_deaths_annual, 0)) %>%
  left_join(pop_lookup, by = c("GEOID","year")) %>%
  mutate(covid_deaths_per_100k = if_else(ACS_TOT_POP_WT > 0, covid_deaths_annual / ACS_TOT_POP_WT * 1e5, 0)) %>%
  select(GEOID, year, covid_deaths_per_100k)

## (b) Drug-overdose deaths per 100k (EXCLUDES X60-X64 = your suicide outcome)-
overdose_codes <- c(sprintf("X%02d", 40:44), "X85", sprintf("Y%02d", 10:14))
overdose_summary <- bind_rows(lapply(mort_list, function(mort_df) {
  mort_df %>% filter(ucod.icd.10 %in% overdose_codes) %>%
    mutate(county_fips = sprintf("%03d", as.integer(county.occurrence)),
           year = as.integer(data.year), state_abbr = state.occurrence) %>%
    group_by(year, state_abbr, county_fips) %>% summarise(overdose_deaths = n(), .groups = "drop") %>%
    left_join(state_codes, by = "state_abbr") %>%
    mutate(GEOID = paste0(state_fips, county_fips)) %>% select(year, GEOID, overdose_deaths)
})) %>% mutate(GEOID = sprintf("%05d", as.integer(GEOID)))
overdose_full <- scaffold_conf %>%
  left_join(overdose_summary, by = c("GEOID","year")) %>%
  mutate(overdose_deaths = replace_na(overdose_deaths, 0L)) %>%
  left_join(pop_lookup, by = c("GEOID","year")) %>%
  mutate(overdose_deaths_per_100k = if_else(ACS_TOT_POP_WT > 0, overdose_deaths / ACS_TOT_POP_WT * 1e5, 0)) %>%
  select(GEOID, year, overdose_deaths_per_100k)

## (c) County Health Rankings -- poor_mental_health_days ONLY ----------------
## mental_health_providers, frequent_mental_distress, and excessive_drinking
## have been removed: too much missing/unstable data across 2010-2022 (see
## prior diagnostics). poor_mental_health_days is the most complete of the
## four and is retained.
dir.create(CHR_DIR, showWarnings = FALSE, recursive = TRUE)
mh_patterns <- c(poor_mental_health_days  = "^v042_rawvalue$|poor.*mental.*health.*day|mentally.*unhealthy")
.b1 <- "https://www.countyhealthrankings.org/sites/default/files/"
.b2 <- "https://www.countyhealthrankings.org/sites/default/files/media/document/"
chr_urls <- c("2010"=paste0(.b1,"analytic_data2010.csv"),"2011"=paste0(.b1,"analytic_data2011.csv"),
              "2012"=paste0(.b1,"analytic_data2012.csv"),"2013"=paste0(.b1,"analytic_data2013.csv"),
              "2014"=paste0(.b1,"analytic_data2014.csv"),"2015"=paste0(.b1,"analytic_data2015.csv"),
              "2016"=paste0(.b1,"analytic_data2016.csv"),"2017"=paste0(.b1,"analytic_data2017.csv"),
              "2018"=paste0(.b1,"analytic_data2018_0.csv"),"2019"=paste0(.b2,"analytic_data2019.csv"),
              "2020"=paste0(.b2,"analytic_data2020_0.csv"),"2021"=paste0(.b2,"analytic_data2021.csv"),
              "2022"=paste0(.b2,"analytic_data2022.csv"))
for (yr in names(chr_urls)) {
  dest <- file.path(CHR_DIR, paste0("analytic_data", yr, ".csv"))
  if (!file.exists(dest)) tryCatch(download.file(chr_urls[[yr]], dest, mode = "wb", quiet = TRUE),
                                   error = function(e) message("CHR download failed ", yr, ": ", conditionMessage(e)))
}
read_chr_year <- function(f) {
  yr <- as.integer(stringr::str_extract(basename(f), "\\d{4}"))
  d  <- readr::read_csv(f, show_col_types = FALSE, col_types = cols(.default = "c")) %>% rename_with(tolower)
  fips_col <- intersect(c("fipscode","fips","5-digit fips code"), names(d))[1]
  if (is.na(fips_col) && all(c("statecode","countycode") %in% names(d))) {
    d$.fips <- paste0(sprintf("%02d", as.integer(d$statecode)), sprintf("%03d", as.integer(d$countycode)))
    fips_col <- ".fips"
  }
  if (is.na(fips_col)) return(NULL)
  out <- tibble(GEOID = sprintf("%05d", as.integer(d[[fips_col]])), year = yr)
  for (nm in names(mh_patterns)) {
    col <- grep(mh_patterns[[nm]], names(d), value = TRUE)[1]
    out[[nm]] <- if (!is.na(col)) suppressWarnings(as.numeric(d[[col]])) else NA_real_
  }
  out %>% filter(!grepl("NA", GEOID), substr(GEOID,3,5) != "000")
}
chr_panel <- map_dfr(list.files(CHR_DIR, pattern = "analytic_data\\d{4}\\.csv$", full.names = TRUE),
                     read_chr_year) %>% distinct(GEOID, year, .keep_all = TRUE)

## (NEW) Alcohol-related deaths per 100k, computed directly from NVSS --------
## NCHS standard "alcohol-induced deaths" definition. X65 (intentional
## self-poisoning by alcohol) is DELIBERATELY EXCLUDED: it falls inside
## suicide_codes (X60:X84), and including it would double-count deaths
## already captured in the outcome variable -- the same reason overdose_codes
## starts at X40, not X60. str_detect() with prefix matching is used because
## several NCHS codes are only valid at the 4-character subcode level (e.g.
## E24.4, not all of E24), while others (F10, K70) are valid as whole
## 3-character chapters.
alcohol_prefixes <- c(
  "E244",   # Alcohol-induced pseudo-Cushing's syndrome
  "F10",    # Mental/behavioural disorders due to alcohol use (whole chapter)
  "G312",   # Degeneration of nervous system due to alcohol
  "G621",   # Alcoholic polyneuropathy
  "G721",   # Alcoholic myopathy
  "I426",   # Alcoholic cardiomyopathy
  "K292",   # Alcoholic gastritis
  "K70",    # Alcoholic liver disease (whole chapter)
  "K852",   # Alcohol-induced acute pancreatitis
  "K860",   # Alcohol-induced chronic pancreatitis
  "R780",   # Finding of alcohol in blood
  "X45",    # Accidental poisoning by alcohol
  "Y15"     # Poisoning by alcohol, undetermined intent
  # X65 (intentional self-poisoning by alcohol) intentionally omitted --
  # overlaps with suicide_codes (X60:X84).
)
alcohol_pattern <- paste0("^(", paste(alcohol_prefixes, collapse = "|"), ")")

alcohol_summary <- bind_rows(lapply(mort_list, function(mort_df) {
  mort_df %>%
    filter(stringr::str_detect(ucod.icd.10, alcohol_pattern)) %>%
    mutate(county_fips = sprintf("%03d", as.integer(county.occurrence)),
           year = as.integer(data.year), state_abbr = state.occurrence) %>%
    group_by(year, state_abbr, county_fips) %>%
    summarise(alcohol_deaths = n(), .groups = "drop") %>%
    left_join(state_codes, by = "state_abbr") %>%
    mutate(GEOID = paste0(state_fips, county_fips)) %>%
    select(year, GEOID, alcohol_deaths)
})) %>% mutate(GEOID = sprintf("%05d", as.integer(GEOID)))

alcohol_full <- scaffold_conf %>%
  left_join(alcohol_summary, by = c("GEOID", "year")) %>%
  mutate(alcohol_deaths = replace_na(alcohol_deaths, 0L)) %>%
  left_join(pop_lookup, by = c("GEOID", "year")) %>%
  mutate(alcohol_deaths_per_100k = if_else(ACS_TOT_POP_WT > 0, alcohol_deaths / ACS_TOT_POP_WT * 1e5, 0)) %>%
  select(GEOID, year, alcohol_deaths_per_100k)

# Verify before trusting this: confirm no overlap with the suicide outcome.
# This should return 0 rows.
# bind_rows(mort_list) %>%
#   filter(ucod.icd.10 %in% suicide_codes,
#         stringr::str_detect(ucod.icd.10, alcohol_pattern)) %>%
#   nrow()

## (d) RAND firearm policies (state x year; binary in-force flags) -----------
if (file.exists(RAND_XLSX)) {
  db <- read_excel(RAND_XLSX, sheet = "Database") %>%
    transmute(state=`State Postal Abbreviation`, class=`Law Class`, subtype=`Law Class Subtype`,
              guns=`Handguns or Long Guns`, effect=`Effect`, change=`Type of Change`,
              eff_year=suppressWarnings(as.integer(`Effective Date Year`)),
              age=suppressWarnings(as.integer(`Age for Minimum Age Laws`))) %>% filter(!is.na(eff_year))
  ev <- db %>% filter(class %in% c("waiting period","child access laws","minimum age")) %>%
    mutate(delta = case_when(change=="Implement" & effect=="Restrictive" ~ 1L,
                             change=="Repeal" & effect=="Permissive" ~ -1L, TRUE ~ 0L)) %>%
    group_by(state, class, year = eff_year) %>% summarise(delta = sum(delta), .groups = "drop")
  in_force <- ev %>% distinct(state, class) %>% tidyr::crossing(year = min(ev$year):2022L) %>%
    left_join(ev, by = c("state","class","year")) %>% mutate(delta = replace_na(delta, 0L)) %>%
    arrange(state, class, year) %>% group_by(state, class) %>%
    mutate(in_force = as.integer(cumsum(delta) > 0)) %>% ungroup() %>% filter(year %in% 2010:2022)
  firearm_binary <- in_force %>%
    mutate(key = recode(class, "waiting period"="fa_waiting_period","child access laws"="fa_cap","minimum age"="fa_min_age")) %>%
    select(state, year, key, in_force) %>% pivot_wider(names_from = key, values_from = in_force, values_fill = 0)
  first21 <- db %>% filter(class=="minimum age", effect=="Restrictive",
                           grepl("purchase|sale", tolower(subtype)), grepl("handgun", tolower(guns)),
                           !is.na(age), age >= 21) %>% group_by(state) %>% summarise(first21 = min(eff_year), .groups="drop")
  firearm_min21 <- tidyr::crossing(state = unique(firearm_binary$state), year = 2010:2022L) %>%
    left_join(first21, by = "state") %>% mutate(fa_min_age_21 = as.integer(!is.na(first21) & year >= first21)) %>%
    select(state, year, fa_min_age_21)
  firearm_policy <- firearm_binary %>% left_join(firearm_min21, by = c("state","year"))
} else {
  warning("RAND xlsx not found; firearm columns set to 0.")
  firearm_policy <- tidyr::crossing(state = unique(my_data_with_spatial_g$state), year = 2010:2022L) %>%
    mutate(fa_waiting_period = 0L, fa_cap = 0L, fa_min_age = 0L, fa_min_age_21 = 0L)
}

## (e) merge into the panel, bridge CHR gaps, standardize continuous ---------
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(GEOID = sprintf("%05d", as.integer(GEOID)), year = as.integer(year)) %>%
  left_join(covid_full,     by = c("GEOID","year")) %>%
  left_join(overdose_full,  by = c("GEOID","year")) %>%
  left_join(alcohol_full,   by = c("GEOID","year")) %>%
  left_join(chr_panel,      by = c("GEOID","year")) %>%
  left_join(firearm_policy, by = c("state","year")) %>%
  mutate(covid_deaths_per_100k    = replace_na(covid_deaths_per_100k, 0),
         overdose_deaths_per_100k = replace_na(overdose_deaths_per_100k, 0),
         alcohol_deaths_per_100k  = replace_na(alcohol_deaths_per_100k, 0),
         across(any_of(c("fa_waiting_period","fa_cap","fa_min_age","fa_min_age_21")), ~ replace_na(.x, 0L)))
# bridge CHR gap years within county (poor_mental_health_days only now),
# then z-score and set NA -> 0 (= the mean, preserving N) so no observations
# drop out of the regressions
my_data_with_spatial_g <- my_data_with_spatial_g %>% arrange(GEOID, year) %>% group_by(GEOID) %>%
  fill(any_of(names(mh_patterns)), .direction = "downup") %>% ungroup()
.new_cont <- c("covid_deaths_per_100k","overdose_deaths_per_100k","alcohol_deaths_per_100k", names(mh_patterns))
my_data_with_spatial_g[.new_cont] <- lapply(my_data_with_spatial_g[.new_cont], zstd_narm)
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(across(any_of(names(mh_patterns)), ~ replace_na(.x, 0)))
# quick check (optional): cat(intersect(.new_cont, names(my_data_with_spatial_g)), "\n")
# ---- end R3.3 ADDITIONAL CONFOUNDERS ---------------------------------------


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

t_1 <- t_1 %>%
  mutate(
    ever_treated = if_else(start_year > 0, 1, 0),
    post         = if_else(year >= start_year & start_year > 0, 1, 0)
  )
t_1 <- t_1 %>%
  mutate(D_it = as.numeric(ever_treated * post))

### ERPO exposure ###
pad5 <- function(x) sprintf("%05d", as.integer(x))

sci  <- fread("C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv")
meta <- fread("C:/Users/kusha/Downloads/county_data.csv",
              select = c("GEOID", "state")) |> unique()

if (!all(c("i_fips","j_fips") %in% names(sci))) {
  setnames(sci,
           old = names(sci)[1:2],
           new = c("i_fips","j_fips"))
}
sci[,  `:=`(i_fips = pad5(i_fips),
            j_fips = pad5(j_fips))]
meta[, GEOID := pad5(GEOID)]

if (!"SCI" %in% names(sci)) {
  num_cols <- names(Filter(is.numeric, sci))
  num_cols <- setdiff(num_cols, c("i_fips","j_fips"))
  if (length(num_cols) != 1)
    stop("Cannot unambiguously identify SCI column.")
  setnames(sci, num_cols, "SCI")
}

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

sci[, totalSCI := sum(SCI), by = i_fips]
sci[, w_ij     := SCI / totalSCI]

w_i_state <- sci[i_state != j_state,
                 .(w_is = sum(w_ij)),
                 by = .(i_fips, j_state)]

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

expo <- w_i_state[state_year,
                  on = .(j_state = state),
                  allow.cartesian = TRUE][
                    ERPO_active == 1L,
                    .(ERPO_exposure = sum(w_is)),
                    by = .(i_fips, year)]

my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(GEOID = pad5(GEOID)) %>%
  left_join(expo, by = c("GEOID" = "i_fips", "year")) %>%
  mutate(ERPO_exposure = replace_na(ERPO_exposure, 0))

unique_counties <- my_data_with_spatial_g %>%
  dplyr::distinct(GEOID, Longitude, Latitude) %>%
  dplyr::arrange(GEOID)

distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")],
                                measure = "geodesic") / 1000

gravity_matrix <- InvDistMat(distance_km)

rownames(gravity_matrix) <- unique_counties$GEOID
colnames(gravity_matrix) <- unique_counties$GEOID

diag(gravity_matrix) <- 0
row_sums <- rowSums(gravity_matrix)
gravity_matrix <- sweep(gravity_matrix, 1, row_sums, FUN = "/")

gravity_dt <- as.data.table(as.table(gravity_matrix))
setnames(gravity_dt, c("i_fips","j_fips","w_ij"))

gravity_dt[, `:=`( i_fips = pad5(as.character(i_fips)),
                   j_fips = pad5(as.character(j_fips)) )]

gravity_dt <- gravity_dt[meta, on = .(j_fips = GEOID), nomatch = 0]
setnames(gravity_dt, "state", "j_state")

gravity_dt <- gravity_dt[meta, on = .(i_fips = GEOID), nomatch = 0]
setnames(gravity_dt, "state", "i_state")

expo_dist <- gravity_dt[state_year,
                        on = .(j_state = state), allow.cartesian = TRUE][
                          i_state != j_state &
                            ERPO_active == 1L,
                          .(InvDist_exposure = sum(w_ij)),
                          by = .(i_fips, year)]
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(GEOID = pad5(GEOID)) %>%
  left_join(expo_dist, by = c("GEOID" = "i_fips", "year")) %>%
  mutate(InvDist_exposure = replace_na(InvDist_exposure, 0))

my_data_with_spatial_g <- my_data_with_spatial_g %>% 
  left_join(policy_data, by = "state") %>%
  mutate(
    start_year   = replace_na(start_year, 0L),
    ever_treated = as.integer(start_year > 0L),
    post         = as.integer(year >= start_year &
                                start_year > 0L),
    D_it         = ever_treated * post
  )

my_data_with_spatial_g$state_year <- interaction(my_data_with_spatial_g$state, my_data_with_spatial_g$year, drop = TRUE)

covariates <- c("ERPO_exposure","InvDist_exposure",
                "population_density","ACS_PCT_AGE_U18", "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64", "ACS_PCT_AGE_ABOVE65",
                "prop_black", "prop_asian", "prop_other", "prop_hispanic",
                "ACS_MEDIAN_HH_INC", "ACS_PCT_ENGL_NOT_WELL",
                "ACS_PCT_UNEMPLOY", "ACS_PCT_LT_HS"
)

normalised <- function(x)
{
  (x - mean(x)) / sd(x)
}

my_data_with_spatial_g[covariates] <- as.data.frame(lapply(my_data_with_spatial_g[covariates], normalised))


# Political Affiliation Integration for Suicide Analysis (2010-2022)

process_election_data_0816 <- function(file_path) {
  
  if (!file.exists(file_path)) {
    cat("Warning: File not found:", file_path, "\n")
    return(tibble())
  }
  
  election_0816 <- read_csv(file_path, show_col_types = FALSE)
  
  political_results <- tibble()
  
  election_2008 <- election_0816 %>%
    select(fips_code, dem_2008, gop_2008) %>%
    filter(!is.na(fips_code), !is.na(dem_2008), !is.na(gop_2008)) %>%
    mutate(
      GEOID = str_pad(as.character(fips_code), width = 5, pad = "0"),
      political_affiliation = as.integer(gop_2008 > dem_2008),
      election_year = 2008
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  election_2012 <- election_0816 %>%
    select(fips_code, dem_2012, gop_2012) %>%
    filter(!is.na(fips_code), !is.na(dem_2012), !is.na(gop_2012)) %>%
    mutate(
      GEOID = str_pad(as.character(fips_code), width = 5, pad = "0"),
      political_affiliation = as.integer(gop_2012 > dem_2012),
      election_year = 2012
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  election_2016 <- election_0816 %>%
    select(fips_code, dem_2016, gop_2016) %>%
    filter(!is.na(fips_code), !is.na(dem_2016), !is.na(gop_2016)) %>%
    mutate(
      GEOID = str_pad(as.character(fips_code), width = 5, pad = "0"),
      political_affiliation = as.integer(gop_2016 > dem_2016),
      election_year = 2016
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  political_results <- bind_rows(election_2008, election_2012, election_2016)
  
  return(political_results)
}

process_election_data_2020 <- function(file_path) {
  
  if (!file.exists(file_path)) {
    cat("Warning: File not found:", file_path, "\n")
    return(tibble())
  }
  
  election_2020 <- read_csv(file_path, show_col_types = FALSE)
  
  election_2020 <- election_2020 %>%
    filter(!is.na(county_fips), !is.na(votes_dem), !is.na(votes_gop)) %>%
    mutate(
      GEOID = str_pad(as.character(county_fips), width = 5, pad = "0"),
      political_affiliation = as.integer(votes_gop > votes_dem),
      election_year = 2020
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  return(election_2020)
}

cat("Processing 2008-2016 election data...\n")
political_0816 <- process_election_data_0816("C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_08-16.csv")

cat("Processing 2020 election data...\n") 
political_2020 <- process_election_data_2020("C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_2020.csv")

all_political_results <- bind_rows(political_0816, political_2020)

if (nrow(all_political_results) == 0) {
  cat("ERROR: No political data loaded. Check file paths:\n")
  cat("- C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_08-16.csv\n")
  cat("- C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_2020.csv\n")
  cat("Skipping political affiliation analysis...\n")
} else {
  all_political_results <- all_political_results %>%
    mutate(GEOID = ifelse(GEOID == "46113", "46102", GEOID))
  
  cat("\nElection data summary:\n")
  all_political_results %>%
    group_by(election_year) %>%
    summarise(
      counties = n(),
      pct_republican = round(mean(political_affiliation, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    print()
  
  assign_political_affiliation <- function() {
    
    year_assignments <- tibble(
      analysis_year = 2010:2022,
      election_year = case_when(
        analysis_year %in% c(2010, 2011) ~ 2008,
        analysis_year == 2012 ~ 2012,
        analysis_year %in% c(2013, 2014, 2015) ~ 2012,
        analysis_year == 2016 ~ 2016,
        analysis_year %in% c(2017, 2018, 2019) ~ 2016,
        analysis_year == 2020 ~ 2020,
        analysis_year %in% c(2021, 2022) ~ 2020,
        TRUE ~ NA_real_
      )
    )
    
    cat("Year assignment logic:\n")
    print(year_assignments)
    
    political_panel <- year_assignments %>%
      left_join(all_political_results, by = "election_year") %>%
      select(GEOID, analysis_year, political_affiliation) %>%
      rename(year = analysis_year) %>%
      filter(!is.na(GEOID), !is.na(political_affiliation))
    
    return(political_panel)
  }
  
  political_panel <- assign_political_affiliation()
  
  if (exists("my_data_with_spatial_g")) {
    
    analysis_counties <- unique(my_data_with_spatial_g$GEOID)
    political_counties <- unique(political_panel$GEOID)
    matched_counties <- intersect(analysis_counties, political_counties)
    missing_counties <- setdiff(analysis_counties, political_counties)
    
    cat("\nCounty matching summary:\n")
    cat("Analysis counties:", length(analysis_counties), "\n")
    cat("Political counties:", length(political_counties), "\n")
    cat("Matched counties:", length(matched_counties), "\n")
    cat("Missing from political data:", length(missing_counties), "\n")
    
    if (length(missing_counties) > 0) {
      cat("\nMissing counties (GEOID):\n")
      print(missing_counties)
      
      missing_county_details <- my_data_with_spatial_g %>%
        filter(GEOID %in% missing_counties) %>%
        select(GEOID, state, county_fips, state_fips) %>%
        distinct() %>%
        arrange(state, GEOID)
      
      cat("\nMissing counties details:\n")
      print(missing_county_details)
    }
    
    cat("\nMerging political affiliation into main dataset...\n")
    
    my_data_with_spatial_g <- my_data_with_spatial_g %>%
      left_join(
        political_panel %>% select(GEOID, year, political_affiliation),
        by = c("GEOID", "year")
      )
    
    if (!"political_affiliation" %in% names(my_data_with_spatial_g)) {
      cat("ERROR: political_affiliation column not created properly.\n")
    } else {
      total_observations <- nrow(my_data_with_spatial_g)
      successful_merges <- my_data_with_spatial_g %>%
        filter(!is.na(political_affiliation)) %>%
        nrow()
      
      missing_observations <- my_data_with_spatial_g %>%
        filter(is.na(political_affiliation)) %>%
        nrow()
      
      cat("Political affiliation successfully merged!\n")
      cat("Successful merges:", successful_merges, "out of", total_observations, "observations\n")
      cat("Missing observations:", missing_observations, "\n")
      
      if (missing_observations > 0) {
        cat("\nDetailed analysis of missing observations:\n")
        
        missing_obs_details <- my_data_with_spatial_g %>%
          filter(is.na(political_affiliation)) %>%
          group_by(GEOID, state) %>%
          summarise(
            years_missing = n(),
            first_year = min(year),
            last_year = max(year),
            .groups = "drop"
          ) %>%
          arrange(state, GEOID)
        
        cat("Counties with missing political affiliation data:\n")
        print(missing_obs_details)
        
        cat("\nMissing observations by state:\n")
        missing_by_state <- my_data_with_spatial_g %>%
          filter(is.na(political_affiliation)) %>%
          group_by(state) %>%
          summarise(
            missing_observations = n(),
            unique_counties = n_distinct(GEOID),
            .groups = "drop"
          ) %>%
          arrange(desc(missing_observations))
        
        print(missing_by_state)
      }
      
      if (exists("covariates")) {
        covariates_updated <- c(covariates, "political_affiliation")
        assign("covariates", covariates_updated, envir = .GlobalEnv)
        cat("\nUpdated covariates list includes: political_affiliation\n")
      }
      
      if (missing_observations > 0) {
        cat("\nApplying Alaska county imputation...\n")
        
        alaska_imputation <- tribble(
          ~GEOID, ~year, ~political_affiliation, ~source,
          
          "02013", 2010, 1, "actual_election",  "02013", 2011, 1, "actual_election",
          "02013", 2012, 1, "actual_election",  "02013", 2013, 1, "actual_election",
          "02013", 2014, 1, "actual_election",  "02013", 2015, 1, "actual_election",
          "02013", 2016, 1, "actual_election",  "02013", 2017, 1, "actual_election",
          "02013", 2018, 1, "actual_election",  "02013", 2019, 1, "actual_election",
          "02013", 2020, 1, "actual_election",  "02013", 2021, 1, "actual_election",
          "02013", 2022, 1, "actual_election",
          
          "02016", 2010, 1, "actual_election",  "02016", 2011, 1, "actual_election",
          "02016", 2012, 0, "actual_election",  "02016", 2013, 0, "actual_election",
          "02016", 2014, 0, "actual_election",  "02016", 2015, 0, "actual_election",
          "02016", 2016, 0, "actual_election",  "02016", 2017, 0, "actual_election",
          "02016", 2018, 0, "actual_election",  "02016", 2019, 0, "actual_election",
          "02016", 2020, 0, "actual_election",  "02016", 2021, 0, "actual_election",
          "02016", 2022, 0, "actual_election",
          
          "02020", 2010, 1, "actual_election",  "02020", 2011, 1, "actual_election",
          "02020", 2012, 1, "actual_election",  "02020", 2013, 1, "actual_election",
          "02020", 2014, 1, "actual_election",  "02020", 2015, 1, "actual_election",
          "02020", 2016, 0, "actual_election",  "02020", 2017, 0, "actual_election",
          "02020", 2018, 0, "actual_election",  "02020", 2019, 0, "actual_election",
          "02020", 2020, 0, "actual_election",  "02020", 2021, 0, "actual_election",
          "02020", 2022, 0, "actual_election"
        ) %>%
          bind_rows(
            expand_grid(
              GEOID = c("02050", "02060", "02068", "02070", "02090", "02100", "02105", 
                        "02110", "02122", "02130", "02150", "02164", "02170", "02180", 
                        "02185", "02188", "02195", "02198", "02220", "02230", "02240", 
                        "02275", "02282", "02290"),
              year = 2010:2022
            ) %>%
              mutate(
                political_affiliation = 1,
                source = "alaska_pattern"
              )
          )
        
        my_data_with_spatial_g <- my_data_with_spatial_g %>%
          left_join(
            alaska_imputation %>% 
              filter(GEOID %in% missing_counties) %>%
              select(GEOID, year, political_affiliation, source),
            by = c("GEOID", "year"),
            suffix = c("", "_alaska")
          ) %>%
          mutate(
            political_affiliation = coalesce(political_affiliation, political_affiliation_alaska),
            political_imputed = ifelse(!is.na(political_affiliation_alaska), 1, 0)
          ) %>%
          select(-political_affiliation_alaska, -source)
        
        final_missing <- sum(is.na(my_data_with_spatial_g$political_affiliation))
        alaska_imputed <- sum(my_data_with_spatial_g$political_imputed == 1, na.rm = TRUE)
        
        cat("Alaska imputation results:\n")
        cat("Alaska observations imputed:", alaska_imputed, "\n")
        cat("Final missing political_affiliation:", final_missing, "\n")
        
        if (final_missing == 0) {
          cat("SUCCESS: All political affiliation data complete!\n")
        }
      }
      
      cat("\nPolitical affiliation integration complete!\n")
      cat("Use 'political_affiliation' variable in your models (1=Republican, 0=Democrat)\n")
    }
    
  } else {
    cat("Error: my_data_with_spatial_g dataset not found in environment.\n")
    cat("Please ensure your main analysis dataset is loaded before running this script.\n")
  }
}


# =============================================================================
# REGRESSIONS -- UPDATED
# mental_health_providers, frequent_mental_distress, and excessive_drinking
# REMOVED from every model below. alcohol_deaths_per_100k ADDED. 
# poor_mental_health_days RETAINED.
# =============================================================================

## no spill over ####
did_no_spill <- felm(
  death_rates_per_100_k ~ D_it +   population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+ prop_asian +
    prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  + fa_waiting_period + fa_cap + fa_min_age_21
  | GEOID + year        # Fixed Effects
  | 0                    # No instrumental variables
  | state,               # Cluster by state
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(did_no_spill)

### spatial spillover

did_spill_spatial <- felm(
  death_rates_per_100_k ~ InvDist_exposure+
    population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+ prop_asian +
    prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  | GEOID + state_year | 0 | state,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(did_spill_spatial)

### social spillover ###
did_spill_social <- felm(
  death_rates_per_100_k ~  ERPO_exposure +
    population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+ prop_asian +
    prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  | GEOID + state_year | 0 | state,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(did_spill_social)


### spatial and social spillover ####
did_spill_social_spatial <- felm(
  death_rates_per_100_k ~ ERPO_exposure +  InvDist_exposure+ population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+ prop_asian +
    prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  | GEOID + state_year | 0 |state ,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(did_spill_social_spatial)

### confidence interval values for the ERPO Exposure ###
ci_df <- bind_rows(
  tidy(did_spill_social,            conf.int = TRUE) |> mutate(model = "indirect social network exposure"),
  tidy(did_spill_social_spatial, conf.int = TRUE) |> mutate(model = "indirect social network exposure (controls for Spatial Exposure)")
) |>
  filter(term == "ERPO_exposure")

ggplot(ci_df,
       aes(x = estimate,
           y = factor(model, levels = c(
             "indirect social network exposure",
             "indirect social network exposure (controls for Spatial Exposure)"
           )),
           colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c(
    "indirect social network exposure" = "red",
    "indirect social network exposure (controls for Spatial Exposure)" = "blue"
  ), guide = "none") +
  scale_y_discrete(labels = c(
    "indirect social network exposure" = "indirect social network exposure",
    "indirect social network exposure (controls for Spatial Exposure)" =
      "indirect social network exposure\n(controls for Spatial Exposure)"
  )) +
  labs(x = "Change in focal-county suicide mortality (per 100,000)\nfor a one-SD increase in ERPO social exposure",
       y = NULL) +
  theme_classic(base_size = 12) +
  theme(panel.grid.major.y = element_line(colour = "grey85"),
        axis.ticks.y = element_blank())


### ENTIRE contiguous US map ###
my_data_with_spatial_map <- my_data_with_spatial_g |>
  mutate(
    ERPO_exposure = if_else(GEOID == "31041" & is.na(ERPO_exposure),
                            0,
                            ERPO_exposure)
  )

delta_df <- dcast(
  setDT(my_data_with_spatial_map)[year %in% c(2010, 2022)],
  GEOID ~ paste0("y", year),
  value.var      = "ERPO_exposure",
  fun.aggregate  = mean,
  na.rm          = TRUE
)[ , delta := y2022 - y2010 ]

contig_fips <- sprintf("%02d", 1:56) |>
  setdiff(c("02","15","60","66","69","72","78"))

us_map <- my_data_with_spatial_map|>
  filter(year == 2022, state_fips %in% contig_fips) |>
  select(GEOID, geometry) |>
  left_join(delta_df, by = "GEOID") |>
  st_as_sf() |>
  st_transform(5070)
us_map$GEOID <- as.character(us_map$GEOID)

contig_fips <- sprintf("%02d", setdiff(1:56, c(2,15,60,66,69,72,78)))

ref <- counties(cb = TRUE, year = 2022) %>%
  filter(STATEFP %in% contig_fips) %>%
  st_transform(5070) %>%
  select(GEOID)

missing <- anti_join(ref %>% st_drop_geometry(),
                     us_map %>% st_drop_geometry(),
                     by = "GEOID")

print(missing)

if (nrow(missing) > 0) {
  us_map <- bind_rows(
    us_map,
    ref %>%
      filter(GEOID %in% missing$GEOID) %>% 
      mutate(
        y2010 = NA_real_,
        y2022 = NA_real_,
        delta = 0
      )
  )
}

us_map <- st_make_valid(us_map)

ggplot(us_map) +
  geom_sf(aes(fill = delta), linewidth = .05, colour = NA) +
  scale_fill_viridis_c(option = "plasma",
                       name = "Δ ERPO\nsocial exposure",
                       na.value = "grey90") +
  coord_sf(crs = 5070, datum = NA) +
  theme_void(base_size = 12)

p <- ggplot(us_map) +
  geom_sf(aes(fill = delta), linewidth = .05, colour = NA) +
  scale_fill_viridis_c(
    option = "plasma",
    name   =  "Δ ERPO\nsocial exposure"
  ) +
  coord_sf(crs = 5070, datum = NA) +
  theme_void(base_size = 12)
p

ggsave("Figure 2.pdf",
       plot   = p,
       device = cairo_pdf,
       width  = 6, height = 3.5, units = "in")

### STARGAZER PLOTS -- UPDATED covariate.labels (3 removed, 1 added) ###
stargazer(
  did_no_spill, 
  did_spill_social, 
  did_spill_social_spatial,
  type = "latex",
  title = "Effect of Policy Exposure on Death Rates",
  column.labels = c("Direct Effect", "Indirect Social Network Exposure", "Robust Indirect Social Network Exposure"),
  dep.var.labels = "Deaths per 100K",
  covariate.labels = c(
    "ERPO",
    "ERPO Social Exposure",
    "ERPO Spatial Exposure",
    "Population density",
    "Percent aged below 18",
    "Percent aged 18-44",
    "Percent aged 45-64",
    "Percent Asian",
    "Percent Black",
    "Percent Other",
    "Percent Hispanic",
    "Median household income ",
    "Percent with limited English proficiency",
    "Percent unemployed",
    "Percent with less than high school education",
    "Political Affiliation",
    "COVID-19 deaths per 100k",
    "Drug-overdose deaths per 100k",
    "Alcohol-related deaths per 100k",
    "Poor mental health days",
    "Firearm: waiting period",
    "Firearm: child-access prevention",
    "Firearm: minimum purchase age 21"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "output_table.tex"
)

### social proximity and spatial proximity ####
social_df <- suicide_mortality[, c("GEOID", "ACS_TOT_POP_WT", "death_rates_per_100_k")]
colnames(social_df)[1] <- "fr_loc"
colnames(social_df)[3] <- "deaths_per_capita"
social_df <- social_df[order(social_df$fr_loc), ]

df_0 <- read_tsv("C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv")
df_1 <- df_0 %>%
  filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc) %>%
  filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))

df_for_matrix_weights <- df_1 %>% select(user_loc, fr_loc, scaled_sci)
nodes <- social_df %>% select(fr_loc) %>% distinct()

k <- graph.data.frame(df_for_matrix_weights, directed = FALSE, vertices = nodes)

colnames(suicide_mortality)[1] <- "FIPS"

population <- suicide_mortality %>%
  group_by(FIPS) %>%
  summarise(population = round(mean(ACS_TOT_POP_WT, na.rm = TRUE))) %>%
  filter(FIPS %in% nodes$fr_loc) %>%
  arrange(match(FIPS, nodes$fr_loc))

pop_vector <- population$population

W   <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE)
diag(W) <- 0

W   <- sweep(W, 2, pop_vector, `*`)

W   <- sweep(W, 1, rowSums(W), `/`)
W[is.na(W)] <- 0                        

A   <- InvDistMat(distance_km)
diag(A) <- 0
A  <- sweep(A, 1, rowSums(A), '/')

setDT(my_data_with_spatial_g)
setkey(my_data_with_spatial_g, GEOID)

my_data_with_spatial_g[, `:=` (s_minus_i = NA_real_,
                               d_minus_i = NA_real_)]

years <- sort(unique(my_data_with_spatial_g$year))

for (yr in years) {
  
  idx <- match(my_data_with_spatial_g[year == yr, GEOID], nodes$fr_loc)
  
  y_t <- my_data_with_spatial_g[year == yr][order(idx), death_rates_per_100_k]
  
  svec <- W[idx, , drop = FALSE] %*% y_t
  
  dvec <- A[idx, , drop = FALSE] %*% y_t
  
  my_data_with_spatial_g[year == yr, `:=` (s_minus_i = as.numeric(svec),
                                           d_minus_i = as.numeric(dvec))]
}

my_data_with_spatial_g[,  s_minus_i_z := as.numeric(scale(s_minus_i))]
my_data_with_spatial_g[,  d_minus_i_z := as.numeric(scale(d_minus_i))]

## two-way fixed effect for  social and spatial proximity -- UPDATED covariates
proximity <- felm(
  death_rates_per_100_k ~ s_minus_i_z +
    population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+
    prop_asian +   prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  + fa_waiting_period + fa_cap + fa_min_age_21
  | GEOID + year | 0 | state,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(proximity)

socio_spatial_proximity <-  felm(
  death_rates_per_100_k ~ s_minus_i_z + d_minus_i_z +
    population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+ prop_asian +
    prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  + fa_waiting_period + fa_cap + fa_min_age_21
  | GEOID + year | 0 | state,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(socio_spatial_proximity)

## 1 ▸ Extract 95% CIs for the two peer-exposure terms
ci_df <- bind_rows(
  tidy(proximity,            conf.int = TRUE) |> mutate(model = "social"),
  tidy(socio_spatial_proximity, conf.int = TRUE) |> mutate(model = "socio-spatial")
) |>
  filter(term == "s_minus_i_z")

ggplot(ci_df,
       aes(x = estimate,
           y = factor(model, levels = c("socio-spatial", "social")),
           colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("social" = "red",
                                 "socio-spatial" = "blue"),
                      guide = "none") +
  scale_y_discrete(labels = c("socio-spatial" = "Model 2",
                              "social"         = "Model 1")) +
  labs(
    x = "Change in focal-county suicide mortality (per 100,000)\nfor a one-SD increase in socially proximal counties suicide rate",
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x  = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y  = element_text(size = 13)
  )

### creating the table for reression results relating to proximity -- UPDATED covariate.labels ###
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
    "spatial and temporal autocorrelation." ),
  column.labels = c("Social Proximity Only", "Socio-Spatial Proximity"),
  dep.var.labels = "Deaths per 100K",
  covariate.labels = c(
    "Deaths in social proximity $s_{-it}$",
    "Deaths in spatial proximity $d_{-it}$",
    "Population density",
    "Percent aged below 18",
    "Percent aged 18-44",
    "Percent aged 45-64",
    "Percent Asian",
    "Percent Black",
    "Percent Other",
    "Percent Hispanic",
    "Median household income ",
    "Percent with limited English proficiency",
    "Percent unemployed",
    "Percent with less than high school education",
    "COVID-19 deaths per 100k",
    "Drug-overdose deaths per 100k",
    "Alcohol-related deaths per 100k",
    "Poor mental health days",
    "Firearm: waiting period",
    "Firearm: child-access prevention",
    "Firearm: minimum purchase age 21"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "output_table.tex"
)

#### mixed with social and spatial exposure -- UPDATED covariates
did_spill_social_spatial_robustness <- felm(
  death_rates_per_100_k ~ ERPO_exposure +  InvDist_exposure+  s_minus_i_z +
    population_density +ACS_PCT_AGE_U18+ ACS_PCT_AGE_18_44
  + ACS_PCT_AGE_45_64+ prop_asian +
    prop_black +  prop_other + prop_hispanic +
    ACS_MEDIAN_HH_INC +
    ACS_PCT_ENGL_NOT_WELL +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  + covid_deaths_per_100k + overdose_deaths_per_100k + alcohol_deaths_per_100k
  + poor_mental_health_days
  | GEOID + state_year | 0 |state ,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)
summary(did_spill_social_spatial_robustness)

stargazer(
  did_spill_social_spatial_robustness,
  type = "latex",
  title = "Effect of Policy Exposure on Death Rates",
  column.labels = "Robustness Check controlling for $s_{-it}$",
  dep.var.labels = "Deaths per 100K",
  covariate.labels = c(
    "ERPO Social Exposure",
    "ERPO Spatial Exposure",
    "Deaths in social proximity $s_{-it}$",
    "Population density",
    "Percent aged below 18",
    "Percent aged 18-44",
    "Percent aged 45-64",
    "Percent Asian",
    "Percent Black",
    "Percent Other",
    "Percent Hispanic",
    "Median household income ",
    "Percent with limited English proficiency",
    "Percent unemployed",
    "Percent with less than high school education",
    "Political Affiliation",
    "COVID-19 deaths per 100k",
    "Drug-overdose deaths per 100k",
    "Alcohol-related deaths per 100k",
    "Poor mental health days"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "output_table.tex"
)

ci_df <- tidy(did_spill_social_spatial_robustness, conf.int = TRUE) |>
  filter(term == "ERPO_exposure") |>
  mutate(model = "socio-spatial")

ggplot(ci_df,
       aes(x = estimate,
           y = factor(model, levels = c("socio-spatial")),
           colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("socio-spatial" = "blue"),
                      guide = "none") +
  scale_y_discrete(labels = c("socio-spatial" = "indirect social network exposure\n(controls for spatial exposure &\ndeath rates in social proximity)")) +
  labs(
    x = "Change in focal-county suicide mortality (per 100,000)\nfor a 1-SD increase in socially proximal counties suicide rate",
    y = NULL
  ) +
  theme_classic(base_size = 12) +
  theme(
    axis.text.x  = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y  = element_text(size = 13)
  )