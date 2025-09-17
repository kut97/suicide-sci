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


suicide_codes <- c(sprintf("X%02d", 60:84), "Y87.0")

mort_list <- list(
  `2010` = mort_2010, `2011` = mort_2011, `2012` = mort_2012,
  `2013` = mort_2013, `2014` = mort_2014, `2015` = mort_2015,
  `2016` = mort_2016, `2017` = mort_2017, `2018` = mort_2018,
  `2019` = mort_2019, `2020` = mort_2020, `2021` = mort_2021,
  `2022` = mort_2022
)
# ================== Suicide UCOD codes ==================
suicide_codes <- c(sprintf("X%02d", 60:84), "Y87.0")

# ================== Age Recode 27 -> label ==================
map_age27_label <- function(a27) dplyr::case_when(
  a27 ==  1L ~ "Under 1 month",
  a27 ==  2L ~ "1–11 months",
  a27 ==  3L ~ "1 year",
  a27 ==  4L ~ "2 years",
  a27 ==  5L ~ "3 years",
  a27 ==  6L ~ "4 years",
  a27 ==  7L ~ "5–9 years",
  a27 ==  8L ~ "10–14 years",
  a27 ==  9L ~ "15–19 years",
  a27 == 10L ~ "20–24 years",
  a27 == 11L ~ "25–29 years",
  a27 == 12L ~ "30–34 years",
  a27 == 13L ~ "35–39 years",
  a27 == 14L ~ "40–44 years",
  a27 == 15L ~ "45–49 years",
  a27 == 16L ~ "50–54 years",
  a27 == 17L ~ "55–59 years",
  a27 == 18L ~ "60–64 years",
  a27 == 19L ~ "65–69 years",
  a27 == 20L ~ "70–74 years",
  a27 == 21L ~ "75–79 years",
  a27 == 22L ~ "80–84 years",
  a27 == 23L ~ "85–89 years",
  a27 == 24L ~ "90–94 years",
  a27 == 25L ~ "95–99 years",
  a27 == 26L ~ "100+ years",
  a27 == 27L ~ "Age not stated",
  TRUE       ~ NA_character_
)

age27_levels <- c(
  "Under 1 month","1–11 months","1 year","2 years","3 years","4 years",
  "5–9 years","10–14 years","15–19 years","20–24 years","25–29 years","30–34 years",
  "35–39 years","40–44 years","45–49 years","50–54 years","55–59 years","60–64 years",
  "65–69 years","70–74 years","75–79 years","80–84 years","85–89 years","90–94 years",
  "95–99 years","100+ years","Age not stated"
)

# ================== One-to-one state FIPS map (no joins) ==================
state_fips_lookup <- c(
  AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10", FL="12", GA="13",
  HI="15", ID="16", IL="17", IN="18", IA="19", KS="20", KY="21", LA="22", ME="23", MD="24",
  MA="25", MI="26", MN="27", MS="28", MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34",
  NM="35", NY="36", NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44", SC="45",
  SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53", WV="54", WI="55", WY="56",
  DC="11", PR="72", GU="66", VI="78", AS="60", MP="69"
)

# ================== GEOID builder (vectorized, no recycling) ==================
build_geoid <- function(state_abbr, county_code) {
  s <- toupper(str_trim(as.character(state_abbr)))
  c <- str_trim(as.character(county_code))
  
  county_fips <- if_else(str_detect(c, "^[0-9]+$"),
                         str_pad(c, width = 3, pad = "0"),
                         NA_character_)
  state_fips  <- unname(state_fips_lookup[s])  # NA if unknown abbr
  
  tibble::tibble(
    state_abbr = s,
    state_fips = state_fips,
    county_fips = county_fips
  ) %>%
    mutate(
      GEOID = if_else(!is.na(state_fips) & !is.na(county_fips) & county_fips != "000",
                      paste0(state_fips, county_fips),
                      NA_character_),
      geoid_issue = case_when(
        is.na(state_fips)        ~ "bad_state_abbr",
        is.na(county_fips)       ~ "non_numeric_county",
        county_fips == "000"     ~ "unknown_county",
        TRUE                     ~ NA_character_
      )
    )
}

# ================== Summarizer: year × GEOID × Age27 label ==================
summarise_suicides_by_age27_label <- function(mort_df, drop_not_stated = TRUE) {
  core <- mort_df %>%
    filter(ucod.icd.10 %in% suicide_codes) %>%
    transmute(
      year      = suppressWarnings(as.integer(data.year)),
      st_abbr   = as.character(state.residence),    # county of residence
      cnty_raw  = as.character(county.residence),
      age27     = readr::parse_integer(as.character(age.recode.27), trim_ws = TRUE),
      age_label = map_age27_label(age27)
    ) %>%
    filter(!is.na(age_label),
           age_label != "Age not stated" | !drop_not_stated)
  
  geo <- build_geoid(core$st_abbr, core$cnty_raw)     # same row count as 'core'
  
  # No bind_cols mismatch because 'geo' and 'core' have equal nrow
  bind_cols(core, geo) %>%
    filter(is.na(geoid_issue)) %>%                    # keep only valid GEOIDs
    mutate(age_label = factor(age_label, levels = age27_levels, ordered = TRUE)) %>%
    group_by(year, GEOID, age_label) %>%
    summarise(deaths = n(), .groups = "drop")
}

# ================== Apply to all years ==================
mort_list <- list(
  `2010` = mort_2010, `2011` = mort_2011, `2012` = mort_2012,
  `2013` = mort_2013, `2014` = mort_2014, `2015` = mort_2015,
  `2016` = mort_2016, `2017` = mort_2017, `2018` = mort_2018,
  `2019` = mort_2019, `2020` = mort_2020, `2021` = mort_2021,
  `2022` = mort_2022
)

suicide_by_age27 <- bind_rows(lapply(mort_list, summarise_suicides_by_age27_label))

# ================== Collapse to target age groups ==================
levels_out <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85+ years")

normalize_label <- function(x) {
  x %>% str_replace_all("[\u2013\u2014]", "-") %>% str_squish()
}

collapse_age27_to_target <- function(lbl) {
  z <- normalize_label(lbl)
  case_when(
    z %in% c("Under 1 month","1-11 months","1 year","2 years","3 years","4 years") ~ "0-4 years",
    z == "5-9 years"   ~ "5-9 years",
    z == "10-14 years" ~ "10-14 years",
    z == "15-19 years" ~ "15-19 years",
    z == "20-24 years" ~ "20-24 years",
    z == "25-29 years" ~ "25-29 years",
    z == "30-34 years" ~ "30-34 years",
    z == "35-39 years" ~ "35-39 years",
    z == "40-44 years" ~ "40-44 years",
    z == "45-49 years" ~ "45-49 years",
    z == "50-54 years" ~ "50-54 years",
    z == "55-59 years" ~ "55-59 years",
    str_detect(z, "^60-64\\b") ~ "60-64 years",  # trims stray spaces
    z == "65-69 years" ~ "65-69 years",
    z == "70-74 years" ~ "70-74 years",
    z == "75-79 years" ~ "75-79 years",
    z == "80-84 years" ~ "80-84 years",
    z %in% c("85-89 years","90-94 years","95-99 years","100+ years") ~ "85+ years",
    z == "Age not stated" | z == "" ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# Collapsed deaths by year × GEOID × target age group
suicide_by_age18 <- suicide_by_age27 %>%
  mutate(age_group = collapse_age27_to_target(as.character(age_label)),
         is_unknown = is.na(age_group)) %>%
  group_by(year, GEOID, age_group, is_unknown) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  filter(!is_unknown) %>%
  mutate(age_group = factor(age_group, levels = levels_out, ordered = TRUE)) %>%
  arrange(year, GEOID, age_group)

# ================== Diagnostics ==================
# (1) Unknown age excluded after collapse
suicide_age_unknown <- suicide_by_age27 %>%
  mutate(age_group = collapse_age27_to_target(as.character(age_label))) %>%
  filter(is.na(age_group)) %>%
  group_by(year, GEOID) %>%
  summarise(deaths_unknown_age = sum(deaths), .groups = "drop")

# (2) GEOID issues (how many records were dropped before grouping)
geoid_issues_yearly <- purrr::imap_dfr(mort_list, function(df, y) {
  core <- df %>%
    filter(ucod.icd.10 %in% suicide_codes) %>%
    transmute(st_abbr = as.character(state.residence),
              cnty_raw = as.character(county.residence))
  geo <- build_geoid(core$st_abbr, core$cnty_raw)
  geo %>%
    filter(!is.na(geoid_issue)) %>%
    count(geoid_issue, name = "n") %>%
    mutate(year = as.integer(y))
}) %>% arrange(year, desc(n))

# ===== Outputs =====
# Target order
levels_out <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85+ years")

normalize_age_label <- function(x) {
  x %>%
    str_replace_all("[\u2013\u2014]", "-") %>%      # en/em dash -> hyphen
    str_replace_all("\\s*years?$", " years") %>%    # "year"/"years" -> " years"
    str_squish()
}

map_pop_label_to_18 <- function(lbl) {
  z <- normalize_age_label(lbl)
  case_when(
    z %in% c("0-4 years","0-4")     ~ "0-4 years",
    z %in% c("5-9 years","5-9")     ~ "5-9 years",
    z %in% c("10-14 years","10-14") ~ "10-14 years",
    z %in% c("15-19 years","15-19") ~ "15-19 years",
    z %in% c("20-24 years","20-24") ~ "20-24 years",
    z %in% c("25-29 years","25-29") ~ "25-29 years",
    z %in% c("30-34 years","30-34") ~ "30-34 years",
    z %in% c("35-39 years","35-39") ~ "35-39 years",
    z %in% c("40-44 years","40-44") ~ "40-44 years",
    z %in% c("45-49 years","45-49") ~ "45-49 years",
    z %in% c("50-54 years","50-54") ~ "50-54 years",
    z %in% c("55-59 years","55-59") ~ "55-59 years",
    str_detect(z, "^60-64")         ~ "60-64 years",   # trims stray spaces
    z == "65-69 years"              ~ "65-69 years",
    z == "70-74 years"              ~ "70-74 years",
    z == "75-79 years"              ~ "75-79 years",
    z == "80-84 years"              ~ "80-84 years",
    z %in% c("85+ years","85+")     ~ "85+ years",
    TRUE                            ~ NA_character_
  )
}

collapse_population_to_18 <- function(pop_df) {
  # drop unnamed index column if present
  pop_df <- pop_df %>% select(-any_of("...1"))
  
  # stage with raw labels preserved
  tmp <- pop_df %>%
    transmute(
      year       = suppressWarnings(as.integer(.data$year)),
      GEOID      = str_pad(as.character(.data$GEOID), width = 5, pad = "0"),
      age_raw    = as.character(.data$Age.Group.Code),
      age_group  = map_pop_label_to_18(age_raw),
      Population = suppressWarnings(as.numeric(.data$Population))
    ) %>%
    filter(!is.na(year), !is.na(GEOID))
  
  # diagnostics BEFORE aggregation
  unmapped <- tmp %>%
    filter(is.na(age_group) | is.na(age_raw) | age_raw == "") %>%
    count(age_raw, name = "n_rows") %>%
    arrange(desc(n_rows))
  
  # aggregate mapped rows to 18 groups
  pop18 <- tmp %>%
    filter(!is.na(age_group)) %>%
    group_by(year, GEOID, age_group) %>%
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
    mutate(age_group = factor(age_group, levels = levels_out, ordered = TRUE)) %>%
    arrange(year, GEOID, age_group)
  
  list(pop18 = pop18, unmapped = unmapped)
}


# Read your file (adjust path if needed)
pop_raw <- read_csv("C:/Users/kusha/Downloads/age_group_population.csv",
                    col_types = cols(
                      GEOID = col_guess(),
                      Age.Group.Code = col_character(),
                      year = col_integer(),
                      Population = col_double(),
                      .default = col_guess()
                    ))

res   <- collapse_population_to_18(pop_raw)
pop18 <- res$pop18
unmapped_labels <- res$unmapped  # should be empty; if not, share it and we’ll extend the mapper


# Expand deaths to a complete panel with structural zeros
panel18 <- pop18 %>%
  select(year, GEOID, age_group) %>%
  left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  mutate(deaths = tidyr::replace_na(deaths, 0L))


panel18 <- pop18 %>%
  dplyr::select(year, GEOID, age_group) %>%
  dplyr::left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  dplyr::mutate(deaths = tidyr::replace_na(deaths, 0L)) %>%
  dplyr::mutate(age_group = factor(age_group, levels = levels(suicide_by_age18$age_group), ordered = TRUE))

# Now 'panel18' has all age groups for every county–year, with 0 where no deaths occurred.

# ================== Collapse to target age groups ==================
levels_out <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85+ years")

normalize_label <- function(x) {
  x %>% str_replace_all("[\u2013\u2014]", "-") %>% str_squish()
}

collapse_age27_to_target <- function(lbl) {
  z <- normalize_label(lbl)
  case_when(
    z %in% c("Under 1 month","1-11 months","1 year","2 years","3 years","4 years") ~ "0-4 years",
    z == "5-9 years"   ~ "5-9 years",
    z == "10-14 years" ~ "10-14 years",
    z == "15-19 years" ~ "15-19 years",
    z == "20-24 years" ~ "20-24 years",
    z == "25-29 years" ~ "25-29 years",
    z == "30-34 years" ~ "30-34 years",
    z == "35-39 years" ~ "35-39 years",
    z == "40-44 years" ~ "40-44 years",
    z == "45-49 years" ~ "45-49 years",
    z == "50-54 years" ~ "50-54 years",
    z == "55-59 years" ~ "55-59 years",
    str_detect(z, "^60-64\\b") ~ "60-64 years",  # trims stray spaces
    z == "65-69 years" ~ "65-69 years",
    z == "70-74 years" ~ "70-74 years",
    z == "75-79 years" ~ "75-79 years",
    z == "80-84 years" ~ "80-84 years",
    z %in% c("85-89 years","90-94 years","95-99 years","100+ years") ~ "85+ years",
    z == "Age not stated" | z == "" ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# Collapsed deaths by year × GEOID × target age group
suicide_by_age18 <- suicide_by_age27 %>%
  mutate(age_group = collapse_age27_to_target(as.character(age_label)),
         is_unknown = is.na(age_group)) %>%
  group_by(year, GEOID, age_group, is_unknown) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  filter(!is_unknown) %>%
  mutate(age_group = factor(age_group, levels = levels_out, ordered = TRUE)) %>%
  arrange(year, GEOID, age_group)

# ================== Diagnostics ==================
# (1) Unknown age excluded after collapse
suicide_age_unknown <- suicide_by_age27 %>%
  mutate(age_group = collapse_age27_to_target(as.character(age_label))) %>%
  filter(is.na(age_group)) %>%
  group_by(year, GEOID) %>%
  summarise(deaths_unknown_age = sum(deaths), .groups = "drop")

# (2) GEOID issues (how many records were dropped before grouping)
geoid_issues_yearly <- purrr::imap_dfr(mort_list, function(df, y) {
  core <- df %>%
    filter(ucod.icd.10 %in% suicide_codes) %>%
    transmute(st_abbr = as.character(state.residence),
              cnty_raw = as.character(county.residence))
  geo <- build_geoid(core$st_abbr, core$cnty_raw)
  geo %>%
    filter(!is.na(geoid_issue)) %>%
    count(geoid_issue, name = "n") %>%
    mutate(year = as.integer(y))
}) %>% arrange(year, desc(n))

# ===== Outputs =====
# Target order
levels_out <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85+ years")

normalize_age_label <- function(x) {
  x %>%
    str_replace_all("[\u2013\u2014]", "-") %>%      # en/em dash -> hyphen
    str_replace_all("\\s*years?$", " years") %>%    # "year"/"years" -> " years"
    str_squish()
}

map_pop_label_to_18 <- function(lbl) {
  z <- normalize_age_label(lbl)
  case_when(
    z %in% c("0-4 years","0-4")     ~ "0-4 years",
    z %in% c("5-9 years","5-9")     ~ "5-9 years",
    z %in% c("10-14 years","10-14") ~ "10-14 years",
    z %in% c("15-19 years","15-19") ~ "15-19 years",
    z %in% c("20-24 years","20-24") ~ "20-24 years",
    z %in% c("25-29 years","25-29") ~ "25-29 years",
    z %in% c("30-34 years","30-34") ~ "30-34 years",
    z %in% c("35-39 years","35-39") ~ "35-39 years",
    z %in% c("40-44 years","40-44") ~ "40-44 years",
    z %in% c("45-49 years","45-49") ~ "45-49 years",
    z %in% c("50-54 years","50-54") ~ "50-54 years",
    z %in% c("55-59 years","55-59") ~ "55-59 years",
    str_detect(z, "^60-64")         ~ "60-64 years",   # trims stray spaces
    z == "65-69 years"              ~ "65-69 years",
    z == "70-74 years"              ~ "70-74 years",
    z == "75-79 years"              ~ "75-79 years",
    z == "80-84 years"              ~ "80-84 years",
    z %in% c("85+ years","85+")     ~ "85+ years",
    TRUE                            ~ NA_character_
  )
}

collapse_population_to_18 <- function(pop_df) {
  # drop unnamed index column if present
  pop_df <- pop_df %>% select(-any_of("...1"))
  
  # stage with raw labels preserved
  tmp <- pop_df %>%
    transmute(
      year       = suppressWarnings(as.integer(.data$year)),
      GEOID      = str_pad(as.character(.data$GEOID), width = 5, pad = "0"),
      age_raw    = as.character(.data$Age.Group.Code),
      age_group  = map_pop_label_to_18(age_raw),
      Population = suppressWarnings(as.numeric(.data$Population))
    ) %>%
    filter(!is.na(year), !is.na(GEOID))
  
  # diagnostics BEFORE aggregation
  unmapped <- tmp %>%
    filter(is.na(age_group) | is.na(age_raw) | age_raw == "") %>%
    count(age_raw, name = "n_rows") %>%
    arrange(desc(n_rows))
  
  # aggregate mapped rows to 18 groups
  pop18 <- tmp %>%
    filter(!is.na(age_group)) %>%
    group_by(year, GEOID, age_group) %>%
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
    mutate(age_group = factor(age_group, levels = levels_out, ordered = TRUE)) %>%
    arrange(year, GEOID, age_group)
  
  list(pop18 = pop18, unmapped = unmapped)
}


# Read your file (adjust path if needed)
pop_raw <- read_csv("C:/Users/kusha/Downloads/age_group_population.csv",
                    col_types = cols(
                      GEOID = col_guess(),
                      Age.Group.Code = col_character(),
                      year = col_integer(),
                      Population = col_double(),
                      .default = col_guess()
                    ))

res   <- collapse_population_to_18(pop_raw)
pop18 <- res$pop18
unmapped_labels <- res$unmapped  # should be empty; if not, share it and we’ll extend the mapper


# Expand deaths to a complete panel with structural zeros
panel18 <- pop18 %>%
  select(year, GEOID, age_group) %>%
  left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  mutate(deaths = tidyr::replace_na(deaths, 0L))


panel18 <- pop18 %>%
  dplyr::select(year, GEOID, age_group) %>%
  dplyr::left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  dplyr::mutate(deaths = tidyr::replace_na(deaths, 0L)) %>%
  dplyr::mutate(age_group = factor(age_group, levels = levels(suicide_by_age18$age_group), ordered = TRUE))

# Now 'panel18' has all age groups for every county–year, with 0 where no deaths occurred.

# ================== Collapse to target age groups ==================
levels_out <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85+ years")

normalize_label <- function(x) {
  x %>% str_replace_all("[\u2013\u2014]", "-") %>% str_squish()
}

collapse_age27_to_target <- function(lbl) {
  z <- normalize_label(lbl)
  case_when(
    z %in% c("Under 1 month","1-11 months","1 year","2 years","3 years","4 years") ~ "0-4 years",
    z == "5-9 years"   ~ "5-9 years",
    z == "10-14 years" ~ "10-14 years",
    z == "15-19 years" ~ "15-19 years",
    z == "20-24 years" ~ "20-24 years",
    z == "25-29 years" ~ "25-29 years",
    z == "30-34 years" ~ "30-34 years",
    z == "35-39 years" ~ "35-39 years",
    z == "40-44 years" ~ "40-44 years",
    z == "45-49 years" ~ "45-49 years",
    z == "50-54 years" ~ "50-54 years",
    z == "55-59 years" ~ "55-59 years",
    str_detect(z, "^60-64\\b") ~ "60-64 years",  # trims stray spaces
    z == "65-69 years" ~ "65-69 years",
    z == "70-74 years" ~ "70-74 years",
    z == "75-79 years" ~ "75-79 years",
    z == "80-84 years" ~ "80-84 years",
    z %in% c("85-89 years","90-94 years","95-99 years","100+ years") ~ "85+ years",
    z == "Age not stated" | z == "" ~ NA_character_,
    TRUE ~ NA_character_
  )
}

# Collapsed deaths by year × GEOID × target age group
suicide_by_age18 <- suicide_by_age27 %>%
  mutate(age_group = collapse_age27_to_target(as.character(age_label)),
         is_unknown = is.na(age_group)) %>%
  group_by(year, GEOID, age_group, is_unknown) %>%
  summarise(deaths = sum(deaths, na.rm = TRUE), .groups = "drop") %>%
  filter(!is_unknown) %>%
  mutate(age_group = factor(age_group, levels = levels_out, ordered = TRUE)) %>%
  arrange(year, GEOID, age_group)

# ================== Diagnostics ==================
# (1) Unknown age excluded after collapse
suicide_age_unknown <- suicide_by_age27 %>%
  mutate(age_group = collapse_age27_to_target(as.character(age_label))) %>%
  filter(is.na(age_group)) %>%
  group_by(year, GEOID) %>%
  summarise(deaths_unknown_age = sum(deaths), .groups = "drop")

# (2) GEOID issues (how many records were dropped before grouping)
geoid_issues_yearly <- purrr::imap_dfr(mort_list, function(df, y) {
  core <- df %>%
    filter(ucod.icd.10 %in% suicide_codes) %>%
    transmute(st_abbr = as.character(state.residence),
              cnty_raw = as.character(county.residence))
  geo <- build_geoid(core$st_abbr, core$cnty_raw)
  geo %>%
    filter(!is.na(geoid_issue)) %>%
    count(geoid_issue, name = "n") %>%
    mutate(year = as.integer(y))
}) %>% arrange(year, desc(n))

# ===== Outputs =====
# Target order
levels_out <- c("0-4 years","5-9 years","10-14 years","15-19 years","20-24 years",
                "25-29 years","30-34 years","35-39 years","40-44 years","45-49 years",
                "50-54 years","55-59 years","60-64 years","65-69 years","70-74 years",
                "75-79 years","80-84 years","85+ years")

normalize_age_label <- function(x) {
  x %>%
    str_replace_all("[\u2013\u2014]", "-") %>%      # en/em dash -> hyphen
    str_replace_all("\\s*years?$", " years") %>%    # "year"/"years" -> " years"
    str_squish()
}

map_pop_label_to_18 <- function(lbl) {
  z <- normalize_age_label(lbl)
  case_when(
    z %in% c("0-4 years","0-4")     ~ "0-4 years",
    z %in% c("5-9 years","5-9")     ~ "5-9 years",
    z %in% c("10-14 years","10-14") ~ "10-14 years",
    z %in% c("15-19 years","15-19") ~ "15-19 years",
    z %in% c("20-24 years","20-24") ~ "20-24 years",
    z %in% c("25-29 years","25-29") ~ "25-29 years",
    z %in% c("30-34 years","30-34") ~ "30-34 years",
    z %in% c("35-39 years","35-39") ~ "35-39 years",
    z %in% c("40-44 years","40-44") ~ "40-44 years",
    z %in% c("45-49 years","45-49") ~ "45-49 years",
    z %in% c("50-54 years","50-54") ~ "50-54 years",
    z %in% c("55-59 years","55-59") ~ "55-59 years",
    str_detect(z, "^60-64")         ~ "60-64 years",   # trims stray spaces
    z == "65-69 years"              ~ "65-69 years",
    z == "70-74 years"              ~ "70-74 years",
    z == "75-79 years"              ~ "75-79 years",
    z == "80-84 years"              ~ "80-84 years",
    z %in% c("85+ years","85+")     ~ "85+ years",
    TRUE                            ~ NA_character_
  )
}

collapse_population_to_18 <- function(pop_df) {
  # drop unnamed index column if present
  pop_df <- pop_df %>% select(-any_of("...1"))
  
  # stage with raw labels preserved
  tmp <- pop_df %>%
    transmute(
      year       = suppressWarnings(as.integer(.data$year)),
      GEOID      = str_pad(as.character(.data$GEOID), width = 5, pad = "0"),
      age_raw    = as.character(.data$Age.Group.Code),
      age_group  = map_pop_label_to_18(age_raw),
      Population = suppressWarnings(as.numeric(.data$Population))
    ) %>%
    filter(!is.na(year), !is.na(GEOID))
  
  # diagnostics BEFORE aggregation
  unmapped <- tmp %>%
    filter(is.na(age_group) | is.na(age_raw) | age_raw == "") %>%
    count(age_raw, name = "n_rows") %>%
    arrange(desc(n_rows))
  
  # aggregate mapped rows to 18 groups
  pop18 <- tmp %>%
    filter(!is.na(age_group)) %>%
    group_by(year, GEOID, age_group) %>%
    summarise(Population = sum(Population, na.rm = TRUE), .groups = "drop") %>%
    mutate(age_group = factor(age_group, levels = levels_out, ordered = TRUE)) %>%
    arrange(year, GEOID, age_group)
  
  list(pop18 = pop18, unmapped = unmapped)
}


# Read your file (adjust path if needed)
pop_raw <- read_csv("C:/Users/kusha/Downloads/age_group_population.csv",
                    col_types = cols(
                      GEOID = col_guess(),
                      Age.Group.Code = col_character(),
                      year = col_integer(),
                      Population = col_double(),
                      .default = col_guess()
                    ))

res   <- collapse_population_to_18(pop_raw)
pop18 <- res$pop18
unmapped_labels <- res$unmapped  # should be empty; if not, share it and we’ll extend the mapper


# Expand deaths to a complete panel with structural zeros
panel18 <- pop18 %>%
  select(year, GEOID, age_group) %>%
  left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  mutate(deaths = tidyr::replace_na(deaths, 0L))


panel18 <- pop18 %>%
  dplyr::select(year, GEOID, age_group) %>%
  dplyr::left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  dplyr::mutate(deaths = tidyr::replace_na(deaths, 0L)) %>%
  dplyr::mutate(age_group = factor(age_group, levels = levels(suicide_by_age18$age_group), ordered = TRUE))

# Now 'panel18' has all age groups for every county–year, with 0 where no deaths occurred.

# 0) Rebuild panel18 so it includes Population (overwrite previous panel18)
panel18 <- pop18 %>%
  left_join(suicide_by_age18, by = c("year","GEOID","age_group")) %>%
  mutate(deaths = replace_na(deaths, 0L)) %>%
  arrange(year, GEOID, age_group)

# 1) Age-specific rates per 100,000
panel18 <- panel18 %>%
  mutate(
    asr        = if_else(Population > 0, deaths / Population, NA_real_),   # per person
    asr_100k   = 1e5 * asr
  )

# 2) Internal standard weights (sum to 1 across the 18 groups)
std_wts18 <- pop18 %>%
  group_by(age_group) %>%
  summarise(w = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  mutate(w = w / sum(w)) %>%
  arrange(age_group)

# 3) Direct age-adjusted rate per 100k (Wald CI)
#    AAR = 1e5 * sum_i w_i * (d_i / p_i)
#    Var(AAR) = 1e10 * sum_i w_i^2 * d_i / p_i^2
rates_aar <- panel18 %>%
  left_join(std_wts18, by = "age_group") %>%
  group_by(year, GEOID) %>%
  summarise(
    aar_100k = 1e5 * sum(if_else(Population > 0, w * (deaths / Population), 0), na.rm = TRUE),
    var_aar  = 1e10 * sum(if_else(Population > 0, w^2 * (deaths / Population^2), 0), na.rm = TRUE),
    se_aar   = sqrt(var_aar),
    lcl_95   = aar_100k - 1.96 * se_aar,
    ucl_95   = aar_100k + 1.96 * se_aar,
    deaths   = sum(deaths, na.rm = TRUE),                     # total deaths (all ages)
    pop_tot  = sum(Population, na.rm = TRUE),                 # total population (all ages)
    crude_100k = if_else(pop_tot > 0, 1e5 * deaths / pop_tot, NA_real_),
    .groups = "drop"
  ) %>%
  arrange(year, GEOID)

# 4) Optional: keep age-specific table too
age_specific <- panel18 %>%
  select(year, GEOID, age_group, deaths, Population, asr_100k)

# 5) Quick diagnostics
# a) No missing Population?
stopifnot(!any(is.na(panel18$Population)))
# b) Any counties with all-zero population across age groups? (should be none)
panel18 %>%
  group_by(year, GEOID) %>%
  summarise(pop_tot = sum(Population, na.rm = TRUE), .groups = "drop") %>%
  filter(pop_tot == 0) %>% nrow() -> n_zero_pop
if (n_zero_pop > 0) warning(sprintf("%d county-years have zero total population.", n_zero_pop))

# 6) (Optional) save

# Minimal table (AAR only)
aar_min <- rates_aar %>%
  dplyr::transmute(
    GEOID,
    year,
    age_adjusted_rate_per_100k = aar_100k
  )

# Standard reporting table (AAR + CI + totals)
aar_full <- rates_aar %>%
  dplyr::transmute(
    GEOID,
    year,
    population_total = pop_tot,            # optional context
    deaths_total     = deaths,             # optional context
    crude_rate_per_100k = crude_100k,      # optional context
    age_adjusted_rate_per_100k = aar_100k,
    lcl_95,
    ucl_95
  )

# reading the nvss mortality file ##
my_data_with_spatial_g <- read_csv(
  "C:/Users/kusha/Downloads/df_mortality_nvss_2010_2022.csv",
  col_types = cols(GEOID = col_character())
)
my_data_with_spatial_g <- my_data_with_spatial_g[,-1]

# 1) Robust state lookup (no joins)
state_fips_lookup <- c(
  AL="01", AK="02", AZ="04", AR="05", CA="06", CO="08", CT="09", DE="10", FL="12", GA="13",
  HI="15", ID="16", IL="17", IN="18", IA="19", KS="20", KY="21", LA="22", ME="23", MD="24",
  MA="25", MI="26", MN="27", MS="28", MO="29", MT="30", NE="31", NV="32", NH="33", NJ="34",
  NM="35", NY="36", NC="37", ND="38", OH="39", OK="40", OR="41", PA="42", RI="44", SC="45",
  SD="46", TN="47", TX="48", UT="49", VT="50", VA="51", WA="53", WV="54", WI="55", WY="56",
  DC="11", PR="72", GU="66", VI="78", AS="60", MP="69"
)
state_name_lookup <- c(
  setNames(state.name, state.abb),
  DC="District of Columbia", PR="Puerto Rico", GU="Guam",
  VI="U.S. Virgin Islands", AS="American Samoa", MP="Northern Mariana Islands"
)
abbr_from_fips <- setNames(names(state_fips_lookup), unname(state_fips_lookup))
name_from_abbr <- state_name_lookup

# 2) Standardize `aar_full` and add state fields
aar_full <- aar_full %>%
  mutate(
    GEOID      = str_pad(as.character(GEOID), 5, pad = "0"),
    year       = as.integer(year),
    state_fips = substr(GEOID, 1, 2),
    state_abbr = unname(abbr_from_fips[state_fips]),
    state_name = unname(name_from_abbr[state_abbr]),
    state      = state_abbr
  ) %>%
  select(GEOID, state_fips, state_abbr, state_name, state, year,
         population_total, deaths_total, crude_rate_per_100k,
         age_adjusted_rate_per_100k, lcl_95, ucl_95)

stopifnot(!any(is.na(aar_full$state_abbr)))

# 3) Reference county-year set from your file
stopifnot(exists("my_data_with_spatial_g"))
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, pad = "0"),
         year  = as.integer(year))
years <- sort(unique(my_data_with_spatial_g$year))

ref_geoids_by_year <- my_data_with_spatial_g %>%
  filter(!is.na(GEOID), !is.na(year)) %>%
  group_by(year) %>%
  summarise(ref_geoids = list(unique(GEOID)), .groups = "drop")

exclude_geoids <- c("02158","02261","46102","15005")

# 4) COMPLETE reference panel, attach AAR (no duplicate state cols; fill state if AAR is NA)
complete_aar_panel_ref <- map_dfr(years, function(y) {
  geoids_ref <- ref_geoids_by_year %>% filter(year == y) %>% pull(ref_geoids)
  geoids_ref <- if (length(geoids_ref) == 1L) geoids_ref[[1]] else character(0)
  
  tibble(GEOID = geoids_ref, year = y) %>%
    mutate(county_fips = substr(GEOID, 3, 5)) %>%
    left_join(
      aar_full %>% select(GEOID, year,
                          state_fips, state_abbr, state_name, state,
                          population_total, deaths_total, crude_rate_per_100k,
                          age_adjusted_rate_per_100k, lcl_95, ucl_95),
      by = c("GEOID","year")
    ) %>%
    mutate(
      # fill state fields for rows missing AAR
      state_fips = if_else(is.na(state_fips), substr(GEOID, 1, 2), state_fips),
      state_abbr = if_else(is.na(state_abbr),
                           unname(abbr_from_fips[state_fips]), state_abbr),
      state_name = if_else(is.na(state_name),
                           unname(name_from_abbr[state_abbr]), state_name),
      state      = if_else(is.na(state), state_abbr, state)
    )
}) %>%
  filter(!GEOID %in% exclude_geoids)

# 5) QA
panel_size_check <- complete_aar_panel_ref %>%
  group_by(year) %>% summarise(n_counties = n(), .groups = "drop")

missing_in_aar <- complete_aar_panel_ref %>%
  filter(is.na(age_adjusted_rate_per_100k)) %>%
  select(year, state_abbr, GEOID)

extra_in_aar <- aar_full %>%
  anti_join(my_data_with_spatial_g %>% distinct(GEOID, year),
            by = c("GEOID","year")) %>%
  select(year, GEOID)

# 6) Final cleaned (valid AAR only)
aar_clean <- complete_aar_panel_ref %>%
  filter(!is.na(age_adjusted_rate_per_100k)) %>%
  arrange(year, GEOID)

# 7) Merge covariates
covars <- c(
  "ACS_TOT_POP_WT","prop_black","prop_asian","prop_other","prop_hispanic",
  "ACS_MEDIAN_HH_INC","ACS_PCT_UNEMPLOY","ACS_PCT_LT_HS","ACS_PCT_AGE_18_44",
  "ACS_PCT_AGE_45_64","ACS_PCT_AGE_U18","ACS_PCT_AGE_ABOVE65","ACS_PCT_ENGL_NOT_WELL",
  "Longitude","Latitude","area_sq_km","population_density"
)

md <- my_data_with_spatial_g %>%
  mutate(GEOID = str_pad(as.character(GEOID), 5, pad = "0"),
         year  = as.integer(year))

dups <- md %>% count(GEOID, year, name = "n") %>% filter(n > 1)
if (nrow(dups) > 0) {
  warning(sprintf("Found %d duplicate GEOID–year rows in my_data_with_spatial_g; aggregating.", nrow(dups)))
  exposures <- md %>%
    group_by(GEOID, year) %>%
    summarise(
      across(all_of(covars),
             ~ if (is.numeric(.)) mean(., na.rm = TRUE) else dplyr::first(.)),
      .groups = "drop"
    )
} else {
  exposures <- md %>% select(GEOID, year, any_of(covars))
}

aar_clean <- aar_clean %>%
  left_join(exposures, by = c("GEOID","year")) %>%
  relocate(GEOID, state_fips, state_abbr, state_name, state, year)

# 8) Optional: % missing by covariate
missing_summary <- aar_clean %>%
  summarise(across(all_of(covars), ~ mean(is.na(.)) * 100))

### to calculate exposure ###
my_data_with_spatial_g <- aar_clean
### calculating exposures ####
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
                "population_density","ACS_PCT_AGE_U18", "ACS_PCT_AGE_18_44", "ACS_PCT_AGE_45_64", "ACS_PCT_AGE_ABOVE65",
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

# Political Affiliation Integration for Suicide Analysis (2010-2022)

# Function to process the 2008-2016 election data
process_election_data_0816 <- function(file_path) {
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat("Warning: File not found:", file_path, "\n")
    return(tibble())
  }
  
  # Read the 2008-2016 data
  election_0816 <- read_csv(file_path, show_col_types = FALSE)
  
  # Process each election year
  political_results <- tibble()
  
  # 2008 Election
  election_2008 <- election_0816 %>%
    select(fips_code, dem_2008, gop_2008) %>%
    filter(!is.na(fips_code), !is.na(dem_2008), !is.na(gop_2008)) %>%
    mutate(
      GEOID = str_pad(as.character(fips_code), width = 5, pad = "0"),
      political_affiliation = as.integer(gop_2008 > dem_2008),
      election_year = 2008
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  # 2012 Election
  election_2012 <- election_0816 %>%
    select(fips_code, dem_2012, gop_2012) %>%
    filter(!is.na(fips_code), !is.na(dem_2012), !is.na(gop_2012)) %>%
    mutate(
      GEOID = str_pad(as.character(fips_code), width = 5, pad = "0"),
      political_affiliation = as.integer(gop_2012 > dem_2012),
      election_year = 2012
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  # 2016 Election
  election_2016 <- election_0816 %>%
    select(fips_code, dem_2016, gop_2016) %>%
    filter(!is.na(fips_code), !is.na(dem_2016), !is.na(gop_2016)) %>%
    mutate(
      GEOID = str_pad(as.character(fips_code), width = 5, pad = "0"),
      political_affiliation = as.integer(gop_2016 > dem_2016),
      election_year = 2016
    ) %>%
    select(GEOID, political_affiliation, election_year)
  
  # Combine all years
  political_results <- bind_rows(election_2008, election_2012, election_2016)
  
  return(political_results)
}

# Function to process the 2020 election data
process_election_data_2020 <- function(file_path) {
  
  # Check if file exists
  if (!file.exists(file_path)) {
    cat("Warning: File not found:", file_path, "\n")
    return(tibble())
  }
  
  # Read the 2020 data
  election_2020 <- read_csv(file_path, show_col_types = FALSE)
  
  # Process 2020 election
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

# Process both election datasets
cat("Processing 2008-2016 election data...\n")
political_0816 <- process_election_data_0816("C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_08-16.csv")

cat("Processing 2020 election data...\n") 
political_2020 <- process_election_data_2020("C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_2020.csv")

# Combine all election results
all_political_results <- bind_rows(political_0816, political_2020)

# Check if we have any political data
if (nrow(all_political_results) == 0) {
  cat("ERROR: No political data loaded. Check file paths:\n")
  cat("- C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_08-16.csv\n")
  cat("- C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_2020.csv\n")
  cat("Skipping political affiliation analysis...\n")
} else {
  # Handle Shannon County rename (FIPS 46113 -> 46102)
  all_political_results <- all_political_results %>%
    mutate(GEOID = ifelse(GEOID == "46113", "46102", GEOID))
  
  # Display summary of available election data
  cat("\nElection data summary:\n")
  all_political_results %>%
    group_by(election_year) %>%
    summarise(
      counties = n(),
      pct_republican = round(mean(political_affiliation, na.rm = TRUE) * 100, 1),
      .groups = "drop"
    ) %>%
    print()
  
  # Create the assignment logic for analysis years (2010-2022)
  assign_political_affiliation <- function() {
    
    # Create the mapping based on your logic
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
    
    # Create the political affiliation dataset for all analysis years
    political_panel <- year_assignments %>%
      left_join(all_political_results, by = "election_year") %>%
      select(GEOID, analysis_year, political_affiliation) %>%
      rename(year = analysis_year) %>%
      filter(!is.na(GEOID), !is.na(political_affiliation))
    
    return(political_panel)
  }
  
  # Create the political affiliation panel
  political_panel <- assign_political_affiliation()
  
  # Merge with your main dataset and identify missing counties
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
    
    # Show missing counties
    if (length(missing_counties) > 0) {
      cat("\nMissing counties (GEOID):\n")
      print(missing_counties)
      
      # Get details about missing counties from your main dataset
      missing_county_details <- my_data_with_spatial_g %>%
        filter(GEOID %in% missing_counties) %>%
        select(GEOID, state, county_fips, state_fips) %>%
        distinct() %>%
        arrange(state, GEOID)
      
      cat("\nMissing counties details:\n")
      print(missing_county_details)
    }
    
    # Merge with your main dataset
    cat("\nMerging political affiliation into main dataset...\n")
    
    my_data_with_spatial_g <- my_data_with_spatial_g %>%
      left_join(
        political_panel %>% select(GEOID, year, political_affiliation),
        by = c("GEOID", "year")
      )
    
    # Check if political_affiliation column was created successfully
    if (!"political_affiliation" %in% names(my_data_with_spatial_g)) {
      cat("ERROR: political_affiliation column not created properly.\n")
    } else {
      # Count successful merges and identify missing observations
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
      
      # Detailed analysis of missing observations
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
        
        # Summary by state
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
      
      # Add political_affiliation to covariates if it exists
      if (exists("covariates")) {
        covariates_updated <- c(covariates, "political_affiliation")
        assign("covariates", covariates_updated, envir = .GlobalEnv)
        cat("\nUpdated covariates list includes: political_affiliation\n")
      }
      
      # Apply Alaska county imputation based on actual election data analysis
      if (missing_observations > 0) {
        cat("\nApplying Alaska county imputation...\n")
        
        # Alaska imputation based on actual election data from countypres_20002024.csv
        alaska_imputation <- tribble(
          ~GEOID, ~year, ~political_affiliation, ~source,
          
          # DISTRICT 13 (02013) - Consistently Republican based on actual election data
          "02013", 2010, 1, "actual_election",  "02013", 2011, 1, "actual_election",
          "02013", 2012, 1, "actual_election",  "02013", 2013, 1, "actual_election",
          "02013", 2014, 1, "actual_election",  "02013", 2015, 1, "actual_election",
          "02013", 2016, 1, "actual_election",  "02013", 2017, 1, "actual_election",
          "02013", 2018, 1, "actual_election",  "02013", 2019, 1, "actual_election",
          "02013", 2020, 1, "actual_election",  "02013", 2021, 1, "actual_election",
          "02013", 2022, 1, "actual_election",
          
          # DISTRICT 16 (02016) - Rep 2008, Dem 2012-2020 based on actual election data
          "02016", 2010, 1, "actual_election",  "02016", 2011, 1, "actual_election",
          "02016", 2012, 0, "actual_election",  "02016", 2013, 0, "actual_election",
          "02016", 2014, 0, "actual_election",  "02016", 2015, 0, "actual_election",
          "02016", 2016, 0, "actual_election",  "02016", 2017, 0, "actual_election",
          "02016", 2018, 0, "actual_election",  "02016", 2019, 0, "actual_election",
          "02016", 2020, 0, "actual_election",  "02016", 2021, 0, "actual_election",
          "02016", 2022, 0, "actual_election",
          
          # DISTRICT 20 (02020) - Rep 2008-2012, Dem 2016-2020 based on actual election data
          "02020", 2010, 1, "actual_election",  "02020", 2011, 1, "actual_election",
          "02020", 2012, 1, "actual_election",  "02020", 2013, 1, "actual_election",
          "02020", 2014, 1, "actual_election",  "02020", 2015, 1, "actual_election",
          "02020", 2016, 0, "actual_election",  "02020", 2017, 0, "actual_election",
          "02020", 2018, 0, "actual_election",  "02020", 2019, 0, "actual_election",
          "02020", 2020, 0, "actual_election",  "02020", 2021, 0, "actual_election",
          "02020", 2022, 0, "actual_election"
        ) %>%
          # Add remaining Alaska counties using Republican pattern
          bind_rows(
            expand_grid(
              GEOID = c("02050", "02060", "02068", "02070", "02090", "02100", "02105", 
                        "02110", "02122", "02130", "02150", "02164", "02170", "02180", 
                        "02185", "02188", "02195", "02198", "02220", "02230", "02240", 
                        "02275", "02282", "02290"),
              year = 2010:2022
            ) %>%
              mutate(
                political_affiliation = 1,  # Republican
                source = "alaska_pattern"
              )
          )
        
        # Apply Alaska imputation to missing observations
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
        
        # Final check
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




### estimating the effects  ###

did_no_spill <- felm(
  age_adjusted_rate_per_100k ~ D_it+ population_density +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  | GEOID + year        # Fixed Effects
  | 0                    # No instrumental variables
  | state,               # Cluster by state
  data =my_data_with_spatial_g,
  weights = my_data_with_spatial_g$population_total
)
summary(did_no_spill)

### spatial spillover

did_spill_spatial <- felm(
  age_adjusted_rate_per_100k ~ InvDist_exposure+
    population_density + 
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  | GEOID + state_year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$population_total
)

summary(did_spill_spatial)

### social spillover ###
did_spill_social <- felm(
  age_adjusted_rate_per_100k ~  ERPO_exposure +
    population_density + 
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  | GEOID + state_year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$population_total
)

summary(did_spill_social)

### social-spatial exposure ###

did_spill_social_spatial <- felm(
  age_adjusted_rate_per_100k~ ERPO_exposure +  InvDist_exposure + population_density +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS + political_affiliation
  | GEOID + state_year | 0 |state ,
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(did_spill_social_spatial)

### creating plots ### 

## 2 ▸ Dot-whisker plot matching the mock-up
ci_df <- bind_rows(
  tidy(did_spill_social,            conf.int = TRUE) |> mutate(model = "indirect social network exposure"),
  tidy(did_spill_social_spatial, conf.int = TRUE) |> mutate(model = "indirect social network exposure (controls for Spatial Exposure)")
) |>
  filter(term == "ERPO_exposure")         # keep only the peer-exposure coefficient


## 2 ▸ Dot-whisker plot matching the mock-up
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
  ))  +
  
  ## axis labels
  labs(x = "Change in focal-county age-adjusted suicide mortality (per 100,000)\nfor a 1-SD increase in ERPO social exposure", y = NULL) +
  
  ## styling
  theme_classic(base_size = 12) +
  theme(
    panel.grid.major.y = element_line(colour = "grey85"),
    axis.ticks.y      = element_blank()
  )
### getting the stargrazer table ###

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
    "Population density",
    "Percent Asian",
    "Percent Black",
    "Percent Other",
    "Percent Hispanic",
    "Median household income ",
    "Percent with limited English proficiency",
    "Percent unemployed",
    "Percent with less than high school education",
    "Political Affiliation"
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
social_df <- my_data_with_spatial_g[, c("GEOID", "population_total", "age_adjusted_rate_per_100k")]
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
population <- my_data_with_spatial_g %>%
  group_by(GEOID) %>%
  summarise(population = round(mean(population_total, na.rm = TRUE))) %>%
  filter(GEOID %in% nodes$fr_loc) %>%
  arrange(match(GEOID, nodes$fr_loc))  # Ensure order matches the adjacency matrix

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
  y_t <- my_data_with_spatial_g[year == yr][order(idx), age_adjusted_rate_per_100k]
  
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
  age_adjusted_rate_per_100k ~ s_minus_i_z +
    population_density +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
  | GEOID + year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$population_total
)

summary(proximity)

socio_spatial_proximity <-  felm(
  age_adjusted_rate_per_100k ~ s_minus_i_z + d_minus_i_z+
    population_density +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + 
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS 
  | GEOID + year | 0 | state, 
  data = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$population_total
)

summary(socio_spatial_proximity)

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
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high),
                 height = 0, linewidth = 0.9) +
  geom_point(size = 3) +
  scale_colour_manual(values = c("social" = "red",
                                 "socio-spatial" = "blue"),
                      guide = "none") +
  ## y-axis tick labels -> 'With' (socio-spatial) and 'Without' (social)
  scale_y_discrete(labels = c("socio-spatial" = "Model 2",
                              "social"         = "Model 1")) +
  ## axis labels
  labs(
    x = "Change in focal-county age-adjusted suicide mortality (per 100,000)\nfor a 1-SD increase in socially proximal counties suicide rate",
    y = NULL
  ) +
  ## larger fonts (x-ticks and x-label)
  theme_classic(base_size = 12) +
  theme(
    axis.text.x  = element_text(size = 13),
    axis.title.x = element_text(size = 13),
    axis.text.y  = element_text(size = 13)
  )

### creating the table for reression results relating to proximity ###

stargazer(
  proximity,
  socio_spatial_proximity,
  type = "latex",
  title = paste0(
    "Two-way fixed effect estimates of socio-spatial influence on \\textit{age-adjusted} suicide mortality. ",
    "Column~(1) presents estimates from a model regressing county-level age-adjusted suicide mortality ($\\tilde{y}_{it}$) ",
    "on standardized deaths in socially connected counties ($\\tilde{s}_{-it}$). Column~(2) additionally controls for ",
    "standardized deaths in spatial proximity ($\\tilde{d}_{-it}$) to isolate social influence from geographic proximity. ",
    "Both models include county and year fixed effects and control for population density, racial composition (percentages Asian, Black, and Other races), ",
    "ethnicity (percentage Hispanic), median household income, percentage with limited English proficiency, unemployment rate, ",
    "and percentage with less than a high school education. Age distribution variables are excluded, as the dependent variable ",
    "is already adjusted for population structure. Standard errors are clustered at the state level."
  ),
  column.labels = c("Social Proximity Only", "Socio-Spatial Proximity"),
  dep.var.labels = "Age-adjusted deaths per 100K",
  covariate.labels = c(
    "$\\tilde{s}_{-it}$",
    "$\\tilde{d}_{-it}$",
    "Population Density",
    "Percent Black",
    "Percent Asian",
    "Percent Other",
    "Percent Hispanic",
    "Median Household Income",
    "Percent of population who do not speak English that well",
    "Percent unemployed",
    "Percent with less than high school education"
  ),
  keep.stat = c("n", "rsq", "adj.rsq", "f"),
  no.space = TRUE,
  omit.stat = "ser",
  omit.table.layout = "n",
  column.sep.width = "1pt",
  out = "sensitivity_socio_spatial_table.tex"
)

### event study code ####
# 1) Inputs (from your workspace)
# =========================
# my_data_with_spatial_g : county-year panel (data.frame/data.table)
# w_i_state              : long weights: i_fips, j_state, w_is
# policy_data            : state-level events: state, start_year (>0 = treated)

# Convert to data.table
panel  <- as.data.table(my_data_with_spatial_g)
wstate <- as.data.table(w_i_state)

# Standardize column names for the weights
setnames(wstate, c("i_fips","j_state","w_is"), c("GEOID","event_state","exposure"))

# Event list (only treated states with valid adoption years)
events <- as.data.table(policy_data)[start_year > 0,
                                     .(event_state = state, t0 = start_year)]

# =========================
# 2) Harmonize keys (fix type mismatch)
# =========================
# Keep GEOID as zero-padded character (width 5) everywhere
panel[,  GEOID := str_pad(as.character(GEOID), 5, pad = "0")]
wstate[, GEOID := str_pad(as.character(GEOID), 5, pad = "0")]

# Ensure 'state' is character for clustering
panel[, state := as.character(state)]

# If a state_year FE string is not present, create it
if (!"state_year" %in% names(panel)) {
  panel[, state_year := paste0(state, "_", year)]
}

# =========================
# 3) Design choices
# =========================
K_pre  <- 4  # leads: years before adoption kept in the stack
K_post <- 2  # lags: years after adoption kept in the stack

# =========================
# 4) Build stacked event–study data
# =========================
keep_cols <- c(
  "GEOID","year","state","age_adjusted_rate_per_100k","InvDist_exposure",
  "population_density",
  "prop_black","prop_asian","prop_other","prop_hispanic",
  "ACS_MEDIAN_HH_INC","ACS_PCT_ENGL_NOT_WELL",
  "ACS_PCT_UNEMPLOY","ACS_PCT_LT_HS",
  "ACS_TOT_POP_WT","state_year"
)

stacked_es <- rbindlist(lapply(1:nrow(events), function(j) {
  
  st <- events$event_state[j]
  t0 <- events$t0[j]
  
  # Copy minimal slice
  tmp <- panel[, ..keep_cols]
  
  # Event label and relative year
  tmp[, `:=`(event_id = st,
             rel_year = year - t0)]
  
  # Keep window [-K_pre, K_post]
  tmp <- tmp[rel_year >= -K_pre & rel_year <= K_post]
  
  # Left join time-invariant exposure w_i^(st) for this treated state
  exp_st <- wstate[event_state == st, .(GEOID, exposure)]
  tmp <- merge(tmp, exp_st, by = "GEOID", all.x = TRUE)
  
  # Counties with no social ties to st get exposure = 0
  tmp[is.na(exposure), exposure := 0]
  
  tmp
}), use.names = TRUE, fill = TRUE)

# Global z-score of exposure in the stacked data
stacked_es[, exposure := scale(exposure)[, 1]]

# =========================
# 5) Estimate Wilson-style specification
# =========================
# FE: county-by-event_id and state_year-by-event_id (as in your code)
# Controls: as in your code
fml <- age_adjusted_rate_per_100k ~
  exposure:i(rel_year, ref = -1) + InvDist_exposure +
  population_density  +
  prop_black + prop_asian + prop_other + prop_hispanic +
  ACS_MEDIAN_HH_INC + ACS_PCT_ENGL_NOT_WELL +
  ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS | 
  GEOID^event_id + state_year^event_id

est_es <- feols(
  fml,
  data    = stacked_es,
  weights = ~ ACS_TOT_POP_WT,
  cluster = ~ state
)

# Quick diagnostic: count support by event time and exposure>0
print(table(stacked_es$rel_year, stacked_es$exposure > 0))

# =========================
# 6) Coefficient table for plotting
# =========================
coef_df <- broom::tidy(est_es)[
  grepl("^exposure:rel_year", broom::tidy(est_es)$term),
] 
coef_df$rel_year <- as.integer(stringr::str_extract(coef_df$term, "-?\\d+"))
coef_df <- coef_df[order(coef_df$rel_year), ]

# =========================
# 7) Event-study plot
# =========================
ggplot(coef_df, aes(x = rel_year, y = estimate)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.25) +
  geom_vline(xintercept = 0, linetype = "dotted", linewidth = 0.25) +
  geom_line(linewidth = 0.5) +
  geom_pointrange(aes(ymin = estimate - 1.96 * std.error,
                      ymax = estimate + 1.96 * std.error),
                  size = 0.4) +
  labs(x = "Years relative to ERPO adoption",
       y = "Coefficient estimate (per 100,000)") +
  theme_minimal(base_size = 10)

# =========================
# 8) (Optional) DID 'did' package block without breaking FIPS
# =========================
# Keep character GEOID for joins; create a separate integer id for did::att_gt
panel[, id_int := as.integer(factor(GEOID))]

### r
