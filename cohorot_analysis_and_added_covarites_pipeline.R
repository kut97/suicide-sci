# =============================================================================
#  contagion (H1 + the R1.2/R3.1 robustness module). ERPO models, the R1.3
#  subgroup block, and the choropleth are intentionally omitted here.
#
#  NEW CONFOUNDERS added to every model (R3.3, county x year -> identifiable
#  under county + year FE):
#     - covid_deaths_per_100k        (COVID-19 burden; 0 in 2010-2019)
#     - overdose_deaths_per_100k     (substance-use proxy; EXCLUDES X60-X64,
#                                      which are suicides already in the outcome)
#     - poor_mental_health_days, mental_health_providers,
#       frequent_mental_distress    (County Health Rankings)
#     - excessive_drinking           (alcohol-availability/control proxy, CHR
#                                      v049). Chosen over alcohol-related death
#                                      rates because it sits closer to the
#                                      causal pathway: alcohol control laws ->
#                                      excessive drinking -> alcohol-related
#                                      death. (County Health Rankings)
#
#  COVID note: it occurs only in 2020-2022, but the YEAR fixed effects absorb
#  the common national shock; the covariate is identified from cross-county
#  variation WITHIN those years. The pre-2020 zeros carry no within-year
#  variation and do not distort estimation. No special handling needed.
#
#  FIREARM policies (waiting period, child-access, minimum age) are pulled into
#  the data pipeline AND are now INCLUDED in the regression by default
#  (include_firearm = TRUE). They are state x year but remain identifiable here
#  because H1 uses county + year FE (not state-by-year FE).
# =============================================================================

# ---------------------------- 0. LIBRARIES -----------------------------------
library(lfe); library(tidyverse); library(igraph); library(tigris)
library(tidycensus); library(geodist); library(sf); library(readxl)
library(lubridate); library(SDPDmod); library(readr); library(stargazer)
library(modelsummary); library(broom); library(data.table); library(ggplot2)
library(viridis); library(fixest)

zstd <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)

# =============================================================================
#  1. SUICIDE MORTALITY  (NVSS Multiple Cause-of-Death, 2010-2022)
# =============================================================================
read_us_mortality_data <- function(file_path) {
  positions <- list(
    resident.status   = c(20, 20), state.occurrence = c(21, 22),
    county.occurrence = c(23, 25), state.residence  = c(29, 30),
    county.residence  = c(35, 37), education.1989   = c(61, 62),
    education.2003     = c(63, 63), education.flag   = c(64, 64),
    `month.of-death`   = c(65, 66), sex             = c(69, 69),
    age                = c(70, 73), age.flag         = c(74, 74),
    age.recode.52      = c(75, 76), age.recode.27    = c(77, 78),
    age.recode.12      = c(79, 80), age.recode.22    = c(81, 82),
    place.of.death     = c(83, 83), marital.status   = c(84, 84),
    `day.of-week.of-death` = c(85, 85), data.year    = c(102, 105),
    injury.at.work     = c(106, 106), `manner.of-death` = c(107, 107),
    `method.of-disposition` = c(108, 108), autopsy   = c(109, 109),
    `activity.code`    = c(144, 144), `place.of.injury` = c(145, 145),
    `ucod.icd.10`      = c(146, 149), `ucod.recode.358` = c(150, 152),
    `ucod.recode.113`  = c(154, 156), `ucod.recode.130` = c(157, 159),
    `ucod.recode.39`   = c(160, 161), entity.n       = c(163, 164),
    race               = c(445, 446), race.recode.3  = c(449, 449),
    race.recode.5      = c(450, 450), hispanic       = c(484, 486),
    hispanic.recode    = c(488, 488), race.recode.40 = c(489, 490)
  )
  ss <- vapply(positions, function(x) as.integer(x[1]), integer(1))
  ee <- vapply(positions, function(x) as.integer(x[2]), integer(1))
  readr::read_fwf(file = file_path,
                  col_positions = readr::fwf_positions(start = ss, end = ee, col_names = names(positions)))
}

mort_root <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/m_files"
mort_2010 <- read_us_mortality_data(file.path(mort_root, "MULT2010.USPSAllCnty/MULT2010.USAllCnty.txt"))
mort_2011 <- read_us_mortality_data(file.path(mort_root, "MULT2011.USPSAllCnty/MULT2011.USAllCnty.txt"))
mort_2012 <- read_us_mortality_data(file.path(mort_root, "MULT2012.USPSAllCnty/MULT2012.USAllCnty.txt"))
mort_2013 <- read_us_mortality_data(file.path(mort_root, "MULT2013.USPSAllCnty/MULT2013.USAllCnty.txt"))
mort_2014 <- read_us_mortality_data(file.path(mort_root, "MULT2014.USPSAllCnty/MULT2014.USAllCnty.txt"))
mort_2015 <- read_us_mortality_data(file.path(mort_root, "MULT2015.USPSAllCnty/MULT2015.USAllCnty.txt"))
mort_2016 <- read_us_mortality_data(file.path(mort_root, "MULT2016.USPSAllCnty/MULT2016.USAllCnty.txt"))
mort_2017 <- read_us_mortality_data(file.path(mort_root, "MULT2017.USPSAllCnty/MULT2017.USAllCnty.txt"))
mort_2018 <- read_us_mortality_data(file.path(mort_root, "MULT2018.USPSAllCnty/Mort2018US.AllCnty.txt"))
mort_2019 <- read_us_mortality_data(file.path(mort_root, "MULT2019.USPSAllCnty/MULT2019US.AllCnty.txt"))
mort_2020 <- read_us_mortality_data(file.path(mort_root, "MULT2020.AllCnty/MULT2020.USAllCnty.txt"))
mort_2021 <- read_us_mortality_data(file.path(mort_root, "MULT2021.AllCnty/MULT2021US.AllCnty.txt"))
mort_2022 <- read_us_mortality_data(file.path(mort_root, "MULT2022_AllCnty/MULT2022US.AllCnty.txt"))

suicide_codes <- c(sprintf("X%02d", 60:84), "Y87.0")
abbs <- c(state.abb, "DC")
fips_codes <- c("01","02","04","05","06","08","09","10","12","13","15","19","16",
                "17","18","20","21","22","23","24","25","26","27","28","29","30",
                "31","32","33","34","35","36","37","38","39","40","41","42","44",
                "45","46","47","48","49","50","51","53","54","55","56","11")
state_codes <- tibble(state_abbr = abbs, state_fips = fips_codes)

summarise_suicides <- function(mort_df) {
  mort_df %>% filter(ucod.icd.10 %in% suicide_codes) %>%
    mutate(county_fips = sprintf("%03d", as.integer(county.occurrence)),
           year = data.year, state_abbr = state.occurrence) %>%
    group_by(year, state_abbr, county_fips) %>%
    summarise(total_deaths = n(), .groups = "drop") %>%
    left_join(state_codes, by = "state_abbr") %>%
    mutate(GEOID = paste0(state_fips, county_fips)) %>%
    select(year, state = state_abbr, state_fips, county_fips, GEOID, total_deaths)
}

mort_list <- list(`2010`=mort_2010,`2011`=mort_2011,`2012`=mort_2012,`2013`=mort_2013,
                  `2014`=mort_2014,`2015`=mort_2015,`2016`=mort_2016,`2017`=mort_2017,
                  `2018`=mort_2018,`2019`=mort_2019,`2020`=mort_2020,`2021`=mort_2021,
                  `2022`=mort_2022)

suicide_panel_all <- bind_rows(lapply(mort_list, summarise_suicides), .id = "year_list") %>%
  mutate(year = as.integer(year)) %>%
  select(year, state, state_fips, county_fips, GEOID, total_deaths) %>% arrange(year, GEOID)

cdc_Wonder_data <- read.csv(
  "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/cdc_wonder.csv",
  colClasses = c(GEOID = "character"), stringsAsFactors = FALSE)
has_year_col <- "year" %in% colnames(cdc_Wonder_data)
if (has_year_col) {
  ref_geoids_by_year <- cdc_Wonder_data %>% mutate(year = as.integer(year)) %>%
    filter(!is.na(GEOID)) %>% group_by(year) %>%
    summarize(ref_geoids = list(unique(GEOID)), .groups = "drop")
} else ref_geoids_all <- unique(cdc_Wonder_data$GEOID)

years <- 2010:2022
complete_suicide_panel_ref <- map_dfr(years, function(y) {
  if (has_year_col) {
    entry <- ref_geoids_by_year %>% filter(year == y) %>% pull(ref_geoids)
    geoids_ref <- if (length(entry) == 1) entry[[1]] else character(0)
  } else geoids_ref <- ref_geoids_all
  tibble(GEOID = geoids_ref) %>%
    mutate(year = y, state_fips = substr(GEOID,1,2), county_fips = substr(GEOID,3,5)) %>%
    left_join(state_codes, by = "state_fips") %>% rename(state = state_abbr) %>%
    left_join(suicide_panel_all %>% filter(year == y) %>% select(GEOID, total_deaths), by = "GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})
exclude_geoids <- c("02158","02261","46102","15005")
complete_suicide_panel_ref <- complete_suicide_panel_ref %>% filter(!GEOID %in% exclude_geoids)
ref_geoids_all <- unique(cdc_Wonder_data$GEOID)
additional_panel <- map_dfr(c(2021L,2022L), function(y) {
  tibble(GEOID = ref_geoids_all) %>%
    mutate(year = y, state_fips = substr(GEOID,1,2), county_fips = substr(GEOID,3,5)) %>%
    left_join(state_codes, by = "state_fips") %>% rename(state = state_abbr) %>%
    left_join(suicide_panel_all %>% filter(year == y) %>% select(GEOID, total_deaths), by = "GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})
complete_suicide_panel_ref <- bind_rows(complete_suicide_panel_ref, additional_panel) %>%
  filter(!GEOID %in% exclude_geoids)

# =============================================================================
#  2. SOCIOECONOMIC COVARIATES  (AHRQ SDOH 2010-2020 + ACS 2021-2022)
# =============================================================================
data_path <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/SDOH_Covariates/sdoh_csvs"
files <- list.files(data_path, pattern = "^sdoh_\\d{4}\\.csv$", full.names = TRUE)
for (f in files) {
  yr <- sub(".*sdoh_(\\d{4}).*", "\\1", basename(f))
  tmp <- read.csv(f, colClasses = c(COUNTYFIPS = "character"))
  tmp$prop_black    <- tmp$ACS_PCT_BLACK         / 100
  tmp$prop_asian    <- tmp$ACS_PCT_ASIAN_NONHISP / 100
  tmp$prop_other    <- (tmp$ACS_PCT_AIAN_NONHISP + tmp$ACS_PCT_NHPI_NONHISP + tmp$ACS_PCT_MULT_RACE_NONHISP)/100
  tmp$prop_hispanic <- tmp$ACS_PCT_HISPANIC      / 100
  tmp$ACS_PCT_AGE_U18 <- rowSums(tmp[, c("ACS_PCT_AGE_0_4","ACS_PCT_AGE_5_9","ACS_PCT_AGE_10_14","ACS_PCT_AGE_15_17")], na.rm = FALSE)
  assign(paste0("df_", yr), tmp)
}
years_files <- sub(".*sdoh_(\\d{4}).*", "\\1", basename(files))
my_panel <- do.call(bind_rows, lapply(years_files, function(yr) {
  d <- get(paste0("df_", yr)); if (!"YEAR" %in% names(d)) d$YEAR <- as.numeric(yr)
  req <- c("COUNTYFIPS","YEAR","ACS_TOT_POP_WT","prop_black","prop_asian","prop_other","prop_hispanic",
           "ACS_MEDIAN_HH_INC","ACS_PCT_UNEMPLOY","ACS_PCT_LT_HS","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64",
           "ACS_PCT_AGE_U18","ACS_PCT_AGE_ABOVE65","ACS_PCT_ENGL_NOT_WELL")
  for (v in setdiff(req, intersect(req, names(d)))) d[[v]] <- NA
  d %>% select(all_of(req))
}))
my_panel <- my_panel %>% filter(COUNTYFIPS %in% complete_suicide_panel_ref$GEOID)
suicide_mortality <- complete_suicide_panel_ref %>%
  left_join(my_panel, by = c("GEOID" = "COUNTYFIPS", "year" = "YEAR"))

get_sdoh_acs <- function(year) {
  core <- c(total_pop="B01003_001", black="B02001_003", asian="B02001_005", aian="B02001_004",
            nhpi="B02001_006", multi="B02001_008", hisp="B03003_003", med_inc="B19013_001",
            unem="B23025_005", labor="B23025_003", lt_hs="B15003_017", tot_edu="B15003_001",
            eng_ltwell="C16002_004", hh_total="C16002_001")
  u18 <- c(sprintf("B01001_%03d",3:6),  sprintf("B01001_%03d",27:30))
  a1844 <- c(sprintf("B01001_%03d",7:14), sprintf("B01001_%03d",31:38))
  a4564 <- c(sprintf("B01001_%03d",15:19),sprintf("B01001_%03d",39:43))
  a65 <- c(sprintf("B01001_%03d",20:25),sprintf("B01001_%03d",44:49))
  vars <- c(core, setNames(u18,u18), setNames(a1844,a1844), setNames(a4564,a4564), setNames(a65,a65))
  acs <- get_acs("county", variables = vars, year = year, survey = "acs5", output = "wide", cache_table = TRUE) %>%
    dplyr::rename_with(~ sub("E$","",.x), dplyr::ends_with("E"))
  rs <- function(cols) acs %>% dplyr::select(dplyr::all_of(cols)) %>% as.matrix() %>% rowSums(na.rm = TRUE)
  acs %>% dplyr::transmute(
    COUNTYFIPS=GEOID, YEAR=year, ACS_TOT_POP_WT=total_pop,
    prop_black=black/total_pop, prop_asian=asian/total_pop,
    prop_other=(aian+nhpi+multi)/total_pop, prop_hispanic=hisp/total_pop,
    ACS_MEDIAN_HH_INC=med_inc, ACS_PCT_UNEMPLOY=100*unem/labor, ACS_PCT_LT_HS=100*lt_hs/tot_edu,
    ACS_PCT_AGE_U18=100*rs(u18)/total_pop, ACS_PCT_AGE_18_44=100*rs(a1844)/total_pop,
    ACS_PCT_AGE_45_64=100*rs(a4564)/total_pop, ACS_PCT_AGE_ABOVE65=100*rs(a65)/total_pop,
    ACS_PCT_ENGL_NOT_WELL=100*eng_ltwell/hh_total)
}
acs_vars <- c("ACS_TOT_POP_WT","prop_black","prop_asian","prop_other","prop_hispanic",
              "ACS_MEDIAN_HH_INC","ACS_PCT_UNEMPLOY","ACS_PCT_LT_HS","ACS_PCT_AGE_18_44",
              "ACS_PCT_AGE_45_64","ACS_PCT_AGE_U18","ACS_PCT_AGE_ABOVE65","ACS_PCT_ENGL_NOT_WELL")
acs21_trim <- get_sdoh_acs(2021) %>% rename(GEOID=COUNTYFIPS, year=YEAR) %>% semi_join(suicide_mortality, by=c("GEOID","year"))
acs22_trim <- get_sdoh_acs(2022) %>% rename(GEOID=COUNTYFIPS, year=YEAR) %>% semi_join(suicide_mortality, by=c("GEOID","year"))
acs_new <- bind_rows(acs21_trim, acs22_trim)
sm1 <- suicide_mortality %>% left_join(acs_new, by=c("GEOID","year"), suffix=c("",".acs")) %>%
  mutate(across(all_of(acs_vars), ~ coalesce(.x, get(paste0(cur_column(),".acs"))))) %>% select(-ends_with(".acs"))
cm  <- sm1 %>% filter(year <= 2020) %>% group_by(GEOID) %>%
  summarise(across(all_of(acs_vars), ~ mean(.x, na.rm=TRUE), .names="{.col}_mu"), .groups="drop")
sm2 <- sm1 %>% left_join(cm, by="GEOID") %>%
  mutate(across(all_of(acs_vars), ~ coalesce(.x, get(paste0(cur_column(),"_mu"))))) %>% select(-ends_with("_mu"))
nm  <- sm2 %>% summarise(across(all_of(acs_vars), ~ mean(.x, na.rm=TRUE)))
suicide_mortality <- sm2 %>% mutate(across(all_of(acs_vars), ~ coalesce(.x, nm[[cur_column()]])))

# =============================================================================
#  3. GEOMETRY: centroids, population density, death rates, distance matrix
# =============================================================================
counties_sf <- counties(cb = TRUE, class = "sf")
selected_counties <- counties_sf[counties_sf$GEOID %in% unique(suicide_mortality$GEOID), ]
centroids <- st_centroid(selected_counties); centroids <- centroids[order(centroids$GEOID), ]
coords <- st_coordinates(centroids)
selected_counties <- selected_counties[order(selected_counties$GEOID), ]
geoid_lat_lng <- data.frame(GEOID = selected_counties$GEOID, Longitude = coords[,1], Latitude = coords[,2])
suicide_mortality <- left_join(suicide_mortality, geoid_lat_lng, by = "GEOID")

counties_area <- counties(year = 2018, cb = TRUE) %>% st_transform(crs = 5070) %>%
  mutate(area_sq_km = as.numeric(st_area(geometry))/1e6)
counties_area <- counties_area %>% filter(GEOID %in% suicide_mortality$GEOID) %>% select(GEOID, area_sq_km)
suicide_mortality <- merge(suicide_mortality, counties_area, by = "GEOID") %>%
  mutate(population_density = ACS_TOT_POP_WT/area_sq_km,
         death_rates_per_100_k = if_else(ACS_TOT_POP_WT > 0, (total_deaths/ACS_TOT_POP_WT)*1e5, NA_real_))

my_data_with_spatial_g <- suicide_mortality %>%
  mutate(GEOID = sprintf("%05d", as.integer(GEOID)), year = as.integer(year))

# distance matrix (county order = sorted GEOID); needed for spatial weights A
unique_counties <- my_data_with_spatial_g %>% distinct(GEOID, Longitude, Latitude) %>% arrange(GEOID)
distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")], measure = "geodesic")/1000

# =============================================================================
#  4. NEW CONFOUNDERS (R3.3)  — added to the analysis panel
# =============================================================================
pop_lookup <- my_data_with_spatial_g %>% as_tibble() %>%
  select(GEOID, year, ACS_TOT_POP_WT) %>% distinct()
all_geoids <- unique(my_data_with_spatial_g$GEOID)
scaffold <- expand.grid(GEOID = all_geoids, year = 2010:2022, stringsAsFactors = FALSE) %>% as_tibble()

## 4a. COVID-19 cumulative deaths per 100k (NYT; 0 for 2010-2019) -------------
##     Year FE absorb the common shock; identified from within-year cross-county
##     variation in 2020-2022. No special treatment for the all-zero years.
covid_raw <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv",
                      show_col_types = FALSE)
covid_panel <- covid_raw %>% filter(!is.na(fips)) %>%
  mutate(GEOID = sprintf("%05d", as.integer(fips)), year = year(date)) %>%
  group_by(GEOID, year) %>% summarise(covid_deaths_cum = max(deaths, na.rm = TRUE), .groups = "drop")
covid_full <- scaffold %>% left_join(covid_panel, by = c("GEOID","year")) %>%
  mutate(covid_deaths_cum = replace_na(covid_deaths_cum, 0)) %>%
  left_join(pop_lookup, by = c("GEOID","year")) %>%
  mutate(covid_deaths_per_100k = if_else(ACS_TOT_POP_WT > 0, covid_deaths_cum/ACS_TOT_POP_WT*1e5, 0)) %>%
  select(GEOID, year, covid_deaths_per_100k)

## 4b. Drug-overdose deaths per 100k (substance-use proxy) --------------------
##     EXCLUDES X60-X64 (intentional self-poisoning) because those are already
##     part of the suicide OUTCOME (X60-X84). Keeps accidental (X40-X44),
##     assault (X85), and undetermined (Y10-Y14) drug poisoning only.
overdose_codes <- c(sprintf("X%02d", 40:44), "X85", sprintf("Y%02d", 10:14))
summarise_overdoses <- function(mort_df) {
  mort_df %>% filter(ucod.icd.10 %in% overdose_codes) %>%
    mutate(county_fips = sprintf("%03d", as.integer(county.occurrence)),
           year = data.year, state_abbr = state.occurrence) %>%
    group_by(year, state_abbr, county_fips) %>% summarise(overdose_deaths = n(), .groups = "drop") %>%
    left_join(state_codes, by = "state_abbr") %>%
    mutate(GEOID = paste0(state_fips, county_fips)) %>% select(year, GEOID, overdose_deaths)
}
overdose_raw <- bind_rows(lapply(mort_list, summarise_overdoses), .id = "year_list") %>%
  mutate(year = as.integer(year)) %>% select(year, GEOID, overdose_deaths)
overdose_full <- scaffold %>% left_join(overdose_raw, by = c("GEOID","year")) %>%
  mutate(overdose_deaths = replace_na(overdose_deaths, 0L)) %>%
  left_join(pop_lookup, by = c("GEOID","year")) %>%
  mutate(overdose_deaths_per_100k = if_else(ACS_TOT_POP_WT > 0, overdose_deaths/ACS_TOT_POP_WT*1e5, 0)) %>%
  select(GEOID, year, overdose_deaths_per_100k)

## 4c. County Health Rankings measures ----------------------------------------
##     Mental-health measures plus EXCESSIVE DRINKING (v049), the alcohol
##     availability/control proxy recommended over alcohol-related death rates
##     because it lies on the causal pathway:
##       alcohol control laws -> excessive drinking -> alcohol-related death.
chr_folder <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/chr_csvs"
dir.create(chr_folder, showWarnings = FALSE, recursive = TRUE)
mh_patterns <- c(poor_mental_health_days  = "^v042_rawvalue$|poor.*mental.*health.*day|mentally.*unhealthy",
                 mental_health_providers  = "^v062_rawvalue$|mental.*health.*provider",
                 frequent_mental_distress = "^v145_rawvalue$|frequent.*mental.*distress",
                 excessive_drinking       = "^v049_rawvalue$|excessive.*drink")
base1 <- "https://www.countyhealthrankings.org/sites/default/files/"
base2 <- "https://www.countyhealthrankings.org/sites/default/files/media/document/"
chr_urls <- c("2010"=paste0(base1,"analytic_data2010.csv"),"2011"=paste0(base1,"analytic_data2011.csv"),
              "2012"=paste0(base1,"analytic_data2012.csv"),"2013"=paste0(base1,"analytic_data2013.csv"),
              "2014"=paste0(base1,"analytic_data2014.csv"),"2015"=paste0(base1,"analytic_data2015.csv"),
              "2016"=paste0(base1,"analytic_data2016.csv"),"2017"=paste0(base1,"analytic_data2017.csv"),
              "2018"=paste0(base1,"analytic_data2018_0.csv"),"2019"=paste0(base2,"analytic_data2019.csv"),
              "2020"=paste0(base2,"analytic_data2020_0.csv"),"2021"=paste0(base2,"analytic_data2021.csv"),
              "2022"=paste0(base2,"analytic_data2022.csv"))
for (yr in names(chr_urls)) {
  dest <- file.path(chr_folder, paste0("analytic_data", yr, ".csv"))
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
chr_files <- list.files(chr_folder, pattern = "analytic_data\\d{4}\\.csv$", full.names = TRUE)
chr_panel <- map_dfr(chr_files, read_chr_year) %>% distinct(GEOID, year, .keep_all = TRUE)

## 4d. RAND firearm policies — PULLED INTO PIPELINE and INCLUDED in regression -
firearm_xlsx <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/RAND_DATA/TL-A243-2-v4 State Firearm Law Database 6.0.xlsx"
fa_cols <- c("fa_waiting_period","fa_cap","fa_min_age","fa_min_age_21")
if (file.exists(firearm_xlsx)) {
  db <- read_excel(firearm_xlsx, sheet = "Database") %>%
    transmute(state = `State Postal Abbreviation`, class = `Law Class`, subtype = `Law Class Subtype`,
              guns = `Handguns or Long Guns`, effect = `Effect`, change = `Type of Change`,
              eff_year = suppressWarnings(as.integer(`Effective Date Year`)),
              age = suppressWarnings(as.integer(`Age for Minimum Age Laws`))) %>% filter(!is.na(eff_year))
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
  firearm_policy <- expand_grid(state = unique(my_data_with_spatial_g$state), year = 2010:2022L) %>%
    mutate(fa_waiting_period = 0L, fa_cap = 0L, fa_min_age = 0L, fa_min_age_21 = 0L)
}

## 4e. MERGE all new confounders into the panel ------------------------------
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  as_tibble() %>%
  left_join(covid_full,    by = c("GEOID","year")) %>%
  left_join(overdose_full, by = c("GEOID","year")) %>%
  left_join(chr_panel,     by = c("GEOID","year")) %>%
  left_join(firearm_policy, by = c("state","year")) %>%
  mutate(covid_deaths_per_100k    = replace_na(covid_deaths_per_100k, 0),
         overdose_deaths_per_100k = replace_na(overdose_deaths_per_100k, 0),
         across(any_of(fa_cols), ~ replace_na(.x, 0L)))

# bridge CHR gap years within county (downup), then we standardize below
my_data_with_spatial_g <- my_data_with_spatial_g %>% arrange(GEOID, year) %>% group_by(GEOID) %>%
  fill(any_of(names(mh_patterns)), .direction = "downup") %>% ungroup()

# =============================================================================
#  5. STANDARDIZE continuous covariates (binary firearm flags left as 0/1)
# =============================================================================
base_covars    <- c("population_density","ACS_PCT_AGE_U18","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64",
                    "prop_asian","prop_black","prop_other","prop_hispanic","ACS_MEDIAN_HH_INC",
                    "ACS_PCT_ENGL_NOT_WELL","ACS_PCT_UNEMPLOY","ACS_PCT_LT_HS")
health_covars  <- intersect(c("covid_deaths_per_100k","overdose_deaths_per_100k",
                              names(mh_patterns)), names(my_data_with_spatial_g))
firearm_covars <- intersect(c("fa_waiting_period","fa_cap","fa_min_age_21"), names(my_data_with_spatial_g))

std_continuous <- c(base_covars, "covid_deaths_per_100k", "overdose_deaths_per_100k",
                    intersect(names(mh_patterns), names(my_data_with_spatial_g)))
my_data_with_spatial_g[std_continuous] <- lapply(my_data_with_spatial_g[std_continuous], zstd)
# preserve N: CHR measures with no coverage become 0 (the standardized mean)
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(across(any_of(names(mh_patterns)), ~ replace_na(.x, 0)))

# ---- TOGGLE: include firearm policies in the regression? -------------------
#  DECISION (R3.3): firearm policies (waiting period, child-access prevention,
#  minimum purchase age 21) are now INCLUDED in the regression.
include_firearm <- TRUE
covs_h1 <- c(base_covars, health_covars, if (isTRUE(include_firearm)) firearm_covars)
covs_h1 <- intersect(covs_h1, names(my_data_with_spatial_g))
cat("\nH1 covariate set (", length(covs_h1), " terms):\n  ", paste(covs_h1, collapse=", "), "\n", sep="")

# =============================================================================
#  6. SOCIAL & SPATIAL PROXIMITY  -> W, A, s_{-it}, d_{-it}
# =============================================================================
social_df <- suicide_mortality %>% mutate(GEOID = sprintf("%05d", as.integer(GEOID))) %>%
  select(GEOID, ACS_TOT_POP_WT, death_rates_per_100_k)
colnames(social_df)[1] <- "fr_loc"; colnames(social_df)[3] <- "deaths_per_capita"
social_df <- social_df[order(social_df$fr_loc), ] %>% distinct(fr_loc, .keep_all = TRUE)

df_0 <- read_tsv("C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv")
df_1 <- df_0 %>% mutate(user_loc = sprintf("%05d", as.integer(user_loc)),
                        fr_loc   = sprintf("%05d", as.integer(fr_loc))) %>%
  filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc) %>%
  filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
nodes <- social_df %>% select(fr_loc) %>% distinct()
k <- graph.data.frame(df_1 %>% select(user_loc, fr_loc, scaled_sci), directed = FALSE, vertices = nodes)

population <- suicide_mortality %>% mutate(GEOID = sprintf("%05d", as.integer(GEOID))) %>%
  group_by(GEOID) %>% summarise(population = round(mean(ACS_TOT_POP_WT, na.rm = TRUE))) %>%
  filter(GEOID %in% nodes$fr_loc) %>% arrange(match(GEOID, nodes$fr_loc))
pop_vector <- population$population

W <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE); diag(W) <- 0
W <- sweep(W, 2, pop_vector, `*`); W <- sweep(W, 1, rowSums(W), `/`); W[is.na(W)] <- 0
A <- InvDistMat(distance_km); diag(A) <- 0; A <- sweep(A, 1, rowSums(A), '/')

setDT(my_data_with_spatial_g); setkey(my_data_with_spatial_g, GEOID)
my_data_with_spatial_g[, `:=`(s_minus_i = NA_real_, d_minus_i = NA_real_)]
for (yr in sort(unique(my_data_with_spatial_g$year))) {
  idx  <- match(my_data_with_spatial_g[year == yr, GEOID], nodes$fr_loc)
  y_t  <- my_data_with_spatial_g[year == yr][order(idx), death_rates_per_100_k]
  my_data_with_spatial_g[year == yr, `:=`(s_minus_i = as.numeric(W[idx, , drop=FALSE] %*% y_t),
                                          d_minus_i = as.numeric(A[idx, , drop=FALSE] %*% y_t))]
}
my_data_with_spatial_g[, s_minus_i_z := as.numeric(scale(s_minus_i))]
my_data_with_spatial_g[, d_minus_i_z := as.numeric(scale(d_minus_i))]

# =============================================================================
#  7. H1 MODELS with the EXTENDED covariate set  (Table 1, extended)
# =============================================================================
rhs <- paste(covs_h1, collapse = " + ")
proximity <- felm(as.formula(paste0("death_rates_per_100_k ~ s_minus_i_z + ", rhs, " | GEOID + year | 0 | state")),
                  data = my_data_with_spatial_g, weights = my_data_with_spatial_g$ACS_TOT_POP_WT)
socio_spatial_proximity <- felm(as.formula(paste0("death_rates_per_100_k ~ s_minus_i_z + d_minus_i_z + ", rhs, " | GEOID + year | 0 | state")),
                                data = my_data_with_spatial_g, weights = my_data_with_spatial_g$ACS_TOT_POP_WT)
summary(proximity); summary(socio_spatial_proximity)

modelsummary(list("Social only" = proximity, "Socio-spatial" = socio_spatial_proximity),
             gof_map = c("nobs","r.squared"), stars = c("*"=.1,"**"=.05,"***"=.01),
             output = op("table1_proximity_extended.tex"))
modelsummary(list("Social only" = proximity, "Socio-spatial" = socio_spatial_proximity),
             gof_map = c("nobs","r.squared"), stars = c("*"=.1,"**"=.05,"***"=.01), output = "markdown")

# =============================================================================
#  8. R1.2 + R3.1 MODULE  (uses the SAME extended covariate set covs_h1)
# =============================================================================
panel <- as_tibble(my_data_with_spatial_g) %>% mutate(GEOID = sprintf("%05d", as.integer(GEOID)), year = as.integer(year))
ord <- sprintf("%05d", as.integer(nodes$fr_loc)); nn <- length(ord)
yrs <- sort(unique(panel$year)); stopifnot(nrow(distance_km) == nn)
state_vec <- (panel %>% distinct(GEOID, state))$state[match(ord, (panel %>% distinct(GEOID, state))$GEOID)]
covs <- covs_h1   # <<< consistency: identical covariates as the H1 models

S <- as.matrix(igraph::as_adjacency_matrix(k, attr = "scaled_sci", sparse = FALSE))
S <- S[match(ord, rownames(S)), match(ord, colnames(S))]; diag(S) <- 0; storage.mode(S) <- "double"

out_dir <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/reviewer_response_R1_2_R3_1"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
op <- function(f) file.path(out_dir, f)
## 8.1 Validity: SCI vs 1/distance per county pair ----------------------------
inv_d_of_pair <- function(a, b) 1/distance_km[cbind(match(a,ord), match(b,ord))]
pairs_dt <- as.data.table(df_1)[, .(user_loc, fr_loc, scaled_sci)]
pairs_dt <- pairs_dt[user_loc %in% ord & fr_loc %in% ord & user_loc != fr_loc]
pairs_dt[, inv_d := inv_d_of_pair(user_loc, fr_loc)]; pairs_dt <- pairs_dt[is.finite(inv_d) & scaled_sci > 0]
cor_pearson <- cor(log(pairs_dt$scaled_sci), log(pairs_dt$inv_d))
cat(sprintf("\n[R3.1] SCI vs 1/distance over %s pairs: Pearson(log,log)=%.3f | distance explains ~%.0f%% of log-SCI variance.\n",
            format(nrow(pairs_dt), big.mark=","), cor_pearson, 100*cor_pearson^2))
p_scatter <- ggplot(pairs_dt, aes(log(inv_d), log(scaled_sci))) +
  geom_bin2d(bins = 80) + geom_smooth(method = "lm", se = FALSE, colour = "white", linewidth = 0.7) +
  scale_fill_viridis_c(option = "magma", trans = "log10", name = "Pairs") +
  labs(x = "log(1 / distance)  (spatial proximity)", y = "log(SCI)  (social connectedness)",
       subtitle = sprintf("Pearson(log,log) = %.2f", cor_pearson)) + theme_classic(base_size = 12)
ggsave(op("R3_1_sci_vs_distance_scatter.pdf"), p_scatter, width = 6.5, height = 4.5)
## 8.2 Robustness to the social-neighborhood definition -----------------------
build_W <- function(Sin, pop, mask = NULL, pop_scale = TRUE) {
  Wv <- Sin; if (!is.null(mask)) Wv[mask] <- 0; diag(Wv) <- 0
  if (pop_scale) Wv <- sweep(Wv, 2, pop, `*`)
  rs <- rowSums(Wv); rs[rs == 0] <- NA; Wv <- sweep(Wv, 1, rs, `/`); Wv[is.na(Wv)] <- 0; Wv
}
same_state <- outer(state_vec, state_vec, `==`); far_100 <- distance_km > 100
proximity_s <- function(Wv) {
  bind_rows(lapply(yrs, function(yy) {
    sub <- panel[panel$year == yy, c("GEOID","death_rates_per_100_k")]
    y <- setNames(numeric(nn), ord); y[sub$GEOID] <- sub$death_rates_per_100_k; y[!is.finite(y)] <- 0
    tibble(GEOID = ord, year = yy, s_raw = as.numeric(Wv %*% y))
  }))
}
fit_social <- function(Wv, label) {
  d <- panel %>% left_join(proximity_s(Wv), by = c("GEOID","year")) %>% mutate(s_z = as.numeric(scale(s_raw)))
  m <- feols(as.formula(paste0("death_rates_per_100_k ~ s_z + d_minus_i_z + ", paste(covs, collapse=" + "), " | GEOID + year")),
             data = d, weights = ~ ACS_TOT_POP_WT, cluster = ~ state)
  list(model = m, label = label, sd_raw = sd(proximity_s(Wv)$s_raw, na.rm=TRUE),
       coef = broom::tidy(m, conf.int = TRUE) %>% filter(term == "s_z") %>% mutate(spec = label))
}
top_pct_mask <- function(Sin, pct = 0.99) {
  mask <- matrix(TRUE, nn, nn)
  for (r in seq_len(nn)) mask[r, Sin[r, ] >= quantile(Sin[r, ], pct, na.rm = TRUE)] <- FALSE
  mask
}
W_base <- build_W(S, pop_vector)
specs <- list(fit_social(W_base, "Baseline (all alters)"),
              fit_social(build_W(S, pop_vector, mask = !far_100), "Distant alters only (> 100 km)"),
              fit_social(build_W(S, pop_vector, mask = top_pct_mask(S)), "Top 1% strongest ties only"))
results_robust <- bind_rows(lapply(specs, `[[`, "coef")) %>%
  mutate(spec = factor(spec, levels = rev(sapply(specs, `[[`, "label"))))
cat("\n[R1.2/R3.1] s_{-it} under alternative neighborhood definitions (extended covariates):\n")
print(results_robust %>% select(spec, estimate, conf.low, conf.high, p.value))
write_csv(results_robust, op("R1_2_social_neighborhood_robustness_extended.csv"))

modelsummary(setNames(lapply(specs, `[[`, "model"), sapply(specs, `[[`, "label")),
             coef_map = c(s_z = "Deaths in social proximity (s_{-it}, per 1 SD)",
                          d_minus_i_z = "Deaths in spatial proximity (d_{-it}, per 1 SD)"),
             gof_map = c("nobs","r.squared"), stars = c("*"=.1,"**"=.05,"***"=.01),
             output = op("R1_2_R3_1_robustness_extended.tex"))

sd_s_raw <- specs[[1]]$sd_raw; b_s <- specs[[1]]$coef$estimate
cat(sprintf("\n[R1.2] 1 SD of s_{-it} (raw) = %.2f deaths/100k; baseline coefficient = %.2f.\n", sd_s_raw, b_s))
cat("\nDone. include_firearm =", include_firearm, "| H1 covariates listed above.\n")

# ============================================================================
#  R1.3  —  SUBGROUP / STRATIFIED ANALYSIS for SOCIAL PROXIMITY (H1)
#  INTEGRATED with the extended-covariate script.
# ----------------------------------------------------------------------------
#  Drop-in replacement for the previous R1.3 block. Place it after Section 8
#  (R1.2/R3.1). It REUSES objects already built above:
#     mort_list, state_codes, nodes, W, A, my_data_with_spatial_g, zstd(),
#     and -- crucially -- covs_h1 (the extended H1 covariate set).
#
#  Cohort controls == covs_h1 (population density; age/race/ethnicity; income;
#  English; unemployment; education; COVID-19; drug overdose; mental-health
#  measures; excessive drinking; + firearm policies iff include_firearm = TRUE),
#  with the three age-composition shares dropped for the age-defined and
#  age x sex cohorts. Because the set is taken from covs_h1, any change there
#  propagates here.
#
#  Two outcome scalings per cohort:
#    (1) RATE         deaths per 100,000                     (interpretable)
#    (2) STD (wtd-SD) outcome z-scored with the POPULATION-WEIGHTED SD
#                     (comparable across cohorts; coherent with the estimator)
#  Exposure s_g, d_g standardized within stratum (per 1 SD) in both.
#  Youngest band = 10-24 (excludes 0-9). Mirrors Table 1 / Model 2:
#  county + year FE, state-clustered SE, population-weighted.
# ============================================================================
library(kableExtra)   # all other packages are loaded at the top of the script

stopifnot(exists("mort_list"), exists("nodes"), exists("W"), exists("A"),
          exists("my_data_with_spatial_g"), exists("state_codes"),
          exists("covs_h1"), exists("zstd"))

panel  <- as_tibble(my_data_with_spatial_g) %>%
  mutate(GEOID = sprintf("%05d", as.integer(GEOID)), year = as.integer(year))
all_gy <- panel %>% distinct(GEOID, year)

ord <- sprintf("%05d", as.integer(nodes$fr_loc))
if (!is.null(rownames(W))) W <- W[ord, ord]
if (!is.null(rownames(A))) A <- A[ord, ord]
n_nodes <- length(ord)
# ---- send all cohort outputs to a dedicated folder -------------------------
cohort_out <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/cohort_analysis"
dir.create(cohort_out, recursive = TRUE, showWarnings = FALSE)
op <- function(f) file.path(cohort_out, f)   # prefix any filename with the folder

# population-weighted z-score for the standardized-outcome models
# (the exposure uses the unweighted zstd() already defined at the top)
wzscore <- function(x, w) {
  ok <- is.finite(x) & is.finite(w) & w > 0
  m  <- sum(w[ok] * x[ok]) / sum(w[ok])
  s  <- sqrt(sum(w[ok] * (x[ok] - m)^2) / sum(w[ok]))
  (x - m) / s
}

# ---- cohort control set = the H1 covariate set (covs_h1) -------------------
age_covars <- c("ACS_PCT_AGE_U18","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64")
covs_full  <- covs_h1                         # sex & manner cohorts (age shares kept)
covs_noage <- setdiff(covs_h1, age_covars)    # age & age x sex cohorts (age shares dropped)
cat("\n[R1.3] cohort controls (full):  ", paste(covs_full,  collapse=", "), "\n")
cat(  "[R1.3] cohort controls (noage): ", paste(covs_noage, collapse=", "), "\n")

# ============================================================================
#  1.  DEATH-LEVEL extraction: suicides with age band, sex, firearm flag
#      (ICD-10 X60-X84 & Y87.0; firearm = X72-X74; ages 0-9 -> NA)
# ============================================================================
suicide_x <- sprintf("X%02d", 60:84)
firearm3  <- c("X72", "X73", "X74")

decode_age_band <- function(age_raw) {
  unit <- substr(age_raw, 1, 1)
  val  <- suppressWarnings(as.integer(substr(age_raw, 2, 4)))
  yrs  <- dplyr::case_when(unit == "1" ~ val,
                           unit %in% c("2","4","5","6") ~ 0L,
                           TRUE ~ NA_integer_)
  dplyr::case_when(yrs >= 10 & yrs < 25 ~ "lt25",
                   yrs >= 25 & yrs <= 64 ~ "a25_64",
                   yrs >= 65            ~ "a65p",
                   TRUE ~ NA_character_)
}

extract_suicides <- function(mort_df) {
  ucod3 <- substr(mort_df$ucod.icd.10, 1, 3)
  is_suic <- ucod3 %in% suicide_x | mort_df$ucod.icd.10 %in% c("Y870", "Y87.0")
  d <- mort_df[is_suic, , drop = FALSE]
  if (!nrow(d)) return(NULL)
  tibble(
    GEOID    = paste0(state_codes$state_fips[match(d$state.occurrence,
                                                   state_codes$state_abbr)],
                      sprintf("%03d", as.integer(d$county.occurrence))),
    year     = as.integer(d$data.year),
    age_band = decode_age_band(d$age),
    sex      = dplyr::case_when(d$sex %in% c("M","1") ~ "M",
                                d$sex %in% c("F","2") ~ "F",
                                TRUE ~ NA_character_),
    firearm  = substr(d$ucod.icd.10, 1, 3) %in% firearm3
  )
}

deaths <- map_dfr(mort_list, extract_suicides) %>%
  filter(!grepl("NA", GEOID), GEOID %in% unique(panel$GEOID))

# ============================================================================
#  2.  DENOMINATORS: subgroup population per county-year (ACS B01001)
#      10-24 : M 005:010 / F 029:034   (excludes 0-9)
#      25-64 : M 011:019 / F 035:043 ;  65+ : M 020:025 / F 044:049
#      All-age M/F/total summed over full ranges (003:025 / 027:049).
# ============================================================================
mcell <- function(a, b) sprintf("B01001_%03dE", a:b)
get_pop_strata <- function(yr) {
  vars <- sprintf("B01001_%03d", c(3:25, 27:49))
  acs  <- get_acs("county", variables = vars, year = yr, survey = "acs5",
                  output = "wide", cache_table = TRUE)
  rs <- function(cols) rowSums(acs[, cols, drop = FALSE], na.rm = TRUE)
  tibble(
    GEOID = acs$GEOID, year = yr,
    pop_lt25_M  = rs(mcell(5,10)),  pop_25_64_M = rs(mcell(11,19)), pop_65p_M = rs(mcell(20,25)),
    pop_lt25_F  = rs(mcell(29,34)), pop_25_64_F = rs(mcell(35,43)), pop_65p_F = rs(mcell(44,49)),
    pop_M = rs(mcell(3,25)), pop_F = rs(mcell(27,49))
  ) %>%
    mutate(pop_lt25  = pop_lt25_M + pop_lt25_F,
           pop_25_64 = pop_25_64_M + pop_25_64_F,
           pop_65p   = pop_65p_M + pop_65p_F,
           pop_total = pop_M + pop_F)
}
denom <- map_dfr(sort(unique(panel$year)), get_pop_strata) %>%
  mutate(GEOID = sprintf("%05d", as.integer(GEOID)), year = as.integer(year))

# ============================================================================
#  3.  STRATUM SPECS
# ============================================================================
strata <- tribble(
  ~id,          ~label,               ~denom_col,    ~drop_age,
  "lt25",       "Age 10-24",          "pop_lt25",    TRUE,
  "a25_64",     "Age 25-64",          "pop_25_64",   TRUE,
  "a65p",       "Age 65+",            "pop_65p",     TRUE,
  "male",       "Male",               "pop_M",       FALSE,
  "female",     "Female",             "pop_F",       FALSE,
  "lt25_M",     "Age 10-24, Male",    "pop_lt25_M",  TRUE,
  "a25_64_M",   "Age 25-64, Male",    "pop_25_64_M", TRUE,
  "a65p_M",     "Age 65+, Male",      "pop_65p_M",   TRUE,
  "lt25_F",     "Age 10-24, Female",  "pop_lt25_F",  TRUE,
  "a25_64_F",   "Age 25-64, Female",  "pop_25_64_F", TRUE,
  "a65p_F",     "Age 65+, Female",    "pop_65p_F",   TRUE,
  "firearm",    "Firearm",            "pop_total",   FALSE,
  "nonfirearm", "Non-firearm",        "pop_total",   FALSE
)

stratum_counts <- function(id) {
  d <- deaths
  if (id %in% c("lt25","a25_64","a65p")) d <- d %>% filter(age_band == id)
  if (id == "male")   d <- d %>% filter(sex == "M")
  if (id == "female") d <- d %>% filter(sex == "F")
  if (grepl("_M$", id)) d <- d %>% filter(sex == "M", age_band == sub("_M$","",id))
  if (grepl("_F$", id)) d <- d %>% filter(sex == "F", age_band == sub("_F$","",id))
  if (id == "firearm")    d <- d %>% filter(firearm)
  if (id == "nonfirearm") d <- d %>% filter(!firearm)
  d %>% count(GEOID, year, name = "deaths")
}

# ============================================================================
#  4.  PROXIMITY: build s^g and d^g from a county-year rate vector via W & A
# ============================================================================
compute_proximity <- function(rate_df) {
  dt <- as.data.table(rate_df); setkey(dt, year)
  res <- vector("list", 0L)
  for (yr in sort(unique(rate_df$year))) {
    sub <- dt[year == yr]
    idx <- match(sub$GEOID, ord); keep <- !is.na(idx); sub <- sub[keep]; idx <- idx[keep]
    yfull <- numeric(n_nodes); yfull[idx] <- ifelse(is.na(sub$rate), 0, sub$rate)
    s_all <- as.numeric(W %*% yfull); d_all <- as.numeric(A %*% yfull)
    res[[as.character(yr)]] <- tibble(GEOID = ord[idx], year = yr,
                                      s_g = s_all[idx], d_g = d_all[idx])
  }
  bind_rows(res)
}

# ============================================================================
#  5.  FIT one stratum.  std_outcome = TRUE -> outcome z-scored with wtd SD.
#      Controls: covs_noage for age/age x sex cohorts, covs_full otherwise.
# ============================================================================
fit_stratum <- function(spec, std_outcome = FALSE) {
  num  <- stratum_counts(spec$id)
  rate <- all_gy %>%
    left_join(num, by = c("GEOID","year")) %>%
    mutate(deaths = replace_na(deaths, 0L)) %>%
    left_join(denom %>% select(GEOID, year, pop = all_of(spec$denom_col)),
              by = c("GEOID","year")) %>%
    mutate(y_g = if_else(pop > 0, deaths / pop * 1e5, NA_real_))
  
  prox <- compute_proximity(rate %>% transmute(GEOID, year, rate = y_g))
  
  df <- panel %>%
    left_join(rate %>% select(GEOID, year, y_g, pop), by = c("GEOID","year")) %>%
    left_join(prox, by = c("GEOID","year")) %>%
    mutate(s_g = zstd(s_g), d_g = zstd(d_g)) %>%       # exposure standardized (unweighted)
    filter(pop > 0, !is.na(y_g), !is.na(s_g), !is.na(d_g))
  
  if (std_outcome) df <- df %>% mutate(y_g = wzscore(y_g, pop))   # pop-weighted SD
  
  covs <- if (spec$drop_age) covs_noage else covs_full
  fml  <- as.formula(paste0("y_g ~ s_g + d_g + ", paste(covs, collapse = " + "),
                            " | GEOID + year | 0 | state"))
  m <- felm(fml, data = df, weights = df$pop)
  coef <- tidy(m, conf.int = TRUE) %>%
    filter(term == "s_g") %>%
    transmute(id = spec$id, label = spec$label,
              estimate, conf.low, conf.high, std.error, p.value, n_obs = nobs(m))
  list(coef = coef, model = m)
}

# ============================================================================
#  6.  RUN both scalings
# ============================================================================
run_all <- function(std_outcome) {
  fits <- lapply(seq_len(nrow(strata)), function(i) fit_stratum(strata[i, ], std_outcome))
  names(fits) <- strata$id
  list(results = bind_rows(lapply(fits, `[[`, "coef")),
       models  = lapply(fits, `[[`, "model"))
}
rate <- run_all(FALSE); results_rate <- rate$results; models_rate <- rate$models
std  <- run_all(TRUE);  results_std  <- std$results;  models_std  <- std$models

cat("\n===== RATE (deaths per 100,000) =====\n");            print(results_rate)
cat("\n===== STANDARDIZED (population-weighted SD) =====\n"); print(results_std)

# ============================================================================
#  7.  FOREST PLOTS (per scaling + side-by-side facet)
# ============================================================================
lvl <- rev(strata$label)
mk_forest <- function(res, xlab) {
  res %>% mutate(label = factor(label, levels = lvl)) %>%
    ggplot(aes(estimate, label)) +
    geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.6) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0,
                   linewidth = 0.9, colour = "#2c7fb8") +
    geom_point(size = 3, colour = "#2c7fb8") +
    labs(x = xlab, y = NULL) +
    theme_classic(base_size = 12) +
    theme(panel.grid.major.y = element_line(colour = "grey90"))
}
p_rate <- mk_forest(results_rate,
                    "Change in focal-county subgroup suicide mortality (per 100,000)\nfor a 1-SD increase in socially proximal counties' subgroup rate")
p_std  <- mk_forest(results_std,
                    "Standardized change in focal-county subgroup suicide mortality (weighted SD)\nfor a 1-SD increase in socially proximal counties' subgroup rate")

both <- bind_rows(results_rate %>% mutate(scaling = "Deaths per 100,000"),
                  results_std  %>% mutate(scaling = "Standardized (weighted SD)")) %>%
  mutate(label = factor(label, levels = lvl))
p_both <- ggplot(both, aes(estimate, label)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0,
                 linewidth = 0.8, colour = "#2c7fb8") +
  geom_point(size = 2.4, colour = "#2c7fb8") +
  facet_wrap(~ scaling, scales = "free_x") +
  labs(x = "Effect of a 1-SD increase in socially proximal counties' subgroup suicide rate", y = NULL) +
  theme_classic(base_size = 12) +
  theme(panel.grid.major.y = element_line(colour = "grey90"))
print(p_rate); print(p_std); print(p_both)
ggsave(op("R1_3_subgroup_rate.pdf"), p_rate, width = 7,  height = 5)
ggsave(op("R1_3_subgroup_std.pdf"),  p_std,  width = 7,  height = 5)
ggsave(op("R1_3_subgroup_both.pdf"), p_both, width = 10, height = 5)

# ============================================================================
#  8.  FULL REGRESSION SUMMARIES (inspect any stratum, either scaling)
# ============================================================================
summary(models_rate[["lt25"]])    # Age 10-24, deaths per 100k
summary(models_std [["lt25"]])    # Age 10-24, weighted-SD standardized
# for (id in names(models_rate)) { cat("\n=====", id, "=====\n"); print(summary(models_rate[[id]])) }

# ============================================================================
#  9.  PAPER-STYLE per-100k tables (stargazer), grouped & internally consistent.
#      Covariate labels are built PROGRAMMATICALLY from covs_full / covs_noage,
#      so adding R3.3 covariates (or toggling firearm) never breaks the labels.
#      (swap models_rate -> models_std for the standardized-outcome versions)
# ============================================================================
lab_lookup <- c(
  s_g = "Deaths in social proximity $s_{-it}$",
  d_g = "Deaths in spatial proximity $d_{-it}$",
  population_density        = "Population density",
  ACS_PCT_AGE_U18           = "Percent aged below 18",
  ACS_PCT_AGE_18_44         = "Percent aged 18-44",
  ACS_PCT_AGE_45_64         = "Percent aged 45-64",
  prop_asian                = "Percent Asian",
  prop_black                = "Percent Black",
  prop_other                = "Percent Other",
  prop_hispanic             = "Percent Hispanic",
  ACS_MEDIAN_HH_INC         = "Median household income",
  ACS_PCT_ENGL_NOT_WELL     = "Percent with limited English proficiency",
  ACS_PCT_UNEMPLOY          = "Percent unemployed",
  ACS_PCT_LT_HS             = "Percent with less than high school education",
  covid_deaths_per_100k     = "COVID-19 deaths per 100k",
  overdose_deaths_per_100k  = "Drug-overdose deaths per 100k",
  poor_mental_health_days   = "Poor mental health days",
  mental_health_providers   = "Mental health providers",
  frequent_mental_distress  = "Frequent mental distress",
  excessive_drinking        = "Excessive drinking",
  fa_waiting_period         = "Firearm: waiting period",
  fa_cap                    = "Firearm: child-access prevention",
  fa_min_age_21             = "Firearm: minimum purchase age 21")
labs_for <- function(covs) unname(lab_lookup[c("s_g","d_g", covs)])
lab_noage   <- labs_for(covs_noage)   # age & age x sex cohorts
lab_withage <- labs_for(covs_full)    # sex & manner cohorts

# ---- Section 9 (modelsummary version; robust for felm, no stargazer crash) --
ms_gof   <- c("nobs", "r.squared", "adj.r.squared")
ms_stars <- c("*" = .1, "**" = .05, "***" = .01)

# (i) by AGE
modelsummary(
  list("Age 10-24"  = models_rate[["lt25"]],
       "Age 25-64"  = models_rate[["a25_64"]],
       "Age 65+"    = models_rate[["a65p"]]),
  coef_map = lab_lookup, gof_map = ms_gof, stars = ms_stars, escape = FALSE,
  title = "Social proximity and suicide mortality, by age group (deaths per 100,000)",
  output = op("tab_subgroup_age.tex") )

# (ii) by SEX
modelsummary(
  list("Male" = models_rate[["male"]], "Female" = models_rate[["female"]]),
  coef_map = lab_lookup, gof_map = ms_gof, stars = ms_stars, escape = FALSE,
  title = "Social proximity and suicide mortality, by sex (deaths per 100,000)",
  output = op("tab_subgroup_sex.tex") )

# (iii) by MANNER
modelsummary(
  list("Firearm" = models_rate[["firearm"]], "Non-firearm" = models_rate[["nonfirearm"]]),
  coef_map = lab_lookup, gof_map = ms_gof, stars = ms_stars, escape = FALSE,
  title = "Social proximity and suicide mortality, by manner of death (deaths per 100,000)",
  output = op("tab_subgroup_manner.tex"))

# (iv) by AGE x SEX  (the one that was crashing)
modelsummary(
  list("10-24 M" = models_rate[["lt25_M"]],   "25-64 M" = models_rate[["a25_64_M"]],
       "65+ M"   = models_rate[["a65p_M"]],   "10-24 F" = models_rate[["lt25_F"]],
       "25-64 F" = models_rate[["a25_64_F"]], "65+ F"   = models_rate[["a65p_F"]]),
  coef_map = lab_lookup, gof_map = ms_gof, stars = ms_stars, escape = FALSE,
  title = "Social proximity and suicide mortality, by age and sex (deaths per 100,000)",
  output = op("tab_subgroup_agesex.tex"))

# ============================================================================
#  10. ONE COMBINED TABLE: per-100k coefficient + weighted-SD standardized beta
# ============================================================================
stars <- function(p) ifelse(p < .01, "***", ifelse(p < .05, "**", ifelse(p < .1, "*", "")))
cellr <- function(r) sprintf("%.3f%s (%.3f)", r$estimate, stars(r$p.value), r$std.error)

combined <- results_rate %>%
  transmute(id, Subgroup = label,
            `Social proximity (per 100k)` = cellr(.),
            N = format(n_obs, big.mark = ",")) %>%
  left_join(results_std %>% transmute(id, `Social proximity (std. beta, wtd)` = cellr(.)),
            by = "id") %>%
  select(Subgroup, `Social proximity (per 100k)`,
         `Social proximity (std. beta, wtd)`, N)
print(combined)

stargazer(as.data.frame(combined), summary = FALSE, rownames = FALSE, type = "latex",
          title = "Social proximity and county-level suicide mortality across subgroups",
          notes = c("Each row is a separate two-way FE model (county + year FE), population-weighted,",
                    "state-clustered SEs; exposure standardized within stratum (per 1 SD).",
                    "Col. 1: outcome in deaths/100k. Col. 2: outcome standardized using the",
                    "population-weighted SD (comparable across cohorts). Controls = the extended",
                    "H1 set (Table 1; incl. COVID-19, drug-overdose, mental-health, and excessive-",
                    "drinking covariates, plus firearm policies), with age-composition shares",
                    "dropped for the age and age x sex cohorts.",
                    "*p<.1; **p<.05; ***p<.01."),
          op("tab_subgroup_combined.tex"))

kbl(combined %>% select(-Subgroup), format = "latex", booktabs = TRUE, escape = FALSE,
    col.names = c("Social proximity, deaths/100k", "Social proximity, std. $\\beta$ (wtd)", "N"),
    caption = "Social proximity and county-level suicide mortality across subgroups. Each row is a separate two-way fixed effects model (county and year fixed effects), population-weighted, with state-clustered standard errors; the social-proximity exposure is standardized within stratum (per one-standard-deviation increase). Column 1 reports the outcome in deaths per 100,000; column 2 standardizes the outcome using the population-weighted standard deviation, giving a coefficient comparable across cohorts. All models adjust for the extended covariate set of the main analysis (Table 1), including the COVID-19, drug-overdose, mental-health, and excessive-drinking covariates and the firearm policies, with the three age-composition shares omitted for the age and age-by-sex cohorts. $^{*}p<0.1$; $^{**}p<0.05$; $^{***}p<0.01$.") %>%
  pack_rows("By age",             1, 3) %>%
  pack_rows("By sex",             4, 5) %>%
  pack_rows("By age and sex",     6, 11) %>%
  pack_rows("By manner of death", 12, 13) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable(op("tab_subgroup_combined_grouped.tex"))

write.csv(results_rate, op("R1_3_subgroup_rate_results.csv"),      row.names = FALSE)
write.csv(results_std,  op("R1_3_subgroup_weightedSD_results.csv"), row.names = FALSE)