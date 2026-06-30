# =============================================================================
#  "Socio-Spatial Patterns of Suicide Mortality in the United States"
#
#  Pipeline (single file, top to bottom):
#    Sec 1-6  Data build + UPDATED CONFOUNDERS (R3.3): suicide mortality,
#             SDOH/ACS, geometry, COVID-19, drug-overdose (substance-use proxy),
#             County Health Rankings mental-health measures + EXCESSIVE DRINKING
#             (alcohol-availability proxy, CHR v049), RAND firearm laws.
#    Sec 7    Social (SCI) and spatial (1/d) weights -> s_{-i}, d_{-i}
#    Sec 8    ERPO exposures: ERPO_exposure (SCI, out-of-state), InvDist_exposure
#             (inverse-distance, out-of-state), w_i_state, D_it
#    Sec 9    Political affiliation (optional; merged if files present)
#    Sec 10   ANALYSIS 1 — PANEL spatial error model (kNN SEM-FE, splm::spml)
#             + kNN-k robustness (k in 6,8,10)               -> 01_spatial_error_model/
#    Sec 11   ANALYSIS 2 — Updated-covariate regressions     -> 02_updated_covariates/
#
#  Spatial error model is the PANEL two-way-FE SEM (county + year FE, kNN error
#  process). 
#
# =============================================================================

# ----------------------------- CONFIG ----------------------------------------
PROJ <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks"
BASE_DIR <- file.path(PROJ, "revision_reviewer3")

include_firearm <- TRUE      # R3.3 asks for CAP/waiting/min-age -> include by default

# data files (edit if your paths differ)
MORT_ROOT  <- file.path(PROJ, "m_files")
CDC_WONDER <- file.path(PROJ, "cdc_wonder.csv")
SDOH_DIR   <- file.path(PROJ, "SDOH_Covariates/sdoh_csvs")
SCI_TSV    <- "C:/Users/kusha/Desktop/opioid-sci/Data for Paper/SCI/county_county.tsv"
META_CSV   <- "C:/Users/kusha/Downloads/county_data.csv"             # GEOID, state
RAND_XLSX  <- file.path(PROJ, "RAND_DATA/TL-A243-2-v4 State Firearm Law Database 6.0.xlsx")
CHR_DIR    <- file.path(PROJ, "chr_csvs")
POL_0816   <- "C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_08-16.csv"
POL_2020   <- "C:/Users/kusha/Downloads/US_County_Level_Presidential_Results_2020.csv"

# ----------------------------- LIBRARIES --------------------------------------
library(lfe); library(tidyverse); library(igraph); library(tigris)
library(tidycensus); library(geodist); library(sf); library(readxl)
library(lubridate); library(SDPDmod); library(readr); library(stargazer)
library(modelsummary); library(broom); library(data.table); library(ggplot2)
library(viridis); library(spdep); library(splm)

zstd <- function(x) (x - mean(x, na.rm = TRUE)) / sd(x, na.rm = TRUE)
pad5 <- function(x) sprintf("%05d", as.integer(x))
log_msg <- function(...) cat(sprintf("[%s] ", format(Sys.time(), "%H:%M:%S")), ..., "\n")

# folders, one per analysis
dir_sem  <- file.path(BASE_DIR, "01_spatial_error_model")
dir_cov  <- file.path(BASE_DIR, "02_updated_covariates")
dir_resp <- file.path(BASE_DIR, "00_response")
for (d in c(BASE_DIR, dir_sem, dir_cov, dir_resp))
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
op_sem  <- function(f) file.path(dir_sem,  f)
op_cov  <- function(f) file.path(dir_cov,  f)
op_resp <- function(f) file.path(dir_resp, f)

# =============================================================================
#  1. SUICIDE MORTALITY  (NVSS Multiple Cause-of-Death, 2010-2022)
# =============================================================================
log_msg("Section 1: reading mortality files ...")
read_us_mortality_data <- function(file_path) {
  positions <- list(
    resident.status=c(20,20), state.occurrence=c(21,22), county.occurrence=c(23,25),
    state.residence=c(29,30), county.residence=c(35,37), education.1989=c(61,62),
    education.2003=c(63,63), education.flag=c(64,64), `month.of-death`=c(65,66),
    sex=c(69,69), age=c(70,73), age.flag=c(74,74), age.recode.52=c(75,76),
    age.recode.27=c(77,78), age.recode.12=c(79,80), age.recode.22=c(81,82),
    place.of.death=c(83,83), marital.status=c(84,84), `day.of-week.of-death`=c(85,85),
    data.year=c(102,105), injury.at.work=c(106,106), `manner.of-death`=c(107,107),
    `method.of-disposition`=c(108,108), autopsy=c(109,109), `activity.code`=c(144,144),
    `place.of.injury`=c(145,145), `ucod.icd.10`=c(146,149), `ucod.recode.358`=c(150,152),
    `ucod.recode.113`=c(154,156), `ucod.recode.130`=c(157,159), `ucod.recode.39`=c(160,161),
    entity.n=c(163,164), race=c(445,446), race.recode.3=c(449,449), race.recode.5=c(450,450),
    hispanic=c(484,486), hispanic.recode=c(488,488), race.recode.40=c(489,490))
  ss <- vapply(positions, function(x) as.integer(x[1]), integer(1))
  ee <- vapply(positions, function(x) as.integer(x[2]), integer(1))
  readr::read_fwf(file_path, readr::fwf_positions(start = ss, end = ee, col_names = names(positions)))
}
mfile <- function(rel) file.path(MORT_ROOT, rel)
mort_list <- list(
  `2010`=read_us_mortality_data(mfile("MULT2010.USPSAllCnty/MULT2010.USAllCnty.txt")),
  `2011`=read_us_mortality_data(mfile("MULT2011.USPSAllCnty/MULT2011.USAllCnty.txt")),
  `2012`=read_us_mortality_data(mfile("MULT2012.USPSAllCnty/MULT2012.USAllCnty.txt")),
  `2013`=read_us_mortality_data(mfile("MULT2013.USPSAllCnty/MULT2013.USAllCnty.txt")),
  `2014`=read_us_mortality_data(mfile("MULT2014.USPSAllCnty/MULT2014.USAllCnty.txt")),
  `2015`=read_us_mortality_data(mfile("MULT2015.USPSAllCnty/MULT2015.USAllCnty.txt")),
  `2016`=read_us_mortality_data(mfile("MULT2016.USPSAllCnty/MULT2016.USAllCnty.txt")),
  `2017`=read_us_mortality_data(mfile("MULT2017.USPSAllCnty/MULT2017.USAllCnty.txt")),
  `2018`=read_us_mortality_data(mfile("MULT2018.USPSAllCnty/Mort2018US.AllCnty.txt")),
  `2019`=read_us_mortality_data(mfile("MULT2019.USPSAllCnty/MULT2019US.AllCnty.txt")),
  `2020`=read_us_mortality_data(mfile("MULT2020.AllCnty/MULT2020.USAllCnty.txt")),
  `2021`=read_us_mortality_data(mfile("MULT2021.AllCnty/MULT2021US.AllCnty.txt")),
  `2022`=read_us_mortality_data(mfile("MULT2022_AllCnty/MULT2022US.AllCnty.txt")))

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
    group_by(year, state_abbr, county_fips) %>% summarise(total_deaths = n(), .groups="drop") %>%
    left_join(state_codes, by = "state_abbr") %>%
    mutate(GEOID = paste0(state_fips, county_fips)) %>%
    select(year, state = state_abbr, state_fips, county_fips, GEOID, total_deaths)
}
suicide_panel_all <- bind_rows(lapply(mort_list, summarise_suicides), .id="year_list") %>%
  mutate(year = as.integer(year)) %>%
  select(year, state, state_fips, county_fips, GEOID, total_deaths) %>% arrange(year, GEOID)

cdc_Wonder_data <- read.csv(CDC_WONDER, colClasses = c(GEOID="character"), stringsAsFactors = FALSE)
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
    mutate(year=y, state_fips=substr(GEOID,1,2), county_fips=substr(GEOID,3,5)) %>%
    left_join(state_codes, by="state_fips") %>% rename(state=state_abbr) %>%
    left_join(suicide_panel_all %>% filter(year==y) %>% select(GEOID,total_deaths), by="GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})
exclude_geoids <- c("02158","02261","46102","15005")
complete_suicide_panel_ref <- complete_suicide_panel_ref %>% filter(!GEOID %in% exclude_geoids)
ref_geoids_all <- unique(cdc_Wonder_data$GEOID)
additional_panel <- map_dfr(c(2021L,2022L), function(y) {
  tibble(GEOID = ref_geoids_all) %>%
    mutate(year=y, state_fips=substr(GEOID,1,2), county_fips=substr(GEOID,3,5)) %>%
    left_join(state_codes, by="state_fips") %>% rename(state=state_abbr) %>%
    left_join(suicide_panel_all %>% filter(year==y) %>% select(GEOID,total_deaths), by="GEOID") %>%
    mutate(total_deaths = replace_na(total_deaths, 0)) %>%
    select(year, state, state_fips, county_fips, GEOID, total_deaths)
})
complete_suicide_panel_ref <- bind_rows(complete_suicide_panel_ref, additional_panel) %>%
  filter(!GEOID %in% exclude_geoids)

# =============================================================================
#  2. SOCIOECONOMIC COVARIATES  (AHRQ SDOH 2010-2020 + ACS 2021-2022)
# =============================================================================
log_msg("Section 2: SDOH + ACS covariates ...")
files <- list.files(SDOH_DIR, pattern = "^sdoh_\\d{4}\\.csv$", full.names = TRUE)
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
  left_join(my_panel, by = c("GEOID"="COUNTYFIPS","year"="YEAR"))

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
#  3. GEOMETRY: centroids, density, death rates, distance matrix
# =============================================================================
log_msg("Section 3: geometry, density, distance matrix ...")
counties_sf <- counties(cb = TRUE, class = "sf")
selected_counties <- counties_sf[counties_sf$GEOID %in% unique(suicide_mortality$GEOID), ]
centroids <- st_centroid(selected_counties); centroids <- centroids[order(centroids$GEOID), ]
coords <- st_coordinates(centroids)
geoid_lat_lng <- data.frame(GEOID = sort(selected_counties$GEOID), Longitude = coords[,1], Latitude = coords[,2])
suicide_mortality <- left_join(suicide_mortality, geoid_lat_lng, by = "GEOID")

counties_area <- counties(year = 2018, cb = TRUE) %>% st_transform(crs = 5070) %>%
  mutate(area_sq_km = as.numeric(st_area(geometry))/1e6)
counties_area <- counties_area %>% filter(GEOID %in% suicide_mortality$GEOID) %>% select(GEOID, area_sq_km) %>% st_drop_geometry()
suicide_mortality <- merge(suicide_mortality, counties_area, by = "GEOID") %>%
  mutate(population_density = ACS_TOT_POP_WT/area_sq_km,
         death_rates_per_100_k = if_else(ACS_TOT_POP_WT > 0, (total_deaths/ACS_TOT_POP_WT)*1e5, NA_real_))

my_data_with_spatial_g <- suicide_mortality %>% as_tibble() %>%
  mutate(GEOID = pad5(GEOID), year = as.integer(year))

unique_counties <- my_data_with_spatial_g %>% distinct(GEOID, Longitude, Latitude) %>% arrange(GEOID)
distance_km <- geodist::geodist(unique_counties[, c("Longitude","Latitude")], measure = "geodesic")/1000
rownames(distance_km) <- colnames(distance_km) <- unique_counties$GEOID

# =============================================================================
#  4. NEW CONFOUNDERS (R3.3): COVID, overdose, CHR mental health + excessive
#     drinking (alcohol proxy), RAND firearm
# =============================================================================
log_msg("Section 4: building updated confounders ...")
pop_lookup <- my_data_with_spatial_g %>% select(GEOID, year, ACS_TOT_POP_WT) %>% distinct()
all_geoids <- unique(my_data_with_spatial_g$GEOID)
scaffold   <- expand.grid(GEOID = all_geoids, year = 2010:2022, stringsAsFactors = FALSE) %>% as_tibble()

## 4a. COVID-19 cumulative deaths per 100k (NYT; 0 for 2010-2019) -------------
covid_raw <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", show_col_types = FALSE)
covid_panel <- covid_raw %>% filter(!is.na(fips)) %>%
  mutate(GEOID = pad5(fips), year = year(date)) %>%
  group_by(GEOID, year) %>% summarise(covid_deaths_cum = max(deaths, na.rm = TRUE), .groups = "drop")
covid_full <- scaffold %>% left_join(covid_panel, by = c("GEOID","year")) %>%
  mutate(covid_deaths_cum = replace_na(covid_deaths_cum, 0)) %>%
  left_join(pop_lookup, by = c("GEOID","year")) %>%
  mutate(covid_deaths_per_100k = if_else(ACS_TOT_POP_WT > 0, covid_deaths_cum/ACS_TOT_POP_WT*1e5, 0)) %>%
  select(GEOID, year, covid_deaths_per_100k)

## 4b. Drug-overdose deaths per 100k (EXCLUDES X60-X64 = suicide outcome) -----
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

## 4c. County Health Rankings: mental-health measures + EXCESSIVE DRINKING -----
##     Excessive drinking (v049) is the alcohol-availability/control proxy,
##     chosen over alcohol-related death rates because it lies on the causal
##     pathway: alcohol control laws -> excessive drinking -> alcohol-related death.
dir.create(CHR_DIR, showWarnings = FALSE, recursive = TRUE)
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
  dest <- file.path(CHR_DIR, paste0("analytic_data", yr, ".csv"))
  if (!file.exists(dest)) tryCatch(download.file(chr_urls[[yr]], dest, mode="wb", quiet=TRUE),
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
  out <- tibble(GEOID = pad5(d[[fips_col]]), year = yr)
  for (nm in names(mh_patterns)) {
    col <- grep(mh_patterns[[nm]], names(d), value = TRUE)[1]
    out[[nm]] <- if (!is.na(col)) suppressWarnings(as.numeric(d[[col]])) else NA_real_
  }
  out %>% filter(!grepl("NA", GEOID), substr(GEOID,3,5) != "000")
}
chr_files <- list.files(CHR_DIR, pattern = "analytic_data\\d{4}\\.csv$", full.names = TRUE)
chr_panel <- map_dfr(chr_files, read_chr_year) %>% distinct(GEOID, year, .keep_all = TRUE)

## 4d. RAND firearm policies (state x year) -----------------------------------
fa_cols <- c("fa_waiting_period","fa_cap","fa_min_age","fa_min_age_21")
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
  firearm_policy <- expand_grid(state = unique(my_data_with_spatial_g$state), year = 2010:2022L) %>%
    mutate(fa_waiting_period = 0L, fa_cap = 0L, fa_min_age = 0L, fa_min_age_21 = 0L)
}

## 4e. merge all new confounders ----------------------------------------------
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  left_join(covid_full,    by = c("GEOID","year")) %>%
  left_join(overdose_full, by = c("GEOID","year")) %>%
  left_join(chr_panel,     by = c("GEOID","year")) %>%
  left_join(firearm_policy, by = c("state","year")) %>%
  mutate(covid_deaths_per_100k = replace_na(covid_deaths_per_100k, 0),
         overdose_deaths_per_100k = replace_na(overdose_deaths_per_100k, 0),
         across(any_of(fa_cols), ~ replace_na(.x, 0L)))
my_data_with_spatial_g <- my_data_with_spatial_g %>% arrange(GEOID, year) %>% group_by(GEOID) %>%
  fill(any_of(names(mh_patterns)), .direction = "downup") %>% ungroup()

# =============================================================================
#  5. STANDARDIZE continuous covariates; define covariate sets
# =============================================================================
log_msg("Section 5: standardizing covariates ...")
base_covars    <- c("population_density","ACS_PCT_AGE_U18","ACS_PCT_AGE_18_44","ACS_PCT_AGE_45_64",
                    "prop_asian","prop_black","prop_other","prop_hispanic","ACS_MEDIAN_HH_INC",
                    "ACS_PCT_ENGL_NOT_WELL","ACS_PCT_UNEMPLOY","ACS_PCT_LT_HS")
health_covars  <- intersect(c("covid_deaths_per_100k","overdose_deaths_per_100k", names(mh_patterns)),
                            names(my_data_with_spatial_g))
firearm_covars <- intersect(c("fa_waiting_period","fa_cap","fa_min_age_21"), names(my_data_with_spatial_g))

std_continuous <- c(base_covars, "covid_deaths_per_100k", "overdose_deaths_per_100k",
                    intersect(names(mh_patterns), names(my_data_with_spatial_g)))
my_data_with_spatial_g[std_continuous] <- lapply(my_data_with_spatial_g[std_continuous], zstd)
my_data_with_spatial_g <- my_data_with_spatial_g %>%
  mutate(across(any_of(names(mh_patterns)), ~ replace_na(.x, 0)))

# =============================================================================
#  6. SOCIAL & SPATIAL WEIGHTS  ->  W, A, s_{-it}, d_{-it}
# =============================================================================
log_msg("Section 6: social/spatial weights and proximity exposures ...")
social_df <- suicide_mortality %>% mutate(GEOID = pad5(GEOID)) %>%
  select(GEOID, ACS_TOT_POP_WT, death_rates_per_100_k)
colnames(social_df)[1] <- "fr_loc"; colnames(social_df)[3] <- "deaths_per_capita"
social_df <- social_df[order(social_df$fr_loc), ] %>% distinct(fr_loc, .keep_all = TRUE)

df_0 <- read_tsv(SCI_TSV, show_col_types = FALSE)
df_1 <- df_0 %>% mutate(user_loc = pad5(user_loc), fr_loc = pad5(fr_loc)) %>%
  filter(user_loc %in% social_df$fr_loc & fr_loc %in% social_df$fr_loc) %>%
  filter(!duplicated(paste0(pmax(user_loc, fr_loc), pmin(user_loc, fr_loc))))
nodes <- social_df %>% select(fr_loc) %>% distinct()
k <- graph.data.frame(df_1 %>% select(user_loc, fr_loc, scaled_sci), directed = FALSE, vertices = nodes)

population <- suicide_mortality %>% mutate(GEOID = pad5(GEOID)) %>%
  group_by(GEOID) %>% summarise(population = round(mean(ACS_TOT_POP_WT, na.rm = TRUE))) %>%
  filter(GEOID %in% nodes$fr_loc) %>% arrange(match(GEOID, nodes$fr_loc))
pop_vector <- population$population

W <- as_adjacency_matrix(k, attr = "scaled_sci", sparse = TRUE); diag(W) <- 0
W <- sweep(as.matrix(W), 2, pop_vector, `*`); W <- sweep(W, 1, rowSums(W), `/`); W[is.na(W)] <- 0
ord <- pad5(nodes$fr_loc)
rownames(W) <- colnames(W) <- ord
A <- InvDistMat(distance_km[ord, ord]); diag(A) <- 0; A <- sweep(A, 1, rowSums(A), '/')
rownames(A) <- colnames(A) <- ord

dt <- as.data.table(my_data_with_spatial_g); setkey(dt, GEOID)
dt[, `:=`(s_minus_i = NA_real_, d_minus_i = NA_real_)]
for (yr in sort(unique(dt$year))) {
  idx <- match(dt[year == yr, GEOID], ord)
  y_t <- dt[year == yr][order(idx), death_rates_per_100_k]
  oo  <- order(idx)
  dt[year == yr, s_minus_i := as.numeric(W[idx[oo], , drop=FALSE] %*% y_t)[order(oo)]]
  dt[year == yr, d_minus_i := as.numeric(A[idx[oo], , drop=FALSE] %*% y_t)[order(oo)]]
}
dt[, s_minus_i_z := as.numeric(scale(s_minus_i))]
dt[, d_minus_i_z := as.numeric(scale(d_minus_i))]
my_data_with_spatial_g <- as_tibble(dt)

# =============================================================================
#  7. ERPO EXPOSURES (out-of-state): ERPO_exposure, InvDist_exposure, D_it
# =============================================================================
log_msg("Section 7: ERPO exposures ...")
sci  <- fread(SCI_TSV)
meta <- fread(META_CSV, select = c("GEOID","state")) |> unique()
if (!all(c("i_fips","j_fips") %in% names(sci))) setnames(sci, names(sci)[1:2], c("i_fips","j_fips"))
sci[, `:=`(i_fips = pad5(i_fips), j_fips = pad5(j_fips))]
meta[, GEOID := pad5(GEOID)]
if (!"SCI" %in% names(sci)) {
  num_cols <- setdiff(names(Filter(is.numeric, sci)), c("i_fips","j_fips"))
  setnames(sci, num_cols[1], "SCI")
}
if (!"i_state" %in% names(sci)) { sci <- sci[meta, on = .(i_fips = GEOID), nomatch = 0]; setnames(sci, "state", "i_state") }
if (!"j_state" %in% names(sci)) { sci <- sci[meta, on = .(j_fips = GEOID), nomatch = 0, allow.cartesian = TRUE]; setnames(sci, "state", "j_state") }
sci[, totalSCI := sum(SCI), by = i_fips]
sci[, w_ij := SCI / totalSCI]
w_i_state <- sci[i_state != j_state, .(w_is = sum(w_ij)), by = .(i_fips, j_state)]

policy_data <- data.table(
  state      = c("CT","IN","CA","WA","OR","FL","VT","MD","RI","DE",
                 "MA","NJ","IL","NY","DC","CO","NV","HI","NM","VA"),
  start_year = c(1999,2005,2016,2016,2018,2018,2018,2018,2018,2018,
                 2018,2019,2019,2019,2019,2020,2020,2020,2020,2020))
years_panel <- sort(unique(my_data_with_spatial_g$year))
erpo_cal <- CJ(state = unique(meta$state), year = years_panel)[policy_data, on = "state"][
  , ERPO_active := as.integer(!is.na(start_year) & year >= start_year)][, start_year := NULL]

expo <- w_i_state[erpo_cal, on = .(j_state = state), allow.cartesian = TRUE][
  ERPO_active == 1L, .(ERPO_exposure = sum(w_is)), by = .(i_fips, year)]

# inverse-distance exposure reuses A (row-normalized 1/d), out-of-state only
gravity_dt <- as.data.table(as.table(A)); setnames(gravity_dt, c("i_fips","j_fips","w_ij"))
gravity_dt[, `:=`(i_fips = pad5(i_fips), j_fips = pad5(j_fips))]
gravity_dt <- gravity_dt[meta, on = .(j_fips = GEOID), nomatch = 0]; setnames(gravity_dt, "state", "j_state")
gravity_dt <- gravity_dt[meta, on = .(i_fips = GEOID), nomatch = 0]; setnames(gravity_dt, "state", "i_state")
expo_dist <- gravity_dt[erpo_cal, on = .(j_state = state), allow.cartesian = TRUE][
  i_state != j_state & ERPO_active == 1L, .(InvDist_exposure = sum(w_ij)), by = .(i_fips, year)]

my_data_with_spatial_g <- my_data_with_spatial_g %>%
  left_join(expo,      by = c("GEOID" = "i_fips", "year")) %>%
  left_join(expo_dist, by = c("GEOID" = "i_fips", "year")) %>%
  mutate(ERPO_exposure = replace_na(ERPO_exposure, 0),
         InvDist_exposure = replace_na(InvDist_exposure, 0)) %>%
  left_join(as_tibble(policy_data), by = "state") %>%
  mutate(start_year   = replace_na(start_year, 0L),
         ever_treated = as.integer(start_year > 0L),
         post         = as.integer(year >= start_year & start_year > 0L),
         D_it         = ever_treated * post,
         state_year   = interaction(state, year, drop = TRUE))

# =============================================================================
#  8. POLITICAL AFFILIATION (optional; merged if files present)
# =============================================================================
have_political <- file.exists(POL_0816) && file.exists(POL_2020)
if (have_political) {
  log_msg("Section 8: political affiliation ...")
  e1 <- read_csv(POL_0816, show_col_types = FALSE)
  pol1 <- bind_rows(
    e1 %>% transmute(GEOID = pad5(fips_code), political_affiliation = as.integer(gop_2008 > dem_2008), election_year = 2008),
    e1 %>% transmute(GEOID = pad5(fips_code), political_affiliation = as.integer(gop_2012 > dem_2012), election_year = 2012),
    e1 %>% transmute(GEOID = pad5(fips_code), political_affiliation = as.integer(gop_2016 > dem_2016), election_year = 2016))
  e2 <- read_csv(POL_2020, show_col_types = FALSE)
  pol2 <- e2 %>% transmute(GEOID = pad5(county_fips), political_affiliation = as.integer(votes_gop > votes_dem), election_year = 2020)
  all_pol <- bind_rows(pol1, pol2) %>% mutate(GEOID = ifelse(GEOID == "46113", "46102", GEOID)) %>%
    filter(!is.na(GEOID), !is.na(political_affiliation))
  ymap <- tibble(year = 2010:2022,
                 election_year = c(2008,2008,2012,2012,2012,2012,2016,2016,2016,2016,2020,2020,2020))
  political_panel <- ymap %>% left_join(all_pol, by = "election_year") %>%
    select(GEOID, year, political_affiliation)
  my_data_with_spatial_g <- my_data_with_spatial_g %>% left_join(political_panel, by = c("GEOID","year"))
} else {
  message("Political-affiliation files not found; political_affiliation omitted.")
}
pol_covar <- if ("political_affiliation" %in% names(my_data_with_spatial_g)) "political_affiliation" else character(0)

## ---------------------------------------------------------------------------
## COVARIATE SETS (the reviewer's collinearity caveat is enforced here)
##  - county + year FE models  : base + health + firearm + political   (covs_h1)
##  - county + state_year FE    : base + health + political (firearm absorbed)
## ---------------------------------------------------------------------------
covs_h1   <- intersect(c(base_covars, health_covars,
                         if (isTRUE(include_firearm)) firearm_covars, pol_covar),
                       names(my_data_with_spatial_g))
covs_erpo <- intersect(c(base_covars, health_covars, pol_covar),
                       names(my_data_with_spatial_g))
rhs <- function(v) paste(v, collapse = " + ")
W_pop <- my_data_with_spatial_g$ACS_TOT_POP_WT
log_msg(sprintf("covs_h1 (%d): %s", length(covs_h1), paste(covs_h1, collapse=", ")))

# =============================================================================
#  9. ANALYSIS 1 (R3.4a) — PANEL SPATIAL ERROR MODEL  (splm::spml, SEM-FE)
#     Two-way (county + year) fixed effects + a spatial ERROR process, fit on
#     the full balanced panel (NOT per-year cross-sections). Error process is a
#     k-nearest-neighbour contiguity matrix; s_{-i}/d_{-i} stay as regressors.
#     Saves summaries + coefficient CSVs; plots betas for s_{-it} and d_{-it}.
# =============================================================================
log_msg("ANALYSIS 1: panel spatial error model (kNN SEM-FE) ...")
KNN_K <- 5      # neighbours for the kNN contiguity error process

## ---- 1. Balanced, complete-case estimation sample ---------------------------
model_vars <- c("death_rates_per_100_k", "s_minus_i_z", "d_minus_i_z", covs_h1)
yrs_all <- sort(unique(my_data_with_spatial_g$year)); T_all <- length(yrs_all)
cc  <- my_data_with_spatial_g[stats::complete.cases(my_data_with_spatial_g[, model_vars]), ]
bal <- cc %>% dplyr::count(GEOID) %>% dplyr::filter(n == T_all) %>% dplyr::pull(GEOID)

cty_poly <- tigris::counties(cb = TRUE, year = 2021, class = "sf") %>%
  dplyr::mutate(GEOID = sprintf("%05d", as.integer(GEOID))) %>% sf::st_transform(5070)

keep <- sort(Reduce(intersect, list(bal, ord, cty_poly$GEOID)))
poly <- cty_poly[match(keep, cty_poly$GEOID), ]; stopifnot(identical(poly$GEOID, keep))

pdat <- my_data_with_spatial_g %>% dplyr::filter(GEOID %in% keep) %>%
  dplyr::arrange(GEOID, year) %>% as.data.frame()
stopifnot(nrow(pdat) == length(keep) * T_all)      # must be balanced
fm <- as.formula(paste("death_rates_per_100_k ~ s_minus_i_z + d_minus_i_z +",
                       paste(covs_h1, collapse = " + ")))

## ---- 2. Row-normalized kNN error-process listw on `keep` --------------------
nb <- spdep::knn2nb(spdep::knearneigh(sf::st_coordinates(sf::st_centroid(sf::st_geometry(poly))), k = KNN_K))
lw_knn <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)

## ---- 3. OLS-FE baseline on the SAME sample (state-clustered, for SE compare)-
ols_fe <- felm(as.formula(paste("death_rates_per_100_k ~ s_minus_i_z + d_minus_i_z +",
                                paste(covs_h1, collapse = " + "), "| GEOID + year | 0 | state")),
               data = pdat)

## ---- 4. Fit the PANEL SEM-FE (two-way within + spatial error 'b') -----------
log_msg(sprintf("  fitting spml (kNN, k = %d) ...", KNN_K))
sem_fit <- tryCatch(
  spml(fm, data = pdat, index = c("GEOID","year"), listw = lw_knn,
       model = "within", effect = "twoways", spatial.error = "b", lag = FALSE),
  error = function(e) { message("  spml failed: ", conditionMessage(e)); NULL })

if (!is.null(sem_fit)) {
  sink(op_sem("sem_fe_kNN_summary.txt"))
  cat(sprintf("===== PANEL SEM-FE (kNN, k = %d) — two-way within, spatial.error = 'b' =====\n", KNN_K))
  print(summary(sem_fit))
  cat("\n--- spatial error parameter (rho) ---\n")
  print(tryCatch(summary(sem_fit)$ErrCompTable, error = function(e) sem_fit$errcomp))
  sink()
  ct_sem <- as.data.frame(summary(sem_fit)$CoefTable); ct_sem$term <- rownames(ct_sem)
  write.csv(ct_sem, op_sem("sem_fe_kNN_coefficients.csv"), row.names = FALSE)
  saveRDS(sem_fit, op_sem("sem_fe_kNN_model.rds"))
}

## ---- 5. Combined coefficient table + plot for s_{-it} and d_{-it} -----------
grab <- function(M, term, ci) if (term %in% rownames(M)) M[term, ci] else NA_real_
coef_rows <- list()
for (tm in c("s_minus_i_z","d_minus_i_z"))
  coef_rows[[paste0("OLS_",tm)]] <- data.frame(
    model = "OLS-FE (state-clustered)", term = tm,
    estimate = coef(ols_fe)[tm], se = summary(ols_fe)$coefficients[tm, "Cluster s.e."])
if (!is.null(sem_fit)) {
  ctm <- as.matrix(summary(sem_fit)$CoefTable); rownames(ctm) <- trimws(rownames(ctm))
  for (tm in c("s_minus_i_z","d_minus_i_z"))
    coef_rows[[paste0("SEM_",tm)]] <- data.frame(
      model = "Panel SEM-FE (kNN error)", term = tm,
      estimate = grab(ctm, tm, 1), se = grab(ctm, tm, 2))
}
coef_df <- dplyr::bind_rows(coef_rows) %>%
  mutate(conf.low = estimate - 1.96*se, conf.high = estimate + 1.96*se,
         term = recode(term, s_minus_i_z = "Social proximity  s_{-it}",
                       d_minus_i_z = "Spatial proximity  d_{-it}"),
         model = factor(model, levels = c("OLS-FE (state-clustered)",
                                          "Panel SEM-FE (kNN error)")))
write.csv(coef_df, op_sem("sem_fe_coefficients_combined.csv"), row.names = FALSE)

p_coef <- ggplot(coef_df, aes(estimate, model, colour = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~ term, scales = "free_x") +
  scale_colour_viridis_d(option = "D", end = 0.9, guide = "none") +
  labs(x = "Coefficient (deaths per 100k, per 1 SD)  with 95% CI", y = NULL,
       title = "Panel SEM-FE: social and spatial proximity under a kNN error process") +
  theme_classic(base_size = 12) +
  theme(panel.grid.major.y = element_line(colour = "grey90"))
ggsave(op_sem("sem_fe_coefficient_plot.pdf"), p_coef, width = 11, height = 4.5)
print(coef_df); log_msg(sprintf("  panel SEM-FE done. N=%d counties, T=%d, obs=%d", length(keep), T_all, nrow(pdat)))

# =============================================================================
#  9b. PANEL SEM-FE robustness over the kNN parameter k in {6, 8, 10}
#     Same panel two-way-FE SEM, varying the number of neighbours in the error
#     process. Reports s_{-it}, d_{-it} and rho per k; saves tables + plot.
#     Reuses pdat, poly (GEOID-sorted), fm from Section 9.
# =============================================================================
log_msg("ANALYSIS 1b: kNN error-process robustness over k ...")
K_GRID <- c(6, 8, 10)
ctr    <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(poly)))   # poly is GEOID-sorted

knn_rows <- list(); knn_fits <- list()
for (k in K_GRID) {
  log_msg(sprintf("  kNN-SEM-FE: k = %d ...", k))
  lw_k <- spdep::nb2listw(spdep::knn2nb(spdep::knearneigh(ctr, k = k)),
                          style = "W", zero.policy = TRUE)
  fit <- tryCatch(
    spml(fm, data = pdat, index = c("GEOID","year"), listw = lw_k,
         model = "within", effect = "twoways", spatial.error = "b", lag = FALSE),
    error = function(e) { message("  spml failed (k=", k, "): ", conditionMessage(e)); NULL })
  if (is.null(fit)) next
  knn_fits[[paste0("k", k)]] <- fit
  
  sink(op_sem(sprintf("sem_fe_kNN_k%d_summary.txt", k)))
  cat(sprintf("===== kNN-SEM-FE (k = %d) — two-way within, spatial.error='b' =====\n", k))
  print(summary(fit)); sink()
  
  sm <- summary(fit); ct <- sm$CoefTable
  if (is.null(ct) || is.null(dim(ct)) || nrow(ct) == 0) {
    message("  empty CoefTable (k=", k, ") — skipping"); next
  }
  ct <- as.matrix(ct); rownames(ct) <- trimws(rownames(ct))
  # rho is row "rho" of CoefTable on this build; cols are est/se/t/p (1/2/3/4)
  rho_est <- if ("rho" %in% rownames(ct)) ct["rho", 1] else NA_real_
  rho_p   <- if ("rho" %in% rownames(ct)) ct["rho", 4] else NA_real_
  for (tm in c("s_minus_i_z","d_minus_i_z")) {
    if (!tm %in% rownames(ct)) { message("  term not found (k=", k, "): ", tm); next }
    knn_rows[[paste0("k",k,"_",tm)]] <- data.frame(
      k = k, term = tm,
      estimate = ct[tm, 1], se = ct[tm, 2], p = ct[tm, 4],
      rho = rho_est, rho_p = rho_p)
  }
}
saveRDS(knn_fits, op_sem("sem_fe_kNN_models.rds"))

## ---- summary tables ---------------------------------------------------------
knn_tab <- dplyr::bind_rows(knn_rows) %>%
  mutate(conf.low = estimate - 1.96*se, conf.high = estimate + 1.96*se,
         stars = as.character(cut(p, c(-Inf,.01,.05,.1,Inf), labels = c("***","**","*",""))),
         cell  = sprintf("%.3f%s (%.3f)", estimate, stars, se),
         label = recode(term, s_minus_i_z = "Social proximity  s_{-it}",
                        d_minus_i_z = "Spatial proximity  d_{-it}"))
write.csv(knn_tab, op_sem("sem_fe_kNN_robustness_table.csv"), row.names = FALSE)

# wide layout: rows = coefficient (+ rho), cols = k
wide <- knn_tab %>% select(label, k, cell) %>%
  tidyr::pivot_wider(names_from = k, values_from = cell, names_prefix = "k = ")
if (any(!is.na(knn_tab$rho))) {
  rho_line <- knn_tab %>% distinct(k, rho, rho_p) %>%
    mutate(cell = sprintf("%.3f%s", rho,
                          ifelse(rho_p<.01,"***",ifelse(rho_p<.05,"**",ifelse(rho_p<.1,"*",""))))) %>%
    select(k, cell) %>%
    tidyr::pivot_wider(names_from = k, values_from = cell, names_prefix = "k = ") %>%
    mutate(label = "Spatial error rho", .before = 1)
  wide <- dplyr::bind_rows(wide, rho_line)
}
write.csv(wide, op_sem("sem_fe_kNN_robustness_table_wide.csv"), row.names = FALSE)
print(wide)

## ---- coefficient plot -------------------------------------------------------
p_knn <- ggplot(knn_tab, aes(estimate, factor(k), colour = factor(k))) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.5) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0, linewidth = 0.9) +
  geom_point(size = 3) +
  facet_wrap(~ label, scales = "free_x") +
  scale_colour_viridis_d(option = "D", end = 0.85, name = "k (neighbours)") +
  labs(x = "Coefficient (deaths per 100k, per 1 SD) with 95% CI",
       y = "k nearest neighbours",
       title = "Panel SEM-FE robustness: social and spatial proximity across k") +
  theme_classic(base_size = 12) +
  theme(panel.grid.major.y = element_line(colour = "grey90"))
ggsave(op_sem("sem_fe_kNN_coefficient_plot.pdf"), p_knn, width = 11, height = 4.5)
log_msg("kNN robustness over k done.")

# =============================================================================
#  10. ANALYSIS 2 (R3.3) — UPDATED-COVARIATE REGRESSIONS
# =============================================================================
log_msg("ANALYSIS 2: updated-covariate regressions ...")
fml <- function(lhs_rhs, fe, cov) as.formula(sprintf("%s + %s | %s | 0 | state", lhs_rhs, rhs(cov), fe))

# county + year FE (firearm + political admissible)
m_direct <- felm(fml("death_rates_per_100_k ~ D_it",        "GEOID + year", covs_h1), data = my_data_with_spatial_g, weights = W_pop)
m_prox   <- felm(fml("death_rates_per_100_k ~ s_minus_i_z", "GEOID + year", covs_h1), data = my_data_with_spatial_g, weights = W_pop)
m_prox2  <- felm(fml("death_rates_per_100_k ~ s_minus_i_z + d_minus_i_z", "GEOID + year", covs_h1), data = my_data_with_spatial_g, weights = W_pop)
# county + state_year FE (out-of-state exposure; firearm absorbed -> covs_erpo)
m_social <- felm(fml("death_rates_per_100_k ~ ERPO_exposure",                    "GEOID + state_year", covs_erpo), data = my_data_with_spatial_g, weights = W_pop)
m_spat   <- felm(fml("death_rates_per_100_k ~ InvDist_exposure",                 "GEOID + state_year", covs_erpo), data = my_data_with_spatial_g, weights = W_pop)
m_both   <- felm(fml("death_rates_per_100_k ~ ERPO_exposure + InvDist_exposure", "GEOID + state_year", covs_erpo), data = my_data_with_spatial_g, weights = W_pop)

modelsummary(list("Social proximity"=m_prox, "Socio-spatial"=m_prox2, "Direct ERPO"=m_direct),
             gof_map = c("nobs","r.squared"), stars = c("*"=.1,"**"=.05,"***"=.01),
             output = op_cov("table_proximity_and_direct.txt"))
modelsummary(list("ERPO social"=m_social, "ERPO spatial"=m_spat, "ERPO both"=m_both),
             gof_map = c("nobs","r.squared"), stars = c("*"=.1,"**"=.05,"***"=.01),
             output = op_cov("table_erpo_exposure.txt"))
saveRDS(list(m_direct=m_direct, m_prox=m_prox, m_prox2=m_prox2,
             m_social=m_social, m_spat=m_spat, m_both=m_both), op_cov("updated_covariate_models.rds"))

# ============================================================================= 