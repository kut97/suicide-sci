library(readr)
library(dplyr)
library(stringr)
library(purrr)
library(tidyverse)

pop_bf_ny_2010 <- read.csv("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/age_group_population_file/2010_pop_group_estimates_before_new_york.csv")
pop_af_ny_2010 <- read.csv("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/age_group_population_file/2010_pop_group_estimates_after_new_york.csv")


# 2. Define paths and parameters
#─────────────────────────────────────────────────────────────────────────────
base_dir    <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/age_group_population_file"
variants    <- c("before", "after")
short       <- c("bf", "af")        # for naming pop_bf/af_ny_<year>
years_csv   <- 2010:2020
years_xls   <- 2021:2022

#─────────────────────────────────────────────────────────────────────────────
# 3. Read & combine CSV files (2010–2020)
#─────────────────────────────────────────────────────────────────────────────
for (i in seq_along(variants)) {
  variant    <- variants[i]
  variant_sd <- short[i]
  
  for (yr in years_csv) {
    fname  <- sprintf("%d_pop_group_estimates_%s_new_york.csv", yr, variant)
    path   <- file.path(base_dir, fname)
    dfname <- sprintf("pop_%s_ny_%d", variant_sd, yr)
    
    # read CSV, force County.Code as character
    assign(
      dfname,
      read.csv(path, colClasses = c("County Code" = "character")),
      envir = .GlobalEnv
    )
  }
}

# bind before/after into pop_df_2010…pop_df_2020
for (yr in years_csv) {
  bf <- get(sprintf("pop_bf_ny_%d", yr))
  af <- get(sprintf("pop_af_ny_%d", yr))
  assign(sprintf("pop_df_%d", yr),
         bind_rows(bf, af),
         envir = .GlobalEnv)
}

#─────────────────────────────────────────────────────────────────────────────
# 4. Pre‑process & aggregate (2010–2020)
#    - drop Notes
#    - remove empty Age.Group
#    - pad County.Code to 5 digits
#    - collapse "< 1" + "1-4" → "0-4 years"
#    - sum Population
#    - add year column
#─────────────────────────────────────────────────────────────────────────────
for (yr in years_csv) {
  df <- get(sprintf("pop_df_%d", yr))
  
  df_clean <- df %>%
    select(-Notes) %>%
    filter(Age.Group != "") %>%
    mutate(
      County.Code    = str_pad(County.Code, width = 5, pad = "0"),
      Population     = as.numeric(Population),
      Age.Group.Code = case_when(
        Age.Group %in% c("< 1 year", "1-4 years") ~ "0-4 years",
        TRUE                                      ~ Age.Group
      ),
      year = yr
    ) %>%
    group_by(County, County.Code, Age.Group.Code, year) %>%
    summarise(Population = sum(Population, na.rm = TRUE),
              .groups = "drop")
  
  assign(sprintf("pop_df_%d", yr), df_clean, envir = .GlobalEnv)
}

#─────────────────────────────────────────────────────────────────────────────
# 5. Read & process Excel files (2021–2022) – same pipeline
#─────────────────────────────────────────────────────────────────────────────

variants   <- c("before", "after")
short      <- c("bf", "af")
years_csv2 <- 2021:2022
base_dir   <- "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/age_group_population_file"

# 1. Read each CSV into pop_bf_ny_<yr> / pop_af_ny_<yr>
for (i in seq_along(variants)) {
  variant    <- variants[i]
  variant_sd <- short[i]
  
  for (yr in years_csv2) {
    fname  <- sprintf("%d_pop_group_estimates_%s_new_york.csv", yr, variant)
    path   <- file.path(base_dir, fname)
    dfname <- sprintf("pop_%s_ny_%d", variant_sd, yr)
    
    assign(
      dfname,
      read.csv(path, colClasses = c("County Code" = "character")),
      envir = .GlobalEnv
    )
  }
}

# 2. Bind before/after → pop_df_<yr>
for (yr in years_csv2) {
  bf <- get(sprintf("pop_bf_ny_%d", yr))
  af <- get(sprintf("pop_af_ny_%d", yr))
  
  assign(
    sprintf("pop_df_%d", yr),
    bind_rows(bf, af),
    envir = .GlobalEnv
  )
}

# 3. Clean pop_df_2021 & pop_df_2022
for (yr in years_csv2) {
  df_name <- sprintf("pop_df_%d", yr)
  
  df_clean <- get(df_name) %>%
    # a) drop unwanted cols
    select(-Notes, -Age.Group.Code, -County) %>%
    # b) rename Age.Group → Age.Group.Code
    rename(Age.Group.Code = Age.Group) %>%
    # c) remove empty age‑group rows
    filter(Age.Group.Code != "") %>%
    # d) pad GEOID and convert Population
    mutate(
      County.Code = str_pad(County.Code, width = 5, pad = "0"),
      Population  = as.numeric(Population),
      year        = yr
    ) %>%
    # e) rename County.Code → GEOID
    rename(GEOID = County.Code) %>%
    # f) reorder columns: GEOID, Age.Group.Code, year, Population
    select(GEOID, Age.Group.Code, year, Population)
  
  assign(df_name, df_clean, envir = .GlobalEnv)
}

#─────────────────────────────────────────────────────────────────────────────
# 6. Drop 'County', rename 'County.Code' → 'GEOID' for ALL pop_df_2010–2022
#─────────────────────────────────────────────────────────────────────────────
for (yr in c(years_csv, years_xls)) {
  df_name <- sprintf("pop_df_%d", yr)
  
  df_final <- get(df_name) %>%
    select(-County) %>%
    rename(GEOID = County.Code)
  
  assign(df_name, df_final, envir = .GlobalEnv)
}

# At the end, each pop_df_2010 … pop_df_2022 has columns:
#   GEOID, Age.Group.Code, Population, year

# — reference grids from 2010–2020 full data —
all_counties   <- unique(pop_df_2010$GEOID)
all_age_groups <- unique(pop_df_2010$Age.Group.Code)

# — complete 2021 & 2022, filling missing Population with NA —
for (yr in years_csv2) {
  df_name <- sprintf("pop_df_%d", yr)
  
  df_full <- get(df_name) %>%
    # expand to every GEOID × Age.Group.Code
    complete(
      GEOID           = all_counties,
      Age.Group.Code  = all_age_groups,
      fill = list(Population = NA, year = yr)
    ) %>%
    arrange(GEOID, Age.Group.Code)
  
  assign(df_name, df_full, envir = .GlobalEnv)
}

## one data frame ###
# 1. Gather all pop_df names
years_all <- 2010:2022
df_names  <- sprintf("pop_df_%d", years_all)

# 2. Retrieve and bind into one data frame
pop_all <- bind_rows(mget(df_names))

pop_all <- pop_all %>%
  arrange(GEOID)