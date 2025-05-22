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
library(splm)



## reading csv ####

my_data_with_spatial_g <- read.csv(
  "C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/spatial_social_proximity_suicide_mortality_2010_2020_cdc_wonder_data_gravity_weights.csv",
  colClasses = c(GEOID = "character"),
  check.names = FALSE
)

### reading spatial weights ###
lw_1_social <- readRDS("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/lw_1_entire_us.rds")
lw_2_spatial <- readRDS("C:/Users/kusha/Desktop/Suide Ideation Data Request Form/suicide-ideation-social-networks/lw_1_entirelw_2_spatial_us.rds")


###running the spatial panel model ####
my_data_with_spatial <- my_data_with_spatial_g %>%
  group_by(year) %>%
  mutate(ACS_MEDIAN_HH_INC = ifelse(is.na(ACS_MEDIAN_HH_INC), 
                                    predict(lm(ACS_MEDIAN_HH_INC ~ ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS, data = ., na.action = na.exclude)), 
                                    ACS_MEDIAN_HH_INC)) %>%
  ungroup()
my_data_with_spatial <- my_data_with_spatial %>%
  group_by(year) %>%
  mutate(ACS_PCT_UNEMPLOY = ifelse(is.na(ACS_PCT_UNEMPLOY), 
                                   mean(ACS_PCT_UNEMPLOY, na.rm = TRUE), 
                                   ACS_PCT_UNEMPLOY)) %>%
  ungroup()
### finding duplicates ###
duplicates <- my_data_with_spatial %>%
  group_by(GEOID, year) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1)

print(duplicates) # Should be empty

### checking if the lw_1 is correct or not ###
my_data_with_spatial_g <- my_data_with_spatial_g[,-21]


### two way fixed effect model ###
model_felm_entire_us <- felm(
  death_rates_per_100_k ~
    s_minus_i + d_minus_i + population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
    prop_black + prop_asian + prop_other + prop_hispanic +     
    ACS_MEDIAN_HH_INC + 
    ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
    ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
    Poor.mental.health.days.raw.value
  | GEOID + year,                                               # county & year FE
  data    = my_data_with_spatial_g,
  weights = my_data_with_spatial_g$ACS_TOT_POP_WT
)

summary(model_felm_entire_us, cluster = ~ state)


### two way effect models with social autocorrelation##
Linear_WITHIN_sem_ML <- spml(formula= death_rates_per_100_k ~
                               s_minus_i + d_minus_i + population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
                               prop_black + prop_asian + prop_other + prop_hispanic +     
                               ACS_MEDIAN_HH_INC + 
                               ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
                               ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
                               Poor.mental.health.days.raw.value,
                             data =  my_data_with_spatial_g, 
                             index=c("GEOID", "year"),
                             listw = lw_1_social,
                             model="within",
                             effect = "twoways",
                             spatial.error="b", 
                             lag=FALSE)
summary(Linear_WITHIN_sem_ML)


### two way fixed effects with spatial autocorrelation ####
Linear_WITHIN_sem_ML_spatial <- spml(formula= death_rates_per_100_k ~
                                       s_minus_i + d_minus_i + population_density + ACS_PCT_AGE_18_44 + ACS_PCT_AGE_45_64 +
                                       prop_black + prop_asian + prop_other + prop_hispanic +     
                                       ACS_MEDIAN_HH_INC + 
                                       ACS_PCT_ENGL_NOT_WELL + percentage_uninsured +
                                       ACS_PCT_UNEMPLOY + ACS_PCT_LT_HS +
                                       Poor.mental.health.days.raw.value,
                                     data =  my_data_with_spatial_g, 
                             index=c("GEOID", "year"),
                             listw = lw_2_spatial,
                             model="within",
                             effect = "twoways",
                             spatial.error="b", 
                             lag=FALSE)

summary(Linear_WITHIN_sem_ML_spatial)







# # Fit the Spatial Dynamic Panel Data Model on the filtered dataset
model_sac <- SDPDm(formula = death_rates_per_100_k ~ d_minus_i+race_cat+ACS_MEDIAN_HH_INC + ACS_PCT_UNEMPLOY
                   + ACS_PCT_LT_HS + race_cat +population_density+Poor.mental.health.days.raw.value,
                   data = my_data_with_spatial_g,
                   W = lw_1_social,  # Spatial weights matrix
                   index = c("GEOID", "year"),
                   model = "sar",  # Spatial autoregressive model
                   effect = "twoways",  # Two-way fixed effects (individual + time)
                   LYtrans = TRUE,  # Lee-Yu transformation for within transformation
                   dynamic = FALSE,  # Include time-lagged dependence
                   tlaginfo = list(ind = NULL, tl = TRUE, stl = TRUE)  # Include time & spatial lags
)

# Display Model Summary
summary(model_sac, robust=TRUE)
# 
# 
# # ### without lag ###
# # model_sac_without_lag <- SDPDm(formula = log(death_rates_per_100_k) ~ d_minus_i+race_cat+ACS_MEDIAN_HH_INC + ACS_PCT_UNEMPLOY 
# #                    + ACS_PCT_LT_HS + race_cat +population_density+Poor.mental.health.days.raw.value,
# #                    data = my_data_with_spatial_g,
# #                    W = lw_1_mat,  # Spatial weights matrix
# #                    index = c("GEOID", "year"),
# #                    model = "sar",  # Spatial autoregressive model
# #                    effect = "twoways",  # Two-way fixed effects (individual + time)
# #                    LYtrans = TRUE,  # Lee-Yu transformation for within transformation
# #                    dynamic = FALSE,  # Include time-lagged dependence
# #                    tlaginfo = list(ind = NULL, tl = FALSE, stl = TRUE)  # Include time & spatial lags
# # )
# # 
# # # Display Model Summary
# # summary(model_sac_without_lag)
# # 
# # 
# 
# # #### log conversion #####
# # df_selected_log <- df_selected
# # df_selected_log[] <- lapply(df_selected_log, function(x) if(is.numeric(x)) log1p(x) else x)
# # 
# # 
# # # Create the conditional scatterplot matrix
# # ggpairs(df_selected_log, 
# #         lower = list(continuous = wrap("points", alpha = 0.8, color="red")),
# #         diag = list(continuous = wrap("barDiag", fill="red", bins=10)),
# #         upper = list(continuous = wrap("smooth", method = "lm", color="red", fullrange = TRUE)))+
# #   theme_minimal()
