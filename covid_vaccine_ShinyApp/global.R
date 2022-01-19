# COVID in the U.S.
# 
library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(leaflet)
library(htmltools)
library(rmapshaper)
library(stringr)
library(plotly)
library(ggExtra)
library(ggpmisc)
library(ggbeeswarm)
library(DT)
library(shinyWidgets)
library(dashboardthemes)
library(scales)
library(shinycssloaders)
library(ggcorrplot)
library(repr)
library(rlang)
library(xlsx)
library(shinybusy)


 
# # set working directory
setwd("~/Documents/Data Science/bootcamp/NSS/DS5/nss_projects/covid_vaccine/covid_vaccine_ShinyApp")

# #####################################################################################################
# # Geospatial data with demographic info
# us_county_covid <- read_sf("data/USA_Counties/USA_Counties.shp")
# 
# # simplify polygons
# us_county_covid <- rmapshaper::ms_simplify(us_county_covid, keep = 0.05, keep_shapes = TRUE)
# 
# # write simplified polygons to RDS
# us_county_covid %>%
#   write_rds("data/us_county_covid.RDS")
# #####################################################################################################

# read simplified polygons from RDS to data frame
us_county_covid <- read_rds("data/us_county_covid.RDS")

# vaccination data from CDC
url <- "https://data.cdc.gov/resource/8xkx-amqh.csv?$limit=3282"
response <- GET(url)
covid_vaccination <- content(response,as = "parsed")

# COVID testing, case, hospitalization, ICU, ventilator, and death data
url1 <- "https://services5.arcgis.com/qWZ7BaZXaP5isnfT/arcgis/rest/services/Community_Profile_Report_Counties/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json"
url2 <- "https://services5.arcgis.com/qWZ7BaZXaP5isnfT/arcgis/rest/services/Community_Profile_Report_Counties/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&resultOffset=2000&outSR=4326&f=json"
response1 <- GET(url1)
response2 <- GET(url2)
df1 <- content(response1, as = "text") %>%
  fromJSON()
df2 <- content(response2, as = "text") %>%
  fromJSON()
case_hospitalization_death <-  rbind(df1$features$attributes,df2$features$attributes)

# COVID vaccine hesitancy data
url <- "https://data.cdc.gov/resource/q9mh-h2tw.csv?$limit=3282&$select=fips_code,estimated_hesitant,estimated_hesitant_or_unsure,estimated_strongly_hesitant,social_vulnerability_index,svi_category,ability_to_handle_a_covid,cvac_category"
response <- GET(url)
covid_vaccine_hesitancy <-content(response, as = "parsed")

covid_vaccine_hesitancy <- covid_vaccine_hesitancy %>%
  mutate(fips_code = as.character(fips_code)) %>%
  mutate(fips_code = str_pad(fips_code, 5, pad = "0"))

state_lat_lon <- read_csv("data/state_lat_lon.csv")  
state_lat_lon

# merge data frame by FIPS

us_county_covid <- left_join(us_county_covid, covid_vaccination, by = c("FIPS"="fips"))
us_county_covid <- left_join(us_county_covid, case_hospitalization_death, by = c("FIPS"="FIPS_code"))
us_county_covid <- left_join(us_county_covid, covid_vaccine_hesitancy, by = c("FIPS"="fips_code"))
us_county_covid <- left_join(us_county_covid, state_lat_lon, by=c("STATE_NAME"="state_name"))

# calculate and add booster_doses_pop_pct column, factor metro_status, svi_category, and cvac_category, filter out four regions
us_county_covid <- us_county_covid %>%
  mutate(booster_doses_pop_pct = round(booster_doses/(series_complete_yes/series_complete_pop_pct), 1),
         booster_doses_18pluspop_pct = round(booster_doses_18plus/(series_complete_18plus/series_complete_18pluspop_pct), 1),
         booster_doses_65pluspop_pct = round(booster_doses_65plus/(series_complete_65plus/series_complete_65pluspop_pct), 1),
         estimated_hesitant = round(estimated_hesitant*100,1),
         metro_status = factor(metro_status, levels = c("Metro",
                                                        "Non-metro")),
         svi_category = factor(svi_category, levels = c("Very Low Vulnerability",
                                                        "Low Vulnerability",
                                                        "Moderate Vulnerability",
                                                        "High Vulnerability",
                                                        "Very High Vulnerability")),
         cvac_category = factor(cvac_category, levels = c("Very Low Concern",
                                                          "Low Concern",
                                                          "Moderate Concern",
                                                          "High Concern",
                                                          "Very High Concern"))

  ) %>%
  filter(!STATE_NAME %in% c("Puerto Rico"))
# , "District of Columbia", "Alaska", "Hawaii"

# labels for legends and titles

choices_data_type <- c('Cases per 100k Last 7 Days',
                       'Test Positivity Rate Last 7 Days',
                       'Hospitalizations per 100k Last 7 Days', 
                       'Deaths per 100k Last 7 Days',
                       'Vaccination Percentage',
                       'Vaccination Hesitancy', 
                       'CDC Social Vulnerability Index',
                       'COVID-19 Vaccine Coverage Index')
choices_vaccination_status <- c('At Lease One Dose', 'Fully Vaccinated', 'Booster (or Additional) Dose')
choices_age <- c("All Age Groups","≥ 5 Years", "≥ 12 Years", "≥ 18 Years", "≥ 65 Years")
choices_xvariable <- c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                       "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                       "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                       "Percentage of ICU Occupied by COVID Patients" = "pct_icu_covid",
                       "Percentage of Ventilator Used by COVID Patients" = "pct_vent_covid",
                       "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                       "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                       "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                       "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                       "Percentage of COVID-19 Vaccine Hesitancy" = "estimated_hesitant",
                       "CDC Social Vulnerability Index" = "social_vulnerability_index",
                       "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid")
choices_yvariable <- c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                       "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                       "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                       "Percentage of ICU Occupied by COVID Patients" = "pct_icu_covid",
                       "Percentage of Ventilator Used by COVID Patients" = "pct_vent_covid",
                       "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                       "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                       "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                       "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                       "Percentage of COVID-19 Vaccine Hesitancy" = "estimated_hesitant",
                       "CDC Social Vulnerability Index" = "social_vulnerability_index",
                       "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid")
choices_state <- c("United States", us_county_covid %>%
                     pull(STATE_NAME) %>%
                     unique() %>%
                     sort())
choices_state_rank <- us_county_covid %>%
  pull(STATE_NAME) %>%
  unique() %>%
  sort()
choices_metro <- c("Metro" , "Non-metro")

switch_labels <- c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                   "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                   "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                   "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                   "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                   "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                   "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                   "Percentage of COVID-19 Vaccine Hesitancy" = "estimated_hesitant",
                   "CDC Social Vulnerability Index" = "social_vulnerability_index",
                   "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid",
                   "Percentage of ICU Occupied by COVID Patients" = "pct_icu_covid",
                   "Percentage of Ventilator Used by COVID Patients" = "pct_vent_covid")
hue_labels<- c("Metropolitan Status"="metro_status",
               "CDC Social Vulnerability Index" = "svi_category",
               "COVID-19 Vaccine Coverage Index" = "cvac_category")

table_columns<- c("County"="NAME",
                  "State" = "STATE_NAME",
                  "Population" = "POPULATION",
                  "Metropolitan Status"="metro_status",
                  "Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                  "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                  "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                  "Percentage of ICU Occupied by COVID patients" = "pct_icu_covid",
                  "Percentage of Ventilator Used by COVID patients" = "pct_vent_covid",
                  "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                  "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                  "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                  "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                  "COVID-19 Vaccine Hesitancy Percentage" = "estimated_hesitant",
                  "CDC Social Vulnerability Index" = "social_vulnerability_index",
                  "CDC Social Vulnerability Index" = "svi_category",
                  "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid",
                  "COVID-19 Vaccine Coverage Index" = "cvac_category")

labels_corr<- c("Cases_per_100k_last_7_days" = "Cases per 100k Last 7 Days",
                "test_positivity_rate_last_7_d"="Test Positivity Rate Last 7 Days",
                "conf_covid_admit_100k_last_7"="Hospitalizations per 100k Last 7 Days",
                "pct_icu_covid"="Percentage of ICU Occupied by COVID patients",
                "pct_vent_covid"="Percentage of Ventilator Used by COVID patients",
                "Deaths_per_100k_last_7_days"="Deaths per 100k Last 7 Days",
                "administered_dose1_pop_pct"="At Least One Dose in All Age Groups",
                "series_complete_pop_pct"= "Fully Vaccinated in All Age Groups",
                "booster_doses_pop_pct"="Booster (or Additional) Dose in All Age Groups",
                "estimated_hesitant"="COVID-19 Vaccine Hesitancy Percentage",
                "social_vulnerability_index"="CDC Social Vulnerability Index",
                "ability_to_handle_a_covid"="COVID-19 Vaccine Coverage Index")

# Range of numericRangeInput
min_pop <- min(us_county_covid$POPULATION)
max_pop <- max(us_county_covid$POPULATION)

# Initial view
initial_lat = 39.8283
initial_lng = -98.5795
initial_zoom = 4

# element_text()
black.bold.plain.11.text<- element_text(color = "black", face = "bold", size=11)
black.bold.plain.14.text<- element_text(color = "black", face = "bold", size=14)
black.bold.plain.18.text<- element_text(color = "black", face = "bold", size=18 )
white.bold.plain.14.text<- element_text(color = "white", face = "bold", size=14)
