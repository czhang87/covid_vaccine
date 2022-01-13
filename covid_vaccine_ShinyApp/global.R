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

# merge data frame by FIPS

us_county_covid <- left_join(us_county_covid, covid_vaccination, by = c("FIPS"="fips"))
us_county_covid <- left_join(us_county_covid, case_hospitalization_death, by = c("FIPS"="FIPS_code"))
us_county_covid <- left_join(us_county_covid, covid_vaccine_hesitancy, by = c("FIPS"="fips_code"))

# calculate and add booster_doses_pop_pct column, factor metro_status, svi_category, and cvac_category, filter out four regions
us_county_covid <- us_county_covid %>%
  mutate(booster_doses_pop_pct = round(booster_doses/(series_complete_yes/series_complete_pop_pct), 1),
         booster_doses_18pluspop_pct = round(booster_doses_18plus/(series_complete_18plus/series_complete_18pluspop), 1),
         booster_doses_65pluspop_pct = round(booster_doses_65plus/(series_complete_65plus/series_complete_65pluspop), 1),
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
  filter(!STATE_NAME %in% c("Puerto Rico", "District of Columbia", "Alaska", "Hawaii"))

# labels for legends and titles

switch_labels <- c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                   "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                   "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                   "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                   "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                   "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                   "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                   "COVID-19 Vaccine Hesitancy Percentage" = "estimated_hesitant",
                   "CDC Social Vulnerability Index" = "social_vulnerability_index",
                   "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid"
)

hue_labels<- c(
  "Metropolitan Status"="metro_status",
  "CDC Social Vulnerability Index" = "svi_category",
  "COVID-19 Vaccine Coverage Index" = "cvac_category"
)

table_columns<- c("Cases per 100k Last 7 Days"="Cases_per_100k_last_7_days",
                  "Test Positivity Rate Last 7 Days" = "test_positivity_rate_last_7_d",
                  "Hospitalizations per 100k Last 7 Days" = "conf_covid_admit_100k_last_7",
                  "Deaths per 100k Last 7 Days" = "Deaths_per_100k_last_7_days",
                  "At Least One Dose in All Age Groups"="administered_dose1_pop_pct",
                  "Fully Vaccinated in All Age Groups" = "series_complete_pop_pct",
                  "Booster (or Additional) Dose in All Age Groups" = "booster_doses_pop_pct",
                  "COVID-19 Vaccine Hesitancy Percentage" = "estimated_hesitant",
                  "CDC Social Vulnerability Index" = "social_vulnerability_index",
                  "COVID-19 Vaccine Coverage Index" = "ability_to_handle_a_covid",
                  "Metropolitan Status"="metro_status",
                  "CDC Social Vulnerability Index" = "svi_category",
                  "COVID-19 Vaccine Coverage Index" = "cvac_category")


# # incrase the memory of the shinyapps.io to the largest 2048M
# rsconnect::configureApp("COVID-19_US", size="xlarge")

# lobstr::mem_used()

