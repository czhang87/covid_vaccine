# COVID in the U.S.

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


# vaccination data from CDC
url <- "https://data.cdc.gov/resource/8xkx-amqh.csv?$limit=3282"
response <- GET(url)
covid_vaccination <- content(response,as = "parsed")

# Geospatial data with demographic info
us_county <- read_sf("data/USA_Counties/USA_Counties.shp")


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

# merge data frame

us_county_covid <- left_join(us_county, covid_vaccination, by = c("FIPS"="fips"))
us_county_covid <- left_join(us_county_covid, case_hospitalization_death, by = c("FIPS"="FIPS_code"))
us_county_covid <- left_join(us_county_covid, covid_vaccine_hesitancy, by = c("FIPS"="fips_code"))


# remove na value
# us_county_covid <- us_county_covid %>%
#   filter(is.na(series_complete_pop_pct) == FALSE)

# calculate and add booster_doses_pop_pct column, fill na with zero
us_county_covid <- us_county_covid %>% 
  mutate(booster_doses_pop_pct = round(booster_doses/(series_complete_yes/series_complete_pop_pct), 1),
         booster_doses_18pluspop_pct = round(booster_doses_18plus/(series_complete_18plus/series_complete_18pluspop), 1),
         booster_doses_65pluspop_pct = round(booster_doses_65plus/(series_complete_65plus/series_complete_65pluspop), 1),
         estimated_hesitant = round(estimated_hesitant*100,1)
  ) #%>% 
  # replace_na(list(administered_dose1_pop_pct= 0,
  #                 series_complete_pop_pct = 0,
  #                 booster_doses_pop_pct =0,
  #                 administered_dose1_recip_5pluspop_pct =0,
  #                 series_complete_5pluspop_pct=0,
  #                 administered_dose1_recip_12pluspop_pct=0,
  #                 series_complete_12pluspop=0,
  #                 administered_dose1_recip_18pluspop_pct=0,
  #                 series_complete_18pluspop = 0,
  #                 booster_doses_18pluspop_pct = 0,
  #                 administered_dose1_recip_65pluspop_pct=0,
  #                 series_complete_65pluspop=0,
  #                 booster_doses_65pluspop_pct=0,
  #                 social_vulnerability_index = 0,
  #                 estimated_hesitant = 0,
  #                 test_positivity_rate_last_7_d=0,
  #                 ability_to_handle_a_covid=0)
  # )

# simplify polygons
us_county_covid <- rmapshaper::ms_simplify(us_county_covid, keep = 0.05, keep_shapes = TRUE)


