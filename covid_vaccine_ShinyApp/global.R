# COVID vaccination

library(shiny)
library(shinydashboard)
library(tidyverse)
library(httr)
library(jsonlite)
library(sf)
library(leaflet)
library(htmltools)
library(rmapshaper)


# vaccination data from CDC
url <- "https://data.cdc.gov/resource/8xkx-amqh.csv?$limit=3282"
response <- GET(url)
covid_vaccination <- content(response,as = "parsed")

# Geospatial data with demographic info
us_county <- read_sf("../data/USA_Counties/USA_Counties.shp")

# case, hospitalization, and death data
url1 <- "https://services5.arcgis.com/qWZ7BaZXaP5isnfT/arcgis/rest/services/Community_Profile_Report_Counties/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&outSR=4326&f=json"
url2 <- "https://services5.arcgis.com/qWZ7BaZXaP5isnfT/arcgis/rest/services/Community_Profile_Report_Counties/FeatureServer/0/query?where=1%3D1&outFields=*&returnGeometry=false&resultOffset=2000&outSR=4326&f=json"
response1 <- GET(url1)
response2 <- GET(url2)
df1 <- content(response1, as = "text") %>% 
  fromJSON()
df2 <- content(response2, as = "text") %>% 
  fromJSON()
case_hospitalization_death <-  rbind(df1$features$attributes,df2$features$attributes)

# merge data frame

us_county_covid <- left_join(us_county, covid_vaccination, by = c("FIPS"="fips"))
us_county_covid <- left_join(us_county_covid, case_hospitalization_death, by = c("FIPS"="FIPS_code"))

# remove na value
# us_county_covid <- us_county_covid %>%
#   filter(is.na(series_complete_pop_pct) == FALSE)

# calculate and add booster_doses_pop_pct column, fill na with zero
us_county_covid <- us_county_covid %>% 
  mutate(booster_doses_pop_pct = round(booster_doses/(series_complete_yes/series_complete_pop_pct), 1),
         booster_doses_18pluspop_pct = round(booster_doses_18plus/(series_complete_18plus/series_complete_18pluspop), 1),
         booster_doses_65pluspop_pct = round(booster_doses_65plus/(series_complete_65plus/series_complete_65pluspop), 1),
         svi_num = recode(svi_ctgy,
                          "A" = 0.25,
                          "B" = 0.5,
                          "C" = 0.75,
                          "D" = 1
         )) %>% 
  replace_na(list(series_complete_pop_pct = 0,
                  administered_dose1_pop_pct= 0,
                  booster_doses_pop_pct =0 )
  )

# simplify polygons
us_county_covid <- rmapshaper::ms_simplify(us_county_covid, keep = 0.05, keep_shapes = TRUE)


