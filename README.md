# Analysis of COVID-19 Cases, Hospitalizations, Deaths, and Vaccine Equity in the U.S.

### Shiny App 

https://czhang87.shinyapps.io/COVID-19_US/

### Motivation

COVID-19 is a viral infectious disease caused by coronavirus and has been a pandemic since 2019. COVID-19 is responsible for significant morbidity and mortality and profoundly impacts the economy and everyday life. One of the most effective preventions against COVID-19 is the vaccine. Widespread vaccination will be critical to protecting the general public, especially the high-risk population of COVID-19, from hospitalization and death. As new variants continue to emerge, the urgency to increase the percent of the vaccinated population is increasingly apparent. This Shiny App is created to help general public understand the risk of COVID-19 and vaccination progress in the U.S. counties and provide up-to-date data for COVID-19 related policy making for public health and govenment officials.

### Data Questions

* What is the current status of COVID-19 related cases, hospitalization, death, and vaccination hesitancy and progress in U.S. counties?
* What is the correlation between these key indicators?
* Are there disparity/ inequalities of COVID-19 related cases, hospitalization, death in different categories of metro statuses, CDC Social Vulnerability Index, and COVID-19 Vaccine Coverage Index?

### Data Source

The data used in this Shiny App covers counties in 48 contiguous U.S. states and District of Columbia. 

* [Geo Data of the U.S. counties (ArcGIS Hub)](https://hub.arcgis.com/datasets/esri::usa-counties/about)
* [Community Profile Report Counties (HHS)](https://protect-public.hhs.gov/datasets/cad5b70395a04b95936f0286be30e282/explore?showTable=true)
* [United States COVID-19 County Level of Community Transmission (CDC)](https://data.cdc.gov/Public-Health-Surveillance/United-States-COVID-19-County-Level-of-Community-T/8396-v7yb)
* [COVID-19 Vaccinations in the United States, County (CDC)](https://data.cdc.gov/Vaccinations/COVID-19-Vaccinations-in-the-United-States-County/8xkx-amqh)
* [Vaccine Hesitancy for COVID-19: County and local estimates (CDC)](https://data.cdc.gov/Vaccinations/Vaccine-Hesitancy-for-COVID-19-County-and-local-es/q9mh-h2tw)

### Indicators

* COVID-19 Epidemiology
  - Cases per 100k Last 7 Days Moving Average
  - Test Positivity Rate Last 7 Days Moving Average
  - Hospitalizations per 100k Last 7 Days Moving Average
  - Percentage of ICU Occupied by COVID-19 Patients
  - Deaths per 100k Last 7 Days Moving Average
* COVID-19 Vaccination
  - At Least One Dose
  - Fully Vaccinated 
  - Booster (or Additional) Dose
  - Percentage of COVID-19 Vaccine Hesitancy
* Contexts for Inequality Analysis
  - Metro Status: Metro and Non-metro
  - CDC Social Vulnerability Index (SVI): community vulnerability to disaster, disease outbreak, and etc.
  - COVID-19 Vaccine Coverage Index (CVAC): supply- and demand-related challenges that hinder the vaccine coverage in the U.S.

The [CDC Social Vulnerability Index](https://data.cdc.gov/stories/s/Vaccine-Hesitancy-for-COVID-19/cnd2-a6zw/) and [COVID-19 Vaccine Coverage Index](https://data.cdc.gov/stories/s/Vaccine-Hesitancy-for-COVID-19/cnd2-a6zw/) are used as the context of disparity/inequality anaylysis in this Shiny App. 

The CDC's Social Vulnerability Index (SVI) summarizes the extent to which a community is socially vulnerable to disaster. The factors considered in developing the SVI include economic data as well as data regarding education, family characteristics, housing language ability, ethnicity, and vehicle access. SVI values range from 0 (least vulnerable) to 1 (most vulnerable). The SVI can also be categorized as follows: Very Low (0.0-0.19), Low (0.20-0.39); Moderate (0.40-0.59); High (0.60-0.79); Very High (0.80-1.0).

The Surgo Covid-19 Vaccine Coverage Index (CVAC) captures supply- and demand-related challenges that may hinder rapid, widespread COVID-19 vaccine coverage in U.S. counties, through five specific themes: historic undervaccination, sociodemographic barriers, resource-constrained healthcare system, healthcare accessibility barriers, and irregular care-seeking behaviors. The CVAC measures the level of concern for a difficult rollout on a range from 0 (lowest concern) to 1 (highest concern). The CVAC Index can also be categorized as follows: Very Low (0.0-0.19), Low (0.20-0.39), Moderate (0.40-0.59), High (0.60-0.79), or Very High (0.80-1.0) Concern.

### Author

Alex Zhang, Apprentice of Data Science 5 Cohort at [Nashville Software School](https://nashvillesoftwareschool.com/)

### Acknowledgement

* Michael Holloway (Lead Instructor)
* Mahesh Rao (Instructor)
* Veronica Ikeshoji-Orlati (Teaching Assistant)
* Alvin Wendt (Teaching Assistant)
* Data Science 5 Cohort, Nashville Software School

[![NSS](/covid_vaccine_ShinyApp/page/img/NSS-logo-horizontal-small.jpeg)](https://nashvillesoftwareschool.com/)
