# 01_ setup-------------------------------------------------------

#load libraries
library(xml2)
library(rvest)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(corrr)
library(purrr)
library(choroplethr)
library(choroplethrMaps)
library(boxr)
library(tidycensus)
library(tigris)
library(tidyverse)
library(sf)
library(ggalt)
library(scales)
library(readxl)
library(rmarkdown)
library(ezknitr)
library(here)

#create links to NCQA website
years <- c(2015, 2016, 2017, 2018)
links <- paste("http://healthinsuranceratings.ncqa.org",
               years, "search", sep ="/")

#create state_names dataframe

state_df <- tibble::tribble(
  ~State, ~Abbreviation,
  "Alabama",          "AL",
  "Alaska",          "AK",
  "Arizona",          "AZ",
  "Arkansas",          "AR",
  "California",          "CA",
  "Colorado",          "CO",
  "Connecticut",          "CT",
  "Delaware",          "DE",
  "Florida",          "FL",
  "Georgia",          "GA",
  "Hawaii",          "HI",
  "Idaho",          "ID",
  "Illinois",          "IL",
  "Indiana",          "IN",
  "Iowa",          "IA",
  "Kansas",          "KS",
  "Kentucky",          "KY",
  "Louisiana",          "LA",
  "Maine",          "ME",
  "Montana",          "MT",
  "Nebraska",          "NE",
  "Nevada",          "NV",
  "New Hampshire",          "NH",
  "New Jersey",          "NJ",
  "New Mexico",          "NM",
  "New York",          "NY",
  "North Carolina",          "NC",
  "North Dakota",          "ND",
  "Ohio",          "OH",
  "Oklahoma",          "OK",
  "Oregon",          "OR",
  "Maryland",          "MD",
  "Massachusetts",          "MA",
  "Michigan",          "MI",
  "Minnesota",          "MN",
  "Mississippi",          "MS",
  "Missouri",          "MO",
  "Pennsylvania",          "PA",
  "Rhode Island",          "RI",
  "South Carolina",          "SC",
  "South Dakota",          "SD",
  "Tennessee",          "TN",
  "Texas",          "TX",
  "Utah",          "UT",
  "Vermont",          "VT",
  "Virginia",          "VA",
  "Washington",          "WA",
  "West Virginia",          "WV",
  "Wisconsin",          "WI",
  "Wyoming",          "WY"
)
write_csv(state_df, here::here("raw_data/state_df.csv"))
state_abbs <- state_df %>% pull(Abbreviation)
state_names <- state_df %>% pull(State)

# create plantypelist, instypes vectors
plantypelist <- c("Aetna", "Humana", "Blue", "United", "Cigna", "Kaiser", "Compass Rose",
                  "Coventry", "Connecticut General", "Group Health", "Molina", "Medica ", "Anthem",
                  "Health Net", "Care Improvement", "HealthSpring", "Health Alliance", "WellCare",
                  "Special Agents", "Pacific Source")

instype= c("Commercial", "Medicare", "Medicaid")

