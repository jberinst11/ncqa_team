#Install Packages
install.packages("readr")
install.packages("data.table")

#Load Library
library(readr)
library(data.table)
library(dplyr)
library(ggplot2)

#Import from https://www.kff.org/state-category/demographics-and-the-economy/
#Import Data 10/20/2018 using C#

# Import Total Residents in 2016 from KFF
#https://www.kff.org/other/state-indicator/total-residents/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
total_residents_2016 <-fread("raw_data/KFF_tables_2016/Total Number of Residents.csv")
total_residents_2016 <- total_residents_2016[-c(1,10), ]   #Remove USA and DC
names(total_residents_2016) <- c("year", "state", "total_residents")

#Save list of 50 states:states
states<-unique(total_residents_2016$state)


#Import Population Distribution by Age in 2016 from KFF
#https://www.kff.org/other/state-indicator/distribution-by-age/?currentTimeframe=1&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states

pop_dis_by_age <- fread("raw_data/KFF_tables_2016/Population Distribution by Age.csv")
pop_dis_by_age <- pop_dis_by_age[pop_dis_by_age$Location %in% states,-c(3:9,16)]
names(pop_dis_by_age) <- c("year", "state", "per_pop_age0-18", "per_pop_age19-25",
                           "per_pop_age26-34", "per_pop_age35-54", "per_pop_age55-64",
                           "per_pop_age65")


#Import Population Distribution by Citizenship Status for 2016 from KFF
#https://www.kff.org/other/state-indicator/distribution-by-citizenship-status/?currentTimeframe=1&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
pop_dis_by_cit_stat <- fread("raw_data/KFF_tables_2016/Population Distribution by Citizenship Status.csv")
pop_dis_by_cit_stat <- pop_dis_by_cit_stat [pop_dis_by_cit_stat$Location %in% states,-c(3:5,8)]
names(pop_dis_by_cit_stat) <- c("year", "state","per_citizen", "per_noncitizen")

#Population Distribution by Gender for 2016 from KFF
#https://www.kff.org/other/state-indicator/distribution-by-gender/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
pop_dis_gen <- fread("raw_data/KFF_tables_2016/Population Distribution by Gender.csv")
pop_dis_gen <- pop_dis_gen [pop_dis_gen$Location %in% states,-c(3:5,8)]
names(pop_dis_gen) <- c("year", "state", "percent_male", "percent_female")

#Population Distribution by Race/Ethnicity for 2016 from KFF
#https://www.kff.org/other/state-indicator/distribution-by-raceethnicity/?currentTimeframe=1&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
pop_dis_race <- fread("raw_data/KFF_tables_2016/Population Distribution by RaceEthnicity.csv")
pop_dis_race <- pop_dis_race [pop_dis_race$Location %in% states,-c(3:10,18)]
names(pop_dis_race) <- c("year", "state",
                         "per_white", "per_black", "per_hispanic", "per_asian", "per_native", "per_pacific", "per_two_or_more_races")

#Distribution of Total Population by Federal Poverty Levelfor 2016 from KFF
#https://www.kff.org/other/state-indicator/distribution-by-fpl/?currentTimeframe=1&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
dis_tot_pop_poverty_level <- fread("raw_data/KFF_tables_2016/Distribution of Total Population by Federal Poverty Level.csv")
dis_tot_pop_poverty_level<- dis_tot_pop_poverty_level[dis_tot_pop_poverty_level$Location %in% states,-c(3:7,12)]
names(dis_tot_pop_poverty_level) <- c("year", "state",
                                      "per_fpv<100",  "per_fpv100-199", "per_fpv200-399", "per_fpv>400")


#Median Annual Household Income for 2016 from KFF
#https://www.kff.org/other/state-indicator/median-annual-income/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states

median_annual_household_income<- fread("raw_data/KFF_tables_2016/Median Annual Household Income.csv")
median_annual_household_income<- median_annual_household_income[median_annual_household_income$Location %in% states,]
names(median_annual_household_income) <- c("year","state", "med_house_income")

#Unemployment Rate (Seasonally Adjusted) for 2016 from KFF
#https://www.kff.org/other/state-indicator/unemployment-rate/?currentTimeframe=2&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
unemployment_rate<- fread("raw_data/KFF_tables_2016/Unemployment Rate (Seasonally Adjusted).csv")
unemployment_rate <- as.data.frame(unemployment_rate)
names(unemployment_rate) <- c("year","state", "unemployment_rate")
unemployment_rate<- unemployment_rate[unemployment_rate$state %in% states, ]


#Monthly Average Number of Persons Participating in Supplemental Nutrition Assistance Program SNAP from KFF 2016
#https://www.kff.org/other/state-indicator/avg-monthly-participation/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
avg_monthly_SNAP_participants<- fread("raw_data/KFF_tables_2016/Monthly Average Number of Persons Participating in Supplemental Nutrition Assistance Program SNAP.csv")
avg_monthly_SNAP_participants<- avg_monthly_SNAP_participants[avg_monthly_SNAP_participants$Location %in% states,]
names(avg_monthly_SNAP_participants) <- c("year", "state", "avg_monthly_SNAP_participants")

##Remeber to Calculate avg_monthly_SNAP_participants as percentage of state population in 2016

#Total State Expenditures per Capita for 2016 from KFF
#https://www.kff.org/other/state-indicator/per-capita-state-spending/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states

total_state_exp_per_cap<- fread("raw_data/KFF_tables_2016/Total State Expenditures per Capita.csv")
total_state_exp_per_cap<- total_state_exp_per_cap[total_state_exp_per_cap$Location %in% states,]
names(total_state_exp_per_cap) <- c("year", "state", "expend_per_cap")

#Measures of State Economic Distress: Housing Foreclosures and Changes in Unemployment
#and Food Stamp Participation for 2016 from KFF
#https://www.kff.org/other/state-indicator/foreclosuresunemploymentfood-stamps/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states

economic_distress <-fread("raw_data/KFF_tables_2016/Measures of State Economic Distress  Housing Foreclosures and Changes in Unemployment and Food Stamp.csv")
economic_distress<-economic_distress[economic_distress$Location %in% states,]
names(economic_distress) <- c("year", "state", "forclosure_rate", "per_change_mon_umployment", "per_change_mon_SNAP", "economic_distress_rank")

#Adults Reporting Mental Illness in the Past Year
#https://www.kff.org/other/state-indicator/poor-mental-health-among-adults/?currentTimeframe=1&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

mental_illness <-fread("raw_data/KFF_tables_2016/Adults Reporting Mental Illness in the Past Year.csv")
mental_illness<-mental_illness[mental_illness$Location %in% states,-c(3,5)]
names(mental_illness) <- c("year", "state", "any_mental_illness", "serious_mental_illness")

#Number of Cancer Deaths per 100000 Population.csv
#https://www.kff.org/other/state-indicator/cancer-death-rate-per-100000/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states
cancer_deaths <-fread("raw_data/KFF_tables_2016/Number of Cancer Deaths per 100000 Population.csv")
cancer_deaths <-cancer_deaths[cancer_deaths$Location %in% states,]
names(cancer_deaths) <- c("year", "state", "cancer_death_rate")

#Number of Deaths per 100000 Population
#https://www.kff.org/other/state-indicator/death-rate-per-100000/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
#Keep only 50 states

death_rate <-fread("raw_data/KFF_tables_2016/Number of Deaths per 100000 Population.csv")
death_rate  <-death_rate [death_rate$Location %in% states, -4]
names(death_rate)  <- c("year", "state", "death_rate")


