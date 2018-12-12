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

#Number of Deaths Due to Diseases of the Heart per 100000 Population.csv
#https://www.kff.org/other/state-indicator/number-of-deaths-due-to-diseases-of-the-heart-per-100000-population/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

death_heart_disease<-fread("raw_data/KFF_tables_2016/Number of Deaths Due to Diseases of the Heart per 100000 Population.csv")
death_heart_disease <-death_heart_disease [death_heart_disease$Location %in% states, -4]
names(death_heart_disease)  <- c("year", "state", "cvs_death_rate")

#Number of Deaths Due to Injury by Firearms per 100000 Population.csv
#https://www.kff.org/other/state-indicator/firearms-death-rate-per-100000/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

death_firearm <- fread("raw_data/KFF_tables_2016/Number of Deaths Due to Injury by Firearms per 100000 Population.csv")
death_firearm <- death_firearm[death_firearm$Location %in% states, -4]
names(death_firearm) <- c("year", "state", "firearm_death_rate")

#Opioid Overdose Death Rates and All Drug Overdose Death Rates per 100000 Population AgeAdjusted.csv
#https://www.kff.org/other/state-indicator/opioid-overdose-death-rates/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
opioid_death_rate<- fread("raw_data/KFF_tables_2016/Opioid Overdose Death Rates and All Drug Overdose Death Rates per 100000 Population AgeAdjusted.csv")
opioid_death_rate <- opioid_death_rate[opioid_death_rate$Location %in% states, -c(5,6)]
names(opioid_death_rate) <- c("year", "state", "opioid_od_death_rate", "all_drug_od_death_rate")


#Number of Deaths per 100000 Population Caused by Influenza and Pneumonia.csv
#https://www.kff.org/other/state-indicator/influenza-and-pneumonia-death-rate/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
death_flu_pna<-fread("raw_data/KFF_tables_2016/Number of Deaths per 100000 Population Caused by Influenza and Pneumonia.csv")
death_flu_pna<- death_flu_pna[death_flu_pna$Location %in% states, -c(4)]
names(death_flu_pna)<- c("year", "state", "flu_pna_death")

#Percent of Adults Who Smoke.csv
#This is 2017 data and should be used until 2016 data is available
perc_smoke<- fread("raw_data/KFF_tables_2016/Percent of Adults Who Smoke.csv")
perc_smoke<-perc_smoke[perc_smoke$Location %in% states,]
names(perc_smoke)<-c("year", "state", "percent_smoke_2017")
perc_smoke$year<-2016

#Percent of Adults Reporting Fair or Poor Health Status.csv
#This is 2017 data
reported_poor_health<- fread("raw_data/KFF_tables_2016/Percent of Adults Reporting Fair or Poor Health Status.csv")
reported_poor_health<-reported_poor_health[reported_poor_health$Location%in% states,]
names(reported_poor_health) <- c("year", "state","reported_poor_health_2017")
reported_poor_health$year<-2016

#Percent of Adults Who are Overweight or Obese.csv
#2017 Data
#overweight<- fread("raw_data/KFF_tables_2016/Percent of Adults Who are Overweight or Obese.csv")
#overweight<-overweight[overweight$Location%in% states,]

#Medicaid Expansion Enrollment.csv
#https://www.kff.org/health-reform/state-indicator/medicaid-expansion-enrollment/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
medicare_exp<- fread("raw_data/KFF_tables_2016/Medicaid Expansion Enrollment.csv")
medicare_exp<- medicare_exp[medicare_exp$Location%in% states, -c(7,8)]
names(medicare_exp) <-c("year", "state", "expansion", "total_medicaid_enrol", "expansion_enroll", "expansion_group_newly_eligible")

#Hospital Adjusted Expenses per Inpatient Day.csv
#https://www.kff.org/health-costs/state-indicator/expenses-per-inpatient-day/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
hosp_expense<- fread("raw_data/KFF_tables_2016/Hospital Adjusted Expenses per Inpatient Day.csv")
hosp_expense<-hosp_expense[hosp_expense$Location%in% states,]
names(hosp_expense) <- c("year", "state", "hosp_expense_inpatient_day")

#Hospital Inpatient Days per 1000 Population by Ownership Type.csv
#https://www.kff.org/other/state-indicator/inpatient-days-by-ownership/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

inpatient_days<- fread("raw_data/KFF_tables_2016/Hospital Inpatient Days per 1000 Population by Ownership Type.csv")
inpatient_days<- inpatient_days[inpatient_days$Location%in% states, ]
names(inpatient_days) <- c("year", "state", "hosp_rate_gov_hosp", "hosp_rate_nonprofit_hosp", "hosp_rate_forprofit_hosp",
                           "hosp_rate_total")

#Change in the Nonelderly Adult Uninsured from 2013 to 2016
#https://www.kff.org/other/state-indicator/change-in-the-nonelderly-adult-uninsured/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

insurance_change<- fread("raw_data/KFF_tables_2016/Change in the Nonelderly Adult Uninsured.csv")
insurance_change<-insurance_change[insurance_change$Location %in% states, -c(3:5)]
names(insurance_change)<- c("year", "state","2013_uninsured", "2016_uninsured", "2013_2016_change_uninsured")

#Individual Insurance Market Competition.csv
#https://www.kff.org/other/state-indicator/individual-insurance-market-competition/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D
insure_mark_comp<- fread("raw_data/KFF_tables_2016/Individual Insurance Market Competition.csv")
insure_mark_comp<-insure_mark_comp[insure_mark_comp$Location%in% states, ]
names(insure_mark_comp)<-c("year", "state", "herfindahl_hirschman_Index", "market_share_largest_insure","no_insure_>5%_mark_share")

#Average Gross Margin Per Member Per Month on the Individual Market.csv
#https://www.kff.org/private-insurance/state-indicator/average-gross-margin-on-the-individual-market/?currentTimeframe=0&sortModel=%7B%22colId%22:%22Location%22,%22sort%22:%22asc%22%7D

gross_margins_per_member<- fread("raw_data/KFF_tables_2016/Average Gross Margin Per Member Per Month on the Individual Market.csv")
gross_margins_per_member<- gross_margins_per_member[gross_margins_per_member$Location%in% states, -c(3:7,9)]
names(gross_margins_per_member) <- c("year", "state", "gross_margins_per_member")
gross_margins_per_member$year<- 2016

#Primary Care Health Professional Shortage Areas HPSAs as if 12/31/2017
hpsa_2017<- fread("raw_data/KFF_tables_2016/Primary Care Health Professional Shortage Areas HPSAs.csv")
hpsa_2017 <- hpsa_2017[hpsa_2017$Location%in% states,-4]
names(hpsa_2017) <-c("year", "state", "total_pc_hpsa", "percent_needs_met", "no_practitioners")
hpsa_2017$year<- 2016

#Build Dataframe
#https://meps.ahrq.gov/data_stats/summ_tables/insr/state/series_2/2017/tiif12.htm
#Among private-sector enrollees with single coverage: Percent in a high deductible health insurance plan by firm size and State: United States, 2017

hdhp=c(0.392,0.520,0.580,0.396,0.384,0.644,0.564,0.574,0.646,0.603,0.112,0.516,0.489,
       0.596,0.588,0.532,0.645,0.458,0.671,0.483,0.430,0.510,0.659,0.511,0.648,
       0.609,0.593,0.488,0.756,0.407,0.478,0.451,0.606,0.457,0.598, 0.454, 0.537,0.494,
       0.536,0.559,0.726,0.664,0.601,0.643,0.633,0.512,0.462,0.524,
       0.657,0.612)


hdhp_among_private_insur<-data.frame("year"= 2016, "state"=states, "percent_hdhp"=hdhp)

#Reduce and Merge table (do not include )
KFF_table<- Reduce(merge, list(
                        total_residents_2016,
                        pop_dis_by_age,
                        pop_dis_by_cit_stat,
                        pop_dis_gen,
                        pop_dis_race,
                        dis_tot_pop_poverty_level,
                        median_annual_household_income,
                        unemployment_rate,
                        avg_monthly_SNAP_participants,
                        total_state_exp_per_cap,
                        economic_distress,
                        mental_illness,
                        cancer_deaths,
                        death_rate,
                        death_heart_disease,
                        death_firearm,
                        opioid_death_rate,
                        death_flu_pna,
                        perc_smoke,
                        reported_poor_health,
                        medicare_exp,
                        hosp_expense,
                        inpatient_days,
                        insurance_change,
                        insure_mark_comp,
                        gross_margins_per_member,
                        hpsa_2017,
                        hdhp_among_private_insur))

#Convert expansion to numeric where No=0 and Yes =1

KFF_table$expansion<-ifelse(grepl("No", KFF_table$expansion), 0, 1)


#Replace all "N\\/A" and empty columns with "-" with NA

KFF_table[KFF_table =="N\\/A"] <- NA
KFF_table[KFF_table =="-"] <- NA

#Convert all to numeric
KFF_table<-lapply(KFF_table, as.numeric)


#Add back state names as factor

state_abb<-state_df$Abbreviation

KFF_table$state<-state_abb
KFF_table$state<- as.factor(KFF_table$state)
