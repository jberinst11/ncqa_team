#06 Map choropleths to demonstrate regional trend in ratings

#SET UP STATE abbreviations table called states - joined by region
states <- read.csv("/Users/MeganMcLeod/Desktop/HealthInsHiggins/state abbreviations 2 letter.csv", header = TRUE)

#plan to subset by which rating, and by type of insurance
cols = c("rating", "consumersat", "prevention", "treatment")
whichrating = c(1,6,7,8)
ratingLabels = c("NCQA Overall", "Consumer Satisfaction", "Prevention", "Treatment")
instype= c("Commercial", "Medicare", "Medicaid")
plantype = c("PPO", "HMO", "HMO/POS", "HMO/POS/PPO", "POS", "HMO/PPO" )


#summarize Rating by state - this could be done with purrr 
#plan to loop over instype and rating - j=L3, collecting the 1, 6, 7, 8 ratings for each of the instypes - same as cols??
for (i in 1:length(whichrating)) 
{
  for (j in 1:length(instype)) 
  {
    print(paste("rating", mean(fulltable[[whichrating[i]]], na.rm = T), "instype", instype[j]))
    rated <-fulltable %>% 
      dplyr::group_by(state) %>% 
      filter(instype == instype[j])
    rated <- as.data.frame(rated)
    rated$meanrat <- mean_col("rated", cols[i])  
    arrange(desc(meanrat)) %>% 
      dplyr::rename(state = abbrev) 
  }
}
rated


#summarize Overall NCQA Rating by state - Commercial
rated <-fulltable %>% 
  dplyr::group_by(state) %>% 
  filter(instype == "Commercial") %>% 
  dplyr::summarize(meanrat =mean(consumersat, na.rm=TRUE)) %>%  
  arrange(desc(meanrat)) %>% 
  dplyr::rename(abbrev = state) 
rated

#summarize Overall NCQA Rating by state - Medicare
rated.medicare <-fulltable %>% 
  dplyr::group_by(state) %>% 
  filter(instype == "Medicare") %>% 
  dplyr::summarize(meanrat =mean(consumersat, na.rm=TRUE)) %>%  
  arrange(desc(meanrat)) %>% 
  dplyr::rename(abbrev = state) 
rated.medicare

#summarize Overall NCQA Rating by state - Medicaid
rated.medicaid <-fulltable %>% 
  dplyr::group_by(state) %>% 
  filter(instype == "Medicaid") %>% 
  dplyr::summarize(meanrat =mean(consumersat, na.rm=TRUE)) %>%  
  arrange(desc(meanrat)) %>% 
  dplyr::rename(abbrev = state) 
rated.medicaid

#leftjoin abbrevs and avg ratings for commercial, medicare and medicaid
comratings <- left_join(states, rated, by='abbrev') %>%  
  select(region = State, value = meanrat) %>% 
  mutate(region = str_to_lower(as.character(region)))
comratings

comratings2 <- left_join(states, rated.medicare, by="abbrev") %>%
  select(region = State, value = meanrat) %>% 
  mutate(region = str_to_lower(as.character(region)))
comratings2

comratings3 <- left_join(states, rated.medicaid, by="abbrev") %>%
  select(region = State, value = meanrat) %>% 
  mutate(region = str_to_lower(as.character(region)))
comratings3
