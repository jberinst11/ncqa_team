
#map and save as PNG _ commercial
state_choropleth(comratings, title = paste("Average", "Consumer Satisfaction", "Rating of", instype[1], "Insurance", "by State"))
#map and save as PNG _ medicare
state_choropleth(comratings2, title = paste("Average", "Consumer Satisfaction", "Rating of", instype[2], "Insurance", "by State"))
#map and save as PNG _ medicaid
#many NA ratings here- not sure why
state_choropleth(comratings3, title = paste("Average", "Consumer Satisfaction", "Rating of", instype[3], "Insurance", "by State"))


#summarize Overall NCQA Rating by state - Commercial - HMO
rated <-fulltable %>% 
  dplyr::group_by(state) %>% 
  filter(instype == "Commercial") %>% 
  dplyr::summarize(meanrat =mean(consumersat, na.rm=TRUE)) %>%  
  arrange(desc(meanrat)) %>% 
  dplyr::rename(abbrev = state) 
rated

state_choropleth(comratings, title = paste("Average", "Consumer Satisfaction", "Rating of",  instype[1], 
                                           "Insurance", "by State on a 0 (Low) to 5 (High) Scale"))
