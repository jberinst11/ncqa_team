#04_clean NCQA data

#run loop to assign plantypes
# check for text in plantype, if found, assign category to plantype variable
for (i in 1:length(plantypelist)) {
  fulltable$plantype[grepl(plantypelist[i], fulltable$plan)]  <- plantypelist[i]
}
#clean up plan names - remove extra lines
fulltable$plan <- gsub("\\n", " ", fulltable$plan)
fulltable$plan <- gsub("\\s+", " ", fulltable$plan)

# add one labeled as BCBS to Blues
fulltable$plantype[(grepl("BCBS", fulltable$plan))] <- "Blue"
#replace NA in plantype as "Other"
fulltable$plantype[is.na(fulltable$plantype)] <-"Other"

#some 21 garbage listings - plan name 12 or 123 - all have rating >5
#remove these
fulltable %>% filter(rating >5) %>% summarize(total=n())
fulltable <- fulltable %>% filter(rating <6) 

#how many have overall rating of NA - 398 out of 1617 plans
fulltable %>% summarize(num_na = sum(is.na(rating)), total=n())
#now remove all with rating of NA - 1219 plans remain, replace fulltable with these 1219 (1198 observations)
fulltable %>% filter(!is.na(rating)) %>% summarize(total=n())
fulltable <- fulltable %>% filter(!is.na(rating)) 

# a few (7) have NA rating for consumersat - remove (1191 observations)
fulltable %>% filter(is.na(consumersat)) %>% summarize(total=n())
fulltable <- fulltable %>% filter(!is.na(consumersat)) 

#check counts (18 plantypes)
fulltable %>% group_by(plantype)  %>%  summarize(count =n()) %>% select(plantype, count) %>% arrange(desc(count)) %>% print(n=32)
#check NA plans - none remain.
fulltable %>% filter(is.na(plantype)) %>% select(plan)  %>% head()

#calculate overall mean rating (3.46)
fulltable %>% summarize(total=n(), mean_rating=mean(rating))
