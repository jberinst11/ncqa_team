# Setup Environment -------------------------------------------------------

## clean up working environment
#rm(list=ls())

#load libraries
library(rvest)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)
library(magrittr)
library(corrr)
library(purrr)

# #create neverending HTML request - overcoming the timeout problem
# html_NE = function(x) {
#   require(rvest)
#   
#   Page.src = try(read_html(x), silent=TRUE)
#   
#   #test if Page.src is erroneous
#   if (class(Page.src)[1] == "try-error") {
#     error.cond = attr(Page.src, "condition")
#     
#     #check if error condition includes "Timeout" phrase
#     #match if it returns -1
#     timed.out = regexpr("Timeout", error.cond, ignore.case = T) != -1
#     
#     # we want to continue only on "Timeout" error
#     if(timed.out ==TRUE) {
#       #print information in the console
#       print(paste(x, ": Timed out. Trying to reconnect in 30 seconds. Please wait..."))
#       Sys.sleep(30)
#       
#       return(html_NE(x))
#     }
#   }
#   return(Page.src)
# }

# html_NE("http://healthinsuranceratings.ncqa.org/2016/search/Commercial/MI", sep="")

#Get list of insurance plans in US from NCQA website
#do initial pull from website, using Michigan private ins to test
#use list of state abbrevs

statename = c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI", "ID", "IL", "IN", "IA", "KS", "KY",
              "LA", "ME", "MD", "MA", "MI", "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC", "ND", "OH",
              "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT", "VT", "VA", "WA", "WV", "WI", "WY")
#statename = c("AL", "AK", "AZ") #SHORT version for testing

instype= c("Commercial", "Medicare", "Medicaid")
link = "http://healthinsuranceratings.ncqa.org/2016/search/"

*****
# Test Loop Printing ------------------------------------------------------

# a printing loop for testing prototypes - Cmd-Shift-C to block comment/uncomment
# for (i in 1:length(statename))
# {
#   for (j in 1:length(instype))
#   {
#     print(paste(instype[j], "/", statename[i], sep = ""))
#   }
# }


# Test Loop printing ------------------------------------------------------


#how data file was acquired, built up as file fulltable
# for (i in 1:length(statename))
#   {
#   for (j in 1:length(instype))
#     {
#       healthins <- read_html(paste(link, instype[j], "/", statename[i], sep = ""))
#       hitable<- as.data.frame(healthins %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=T))
#       rows<- dim(hitable)[[1]] #grab first dimension as number of rows
#       hitable <- hitable[9:rows, 1:8]
#       names(hitable) <-  c("rating", "plan", "state", "type", "ncqa", "consumersat", "prevention", "treatment")
#       hitable$state <-  statename[i]
#       hitable$instype <- instype[j]
#       hitable$rating <- as.numeric(hitable$rating)
#       hitable$consumersat <- as.numeric(hitable$consumersat)
#       hitable$prevention <- as.numeric(hitable$prevention)
#       hitable$treatment <- as.numeric(hitable$treatment)
#       #remove NA plans
#       junk <- is.na(hitable$plan)
#       hitable[!junk,]
#       #assign(paste("Table", instype[j], statename[i], sep=""), hitable)
#       if (i ==1 & j==1) {
#         fulltable <- hitable
#         } else {
#           fulltable <- rbind(fulltable, hitable)
#         }
#     }
# }
# #cleanup rows with no real plan or labeled as "2" (missing)
# missing  <- is.na(fulltable$plan) | fulltable$plan == "2"
# fulltable  <- fulltable[!missing,]
# write.table(fulltable, "InsPlans2016")
# write.csv(fulltable, "/Users/MeganMcLeod/Documents/MSCR/Insurance Higgins/InsPlans2016.csv")
#NOTE also have a file for 2015 in Rcode folder


#-----------------------------------------------
#now read in csv file
# read data back in from csv file
fulltable <-  read.csv("/Users/MeganMcLeod/Documents/MSCR/Insurance Higgins/InsPlans2016.csv", header=T) %>% select(-X)

#list of 18 with at least 10 health care plans in US that reported scores (Oxford and Sierra have more than 10, but not enough scores)
#move to 01, clean data by year
plantypelist <- c("Aetna", "Humana", "Blue", "United", "Cigna", "Kaiser", "Compass Rose", 
                  "Coventry", "Connecticut General", "Group Health", "Molina", "Medica ", "Anthem",
                  "Health Net", "Care Improvement", "HealthSpring", "Health Alliance", "WellCare",
                  "Special Agents", "Pacific Source")

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

#check counts
fulltable %>% group_by(plantype)  %>%  summarize(count =n()) %>% select(plantype, count) %>% arrange(desc(count)) %>% print(n=32)
#check NA plans - none remain.
fulltable %>% filter(is.na(plantype)) %>% select(plan)  %>% head()

fulltable %>% summarize(total=n(), mean_rating=mean(rating))
#------------------
#now have fulltable with 1191 plans with ratings for overall and consumersat average 3.46/5


# --> a bit of dataviz - visualize distribution of insurance set satisfaction overall, focus on commercial, or facet based on plan type/instype
#consumersat ratings by plantype, wellcare and molina appear to be among the lowest rated
fulltable  %>% filter(!is.na(plantype)) %>%  filter(!is.na(consumersat)) %>% 
  ggplot(aes(plantype, consumersat)) + geom_violin(aes(fill=plantype)) +
  coord_flip() + xlab("Plan Type") + ylab("Rating of Consumer Satisfaction")

#Overall rating by plantype, Kaiser does much better than most, careimprovement appears to do the worst.
fulltable  %>% filter(!is.na(plantype)) %>%  select(plantype, rating)  %>% filter(!is.na(rating))  %>%  
  ggplot(aes(plantype, rating)) + geom_violin(aes(fill=plantype)) +
  coord_flip() + xlab("Plan Type") + ylab("Overall Rating")

#Rating of prevention by plantype, Kaiser does well again
fulltable  %>% filter(!is.na(plantype)) %>%  select(plantype, prevention)  %>% filter(!is.na(prevention))  %>%  
  ggplot(aes(plantype, prevention)) + geom_violin(aes(fill=plantype)) +
  coord_flip() + xlab("Plan Type") + ylab("Rating of Prevention")

#treatment rating by plantype as counts
fulltable  %>% filter(!is.na(plantype)) %>%  select(plantype, treatment)  %>% filter(!is.na(treatment))  %>%  
  ggplot(aes(plantype, treatment)) + geom_count() +
  coord_flip() + xlab("Plan Type") + ylab("Rating of Treatment")


# more graphing
#compare types
#consumer sat rating by insurance type (commercial vs. medicaid vs. medicare) - medicare appears to have highest overall sat
fulltable  %>% ggplot(aes(instype, consumersat)) +
  geom_violin(aes(fill=instype), draw_quantiles = c(0.25,0.5,0.75)) +
  coord_flip() + xlab("Insurance Type") + ylab("Rating of Consumer Satisfaction") +
  annotate("text", label = "vertical black lines denote the 25th, 50th, and 75th percentiles", 
           x = 1.5, y = 3, size = 4, colour = "black", fontface="italic")

#overall NCQA rating by ins type - again medicare has a slight advantage
fulltable  %>% ggplot(aes(instype, rating)) + geom_violin(aes(fill=instype), draw_quantiles = c(0.25,0.5,0.75)) +
  coord_flip() + xlab("Insurance Type") + ylab("Overall NCQA Rating") +
  annotate("text", label = "vertical black lines denote the 25th, 50th, and 75th percentiles", 
           x = 1.5, y = 3, size = 4, colour = "black", fontface="italic")

#rating of preventive services by instype- medicare is best
fulltable  %>% ggplot(aes(instype, prevention)) + geom_violin(aes(fill=instype), draw_quantiles = c(0.25,0.5,0.75)) +
  coord_flip() + xlab("Insurance Type") + ylab("Rating of Prevention Services") +
  annotate("text", label = "vertical black lines denote the 25th, 50th, and 75th percentiles", 
           x = 1.5, y = 3, size = 4, colour = "black", fontface="italic")

#rating of treatment by instype, medicare has substantially higher ratings
fulltable  %>% ggplot(aes(instype, treatment)) + geom_violin(aes(fill=instype), draw_quantiles = c(0.25,0.5,0.75)) +
  coord_flip() + xlab("Insurance Type") + ylab("Rating of Insurance Treatment") +
  annotate("text", label = "vertical black lines denote the 25th, 50th, and 75th percentiles", 
           x = 1.5, y = 2.8, size = 4, colour = "black", fontface="italic")

# Now prepare for some mapping
#choropleths
library(choroplethr)
library(choroplethrMaps)
data(df_pop_state)

#SET UP STATE abbreviations table called states - joined by region
abbrevs <- read.csv("/Users/MeganMcLeod/Documents/MSCR/Insurance Higgins/state abbreviations 2 letter.csv", header = TRUE)
abbrevs$region <- tolower(abbrevs$State) 
states <- left_join(df_pop_state, abbrevs, by = NULL) %>% 
  select(-State) %>% 
  dplyr::rename(pop = value, abbrev = Abbreviation) #region, pop, abbrev


#plan to subset by which rating, and by type of insurance
cols = c("rating", "consumersat", "prevention", "treatment")
whichrating = c(1,6,7,8)
ratingLabels = c("NCQA Overall", "Consumer Satisfaction", "Prevention", "Treatment")
instype= c("Commercial", "Medicare", "Medicaid")
plantype = c("PPO", "HMO", "HMO/POS", "HMO/POS/PPO", "POS", "HMO/PPO" )

#build function
#I don't fully understand this function!!! - hope to have a generic function for the mean within a state - retry?
mean_col <- function( dfrm, col ) mean( get(dfrm)[[ col ]], na.rm = TRUE )

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
      dplyr::rename(abbrev = state) 
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
comratings <- left_join(states, rated) %>%  select(region, meanrat) %>% dplyr::rename(value = meanrat) 
comratings

comratings2 <- left_join(states, rated.medicare) %>%  select(region, meanrat) %>% dplyr::rename(value = meanrat) 
comratings2

comratings3 <- left_join(states, rated.medicaid) %>%  select(region, meanrat) %>% dplyr::rename(value = meanrat) 
comratings3

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


comratings <- left_join(states, rated) %>%  select(region, meanrat) %>% dplyr::rename(value = meanrat) 
comratings


state_choropleth(comratings, title = paste("Average", "Consumer Satisfaction", "Rating of",  instype[1], 
                                           "Insurance", "by State on a 0 (Low) to 5 (High) Scale"))


#summarize Overall NCQA Rating by state - Commercial - PPO
rated <-fulltable %>% 
  dplyr::group_by(state) %>% 
  filter(instype == "Commercial") %>% 
  filter(type="PPO")
dplyr::summarize(meanrat =mean(consumersat, na.rm=TRUE)) %>%  
  arrange(desc(meanrat)) %>% 
  dplyr::rename(abbrev = state) 
rated


comratings <- left_join(states, rated) %>%  select(region, meanrat) %>% dplyr::rename(value = meanrat) 
comratings


state_choropleth(comratings, title = paste("Average", "Consumer Satisfaction", "Rating of", plantype[1], instype[1], "Insurance", "by State"))

# help from www.reed.edu/data-at-reed/resources/R/rvest.html
# help from zevross.com/blog/2015/05/19/scrape-website-data-with-the-new-r-package-rvest/
# remember dplyr::group_by
# remember [i] and [j] in nested loops


#testing out tidycensus and tigris
#census api_key
api_key <- 'd0263456f8db63ed8dc1084210d76f4cef099dd3'

library(tidycensus)
library(tigris)
library(tidyverse)
library(ggplot2)
library(sf)
library(ggalt)
library(scales)
library(viridis)

census_api_key(api_key)
Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

#snap
snap <- get_acs(geography = "state",
                variables = c(total_households ="B22001_001",
                              snap_received ="B22001_002",
                              not_snap = "B22001_005"),
                geometry = TRUE)

snap %>%
  select(state=NAME, variable, estimate, geometry) %>%
  spread(key = variable, value = estimate) %>%
  mutate(pct_snap = round(100*snap_received/total_households,2)) %>%
  select(state, pct_snap, geometry) ->
  snap2

ggplot(snap2, aes(y=reorder(state, pct_snap), x=pct_snap)) +
  geom_lollipop(point.colour = "steelblue", point.size = 3,
                horizontal = TRUE) +
  scale_x_continuous(expand=c(0,0),
                     breaks = seq(0,45,by=15), limits = c(0,45)) +
  labs(x=NULL, y= NULL,
       title = "States by Percent Receiving SNAP",
       subtitle = "Data from ACS 2016",
       caption = "using the get_acs function from tidycensus")+
  theme_minimal(base_family = "Arial Narrow")

#now focus on modeling why less satisfaction with commercial insurance in Western US
library(readxl)
#path <- "/Users/MeganMcLeod/Documents/MSCR/Insurance Higgins/KFF_2016_2.csv"
path <- "/Users/MeganMcLeod/Documents/MSCR/Insurance Higgins/KFF_2016_2.csv"
hisat <- read.csv(path, header =TRUE) 

#clean up column names
names(hisat) <- c("state", "pctwhite","pctblack", "pcthispanic", "pctasian", "pctnatam", "pctislander", "2ormoreraces", "medianincome", "pctunemployed", "SNAP", "percapitaspend", "governor", "pctsmoking","rxopioiddeath","pedgunlaw", "safestoragelaw", "assaultweapban",
                  "disabilityprev", "anymentalill", "seriousmentalill", "teenalcabuse", "adultalcabuse", "BMIgt25", "physactivity", "providerrateincr", "inptrateincr", "outptrateincr", "physrateincr", "dentistrateincr", "MCOrateincr", "nursingrateincr", "pctadultswopcp", "doccost", "totalchcs", 
                  "svcdelivsites", "encounters")
hisat$state<- tolower(hisat$state)
#hisat$governor  <- as.factor(hisat$governor)
#note: replace independent for alaska governor with R, as he has run as R before most recent election (per Wikipedia)
#hisat$governor[2] <- 'R'

#now put together a complete table of predictors by plan and by state
#Add states by percent receiving snap as obtained from tidycensus
states2 <-dplyr::rename(states, state = abbrev )
dplyr::left_join(fulltable, states2, by = "state")-> fulltable2
fulltable2<- dplyr::rename(fulltable2, abbrev=state )
fulltable2<- dplyr::rename(fulltable2, state=region )
completetable<- dplyr::left_join(fulltable2, hisat, snap2, by = "state")
#completetable has 1191 obs of 49 var
completetable$plantype[is.na(completetable$plantype)] <-"Other"


#****************
#try modeling with multilevel model
library(lme4)
test.formula <- consumersat ~ instype + type + plantype + pctsmoking  + medianincome + pctadultswopcp +(1 | state/instype/plan)
model.matrix(test.formula, completetable)

fit <- lmer(consumersat ~ instype + type + plantype + pctsmoking  +medianincome +pctadultswopcp +
              (1 | state/instype/plan), control = lmerControl(optimizer ="Nelder_Mead"), data=completetable)
#Summary gives REML criterion, scaled residuals, fixed and random effects with R 
summary(fit)

fit2 <- lmer(consumersat ~ instype + pctsmoking + SNAP + (1 | state/instype/plan), data=completetable)
summary(fit2)
#**************
# test comparing models with purrr and map functions

completetable %>%
  split(.$instype) %>%
  map(~ lm(consumersat ~ pctunemployed + pctwhite + state + pctadultswopcp + type +plantype, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

class("r.squared")

#***************************
#try a model
# leave out type, state, treatment, +completetable$prevention, +completetable$type 
fit <- lm(completetable$consumersat ~ completetable$pctunemployed  +completetable$pctsmoking  
          +completetable$rxopioiddeath  +completetable$type +as.numeric(completetable$snap) 
          +completetable$pctadultswopcp + completetable$plantype + completetable$instype
          
)
summary(fit)
#AdjR2 0.28 with this
#AdjR2 to 0.31 if add state
#*********************
#CORRELATIONS
# check correlations of predictors with corrplot
var(hisat)
predictors <- hisat[,2:28]
#remove non-numerical vectors
predictors <- predictors[c(-13,-14,-15)]
library(corrplot)
m <- cor(predictors)
corrplot.mixed(m)

#now corrr network
library(corrr)
#devtools::install_github("drsimonj/corrr") 
#better names
names(predictors)[1]<-"Consumer Satisfaction"
names(predictors)[2]<-"Smoking"
names(predictors)[3]<-"Obesity"
names(predictors)[4]<-"Income"
names(predictors)[5]<-"PopDens"
names(predictors)[6]<-"DocDens"
names(predictors)[7]<-"PctInsured"
names(predictors)[8]<-"CostLiving"
names(predictors)[9]<-"Guns"
names(predictors)[10]<- "PctHMO"
names(predictors)[11]<-"HospBeds"
names(predictors)[13]<-"NoPCP"

predictors %>% correlate() %>% network_plot(min_cor = .25)

#*********
#STATE factpr modeling
#now prep for state factor modeling
#used all predictors and backward selection
# tried combos of variables that were correlated
# no additive benefit of guns* smoke * obese
#nor from pctinsured * docdens
# nor from cost of living/top 1%

# model with adj Rsquared of 0.5137
fit <- lm(hisat$satisfact ~  hisat$NoPlaceMedCare  +hisat$docdens + hisat$smoke + hisat$hospBeds + hisat$popdens +hisat$guns +
            hisat$pctdisabled +hisat$hmo)
summary(fit)

#next adjR2 0.5512
fit <- lm(hisat$satisfact ~  hisat$smoke +hisat$popdens*hisat$docdens*hisat$hospBeds+ hisat$guns + hisat$avgInsPrem  +
            hisat$NoPlaceMedCare) 
summary(fit)

# model with adj Rsquared of 0.4812
fit <- lm(hisat$satisfact ~  hisat$smoke +hisat$NoPlaceMedCare  +hisat$docdens + hisat$smoke + hisat$hospBeds)
summary(fit)

#combine docdens w/ pctins to get next best model : 0.46
hisat$ddpctins <- hisat$docdens * hisat$pctinsured
fit <- lm(hisat$satisfact ~ hisat$NoPlaceMedCare + hisat$ddpctins +hisat$smoke +  hisat$popdens + hisat$avgInsPrem + 
            hisat$pctdisabled)
summary(fit)

# nextbest model
fit <- lm(hisat$satisfact ~ hisat$docdens  + hisat$smoke)
summary(fit)

#model checking
hisat$yhat <- predict(fit, hisat)
plot(hisat$satisfact, hisat$yhat)

hisat$resid <- hisat$satisfact - hisat$yhat
plot(hisat$resid)

#close model
fit <- lm(hisat$satisfact ~ hisat$docdens  + hisat$smoke + hisat$pctinsured)
summary(fit)

#high adj R2
fit <- lm(hisat$satisfact ~ hisat$pctinsured + hisat$docdens  +hisat$smoke +hisat$popdens)
summary(fit)

#lower adj R2
fit <- lm(hisat$satisfact ~ hisat$docdens  +hisat$smoke +hisat$popdens)
summary(fit)

#all possible models
# need to install leaps pkg
#hospbeds and insurance premiums interact interestingly, might be worth looking into an interaction
#high guns = high satisfaction high r levels
#nopcp is a big deal, non-smoking states do better, 
#publish this paper AND our repository - share the data and the code
#later compare all-comers to ibd patients

library(leaps)
with (hisat,
      leaps<-regsubsets(satisfact~hisat$smoke+hisat$obese+
                          hisat$income+hisat$popdens+ hisat$docdens+hisat$pctinsured +
                          hisat$pctdisabled + hisat$col +hisat$top1 +hisat$HIV +hisat$guns + hisat$hmo +
                          hisat$compIns +hisat$hospBeds + hisat$avgInsPrem +NoPlaceMedCare,
                        data=hisat,nbest=5))
summary(leaps)
plot(leaps,scale="r2")

