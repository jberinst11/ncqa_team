#09_correlations using corrplot
 
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
