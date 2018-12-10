#03_read in KFF data
#Read in Downloaded Kaiser Family Foundation data
##Would like to figure out how to do this with rvest but am running into difficulty due to large number of links
path <- "/Users/MeganMcLeod/Desktop/HealthInsHiggins/KFF_2016_2.csv"
hisat <- read.csv(path, header =TRUE) 

#clean up column names, label as var_year
names(hisat) <- c("state", "pctwhite_2016","pctblack_2016", "pcthispanic_2016", "pctasian_2016", "pctnatam_2016", "pctislander_2016", "2ormoreraces_2016", "medianincome_2016", "pctunemployed_2016", "SNAP_2016",
                  "percapitaspend_2016", "governor_2018", "pctsmoking_2016","rxopioiddeath_2016","pedgunlaw_2014", "safestoragelaw_2014", "assaultweapban_2014",
                  "disabilityprev_2016", "anymentalill_1516", "seriousmentalill_1516", "teenalcabuse_1516", "adultalcabuse_1516", "pctBMIgt25_2016", "physactivity_2016", "providerrateincr_2016", 
                  "inptrateinc_2016", "outptrateincr_2016", "physrateincr_2016", "dentistrateincr_2016", "MCOrateincr_2016", "nursingrateincr_2016", "pctadultswousplace_2014", "pctwoPCP_2016", "prohibdoccost_2016", "totalchcs_2016",
                  "svcdelivsites_2016", "encounters_2016", "hospbedsper1k_2016")
hisat$state<- tolower(hisat$state)
hisat$governor  <- as.factor(hisat$governor)
#note: replace independent for alaska governor with R, as he has run as R before most recent election (per Wikipedia)
hisat$governor_2018[2] <- 'Republican'

save(hisat, file = "/Users/Mrmcleod/Desktop/processed_data/hisat.RData")




