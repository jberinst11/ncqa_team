#02_read_ncqa_data


# start wth 2015 ----------------------------------------------------------

for (i in 1:length(state_names))
{
  for (j in 1:length(instype))
  {
    healthins <- read_html(paste(links[1], "/", instype[j], "/", state_names[i], sep = ""))
    hitable<- as.data.frame(healthins %>% html_nodes("table") %>% .[[3]] %>% html_table(fill=T))
    rows<- dim(hitable)[[1]] #grab first dimension as number of rows
    hitable <- hitable[9:rows, 1:8]
    names(hitable) <-  c("rating", "plan", "state", "type", "ncqa", "consumersat", "prevention", "treatment")
    hitable$state <-  statename[i]
    hitable$instype <- instype[j]
    hitable$rating <- as.numeric(hitable$rating)
    hitable$consumersat <- as.numeric(hitable$consumersat)
    hitable$prevention <- as.numeric(hitable$prevention)
    hitable$treatment <- as.numeric(hitable$treatment)
    #remove NA plans
    junk <- is.na(hitable$plan)
    hitable[!junk,]
    #assign(paste("Table", instype[j], statename[i], sep=""), hitable)
    if (i ==1 & j==1) {
      fulltable <- hitable
    } else {
      fulltable <- rbind(fulltable, hitable)
    }
  }
}
#cleanup rows with no real plan or labeled as "2" (missing)
missing  <- is.na(fulltable$plan) | fulltable$plan == "2"
fulltable  <- fulltable[!missing,]
write.table(fulltable, "InsPlans2015")
write.csv(fulltable, "InsPlans2015.csv")



#-----------------------------------------------
#now read in csv file
# read data back in from csv file
fulltable <-  read.csv("/Users/MeganMcLeod/Desktop/HealthInsHiggins/InsPlans2016.csv", header=T) %>% select(-X)

