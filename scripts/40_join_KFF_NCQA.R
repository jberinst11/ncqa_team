#08_join_KFF_SNAP -
#Put together a complete table of predictors by plan and by state
#Add states by percent receiving snap as obtained from tidycensus

#testing out tidycensus and tigris
#census api_key
api_key <- 'd0263456f8db63ed8dc1084210d76f4cef099dd3'

census_api_key(api_key)
Sys.getenv("CENSUS_API_KEY")
options(tigris_use_cache = TRUE)

#read in SNAP data using tidycensus package
snap <- get_acs(geography = "state",
                variables = c(total_households ="B22001_001",
                              snap_received ="B22001_002",
                              not_snap = "B22001_005"),
                geometry = TRUE)

snap <- snap %>%
  select(state=NAME, variable, estimate, geometry) %>%
  spread(key = variable, value = estimate) %>%
  mutate(pct_snap = round(100*snap_received/total_households,2))
snap2 <- select(snap, state, pct_snap, geometry) %>% 
  filter(state != "Puerto Rico")

#plot pct snap according to state
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

#now put together a complete table of predictors by plan and by state
#Add states by percent receiving snap as obtained from tidycensus
states2 <-dplyr::rename(states, state = abbrev )
left_join(fulltable, states2, by = "state")-> fulltable2
hisat <- mutate(hisat, state = str_to_title(state))
fulltable2 <- select(fulltable2, -state, state = State)
completetable<- left_join(fulltable2, hisat, snap2, by = "state")

#completetable has 1191 obs of 51 var
completetable$plantype[is.na(completetable$plantype)] <-"Other"
