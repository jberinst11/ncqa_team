#05 a bit of dataviz - visualize distribution of insurance set satisfaction overall, focus on commercial, or facet based on plan type/instype
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