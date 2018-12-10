#10_modeling -try modeling with multilevel model
library(lme4)
fit<- lmer(consumersat ~ instype +type + plantype + pctsmoking_2016  +medianincome_2016 +pctwoPCP_2016 
           + hospbedsper1k_2016 + (1 | state/instype/plan), data=completetable)
summary(fit)
fit <- lmer(consumersat ~ instype + type + plantype + pctsmoking_2016  +medianincome_2016 +pctwoPCP_2016 +
              (1 | state/instype/plan), control = lmerControl(optimizer ="Nelder_Mead"), data=completetable)
#Summary gives REML criterion, scaled residuals, fixed and random effects with R 
summary(fit)
completetable$pctunemployed_2016
fit2 <- lmer(consumersat ~ instype + pctsmoking_2016 + SNAP_2016 + (1 | state/instype/plan), data=completetable)
summary(fit2)



# test comparing models with purrr and map functions

completetable %>%
  split(.$instype) %>%
  map(~ lm(consumersat ~ pctunemployed_2016 + pctwhite_2016 + state + pctwoPCP_2016 + type +plantype, data = .)) %>%
  map(summary) %>%
  map_dbl("r.squared")

class("r.squared")

#***************************
#try a model
# leave out type, state, treatment, +completetable$prevention, +completetable$type 
fit <- lm(completetable$consumersat ~ completetable$pctunemployed_2016 
          +completetable$rxopioiddeath_2016  +as.numeric(completetable$SNAP_2016) 
         +completetable$anymentalill_1516 +completetable$pctBMIgt25_2016
          
          
)
summary(fit)
#AdjR2 0.28 with this
#AdjR2 to 0.31 if add state

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


#high adj R2
fit <- lm(completetablet$consumersat ~ hisat$pctinsured + hisat$docdens  +hisat$s +hisat$popdens)
summary(fit)

#lower adj R2
fit <- lm(hisat$satisfact ~ hisat$docdens  +hisat$smoke +hisat$popdens)
summary(fit)

library(leaps)
with (hisat,
      leaps<-regsubsets(completetable$consumersat~ hisat$pctwhite_2016,hisat$pctblack_2016, hisat$medianincome_2016, 
                        hisat$pctunemployed_2016, hisat$SNAP_2016, hisat$percapitaspend_2016, hisat$governor_2018, hisat$pctsmoking_2016,hisat$rxopioiddeath_2016, hisat$pedgunlaw_2014, 
                  hisat$safestoragelaw_2014, hisat$assaultweapban_2014,hisat$disabilityprev_2016, hisat$anymentalill_1516, hisat$seriousmentalill_1516, hisat$teenalcabuse_1516, hisat$adultalcabuse_1516, hisat$pctBMIgt25_2016, hisat$physactivity_2016, hisat$providerrateincr_2016, 
                        hisat$inptrateinc_2016, hisat$outptrateincr_2016, hisat$physrateincr_2016, hisat$pctadultswousplace_2014, hisat$pctwoPCP_2016, hisat$prohibdoccost_2016, hisat$totalchcs_2016,
                        hisat$svcdelivsites_2016, hisat$encounters_2016, hisat$hospbedsper1k_2016,
                        data=hisat,nbest=5))
summary(leaps)
plot(leaps,scale="r2")

