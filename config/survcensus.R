# Default survey configuration here is a census
# Need to define survey season, area, efficiency, selectivity
# Inherits species from input omlist_ss
survspp <- omlist_ss$species_ss 

# survey season and other time dimensioning parameters
# generalized timesteps all models
noutsteps <- omlist_ss$runpar$tstop/omlist_ss$runpar$outputstep
timeall <- c(0:noutsteps)

stepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutinc
midptyr <- round(median(seq(0,stepperyr)))

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc

# a survey that takes place once per year mid year has now been declared default
annualmidyear <- seq(midptyr, noutsteps, stepperyr)
survtime <- annualmidyear

# survey area
# should return all model areas
survboxes <- c(0:(omlist_ss$boxpars$nbox - 1))

# survey efficiency (q)
# should return a perfectly efficient survey 
surveffic <- data.frame(species=survspp,
                     efficiency=rep(1.0,length(survspp)))

# survey selectivity (agecl based)
# should return all age classes fully sampled (Atlantis output is 10 age groups per spp)
n_age_classes <- omlist_ss$funct.group_ss$NumCohorts
age_classes <- 1:n_age_classes

n_annages <- n_age_classes * omlist_ss$funct.group_ss$NumAgeClassSize
annages <- 1:n_annages

# this is by age class, need to change to use with ANNAGEBIO output
survselex <- data.frame(species=rep(survspp, each=n_age_classes),
                     agecl=rep(c(1:n_age_classes),length(survspp)),
                     selex=rep(1.0,length(survspp)*n_age_classes))

# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
surveffN <- data.frame(species=survspp, effN=rep(1e+8, length(survspp)))

# survey index cv needed for sample_survey_xxx
# perfect observation
surv_cv <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

