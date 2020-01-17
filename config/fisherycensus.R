# Default fishery configuration here is a census
# Inherits species from input omlist_ss
fishspp <- omlist_ss$species_ss 

# same time dimensioning parameters as in surveycensus.R

# fishery output: learned the hard way this can be different from ecosystem outputs
fstepperyr <- if(omlist_ss$runpar$outputstepunit=="days") 365/omlist_ss$runpar$toutfinc

# fishery sampling area
# should return all model areas
fishboxes <- c(0:(omlist_ss$boxpars$nbox - 1))

# effective sample size needed for sample_fish
# this effective N is high but not equal to total for numerous groups
fisheffN <- data.frame(species=survspp, effN=rep(1e+8, length(survspp)))

# fishery catch cv can be used in sample_survey_biomass
# perfect observation
fish_cv <- data.frame(species=survspp, cv=rep(0.0,length(survspp)))

