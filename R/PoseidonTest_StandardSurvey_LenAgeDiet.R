# script for using create_survey with the true numbers at age directly
# and then sample_fish to get a biolgical sample dataset
# from that dataset:
#   age samples from sample_ages
#   length samples from sample_lengths (TO BE WRITTEN)
#   weight samples from sample_weights (TO BE WRITTEN)
#   diet samples from sample_diet
#
# S. Gaichas Feb 2017

library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(atlantisom)

# for reference, this is the call to run_truth for NEUS1.0 in trunk
# this model produces required files, but is untuned

#define group names
groups2 <- load_fgs(dir = "test20160303", "NeusGroups.csv")
groups2 <- groups2[groups2$IsTurnedOn > 0, "Name"]

run_truth(scenario = "neusDynEffort_Test1_",
          dir = "test20160303",
          file_fgs = "NeusGroups.csv",
          file_bgm = "neus30_2006.bgm",
          select_groups = groups2,
          file_init = "inneus_2012.nc",
          file_biolprm = "at_biol_neus_v15_DE.prm",
          file_runprm = "at_run_neus_v15_DE.xml")

load("test20160303/outputneusDynEffort_Test1_run_truth.Rdata")

# this is the create survey call using the loaded run_truth output "result"

# make defaults that return a standard survey, implement in standard_survey
# users need to map their species groups into these general ones
#   large pelagics/reef associated/burrowers/otherwise non-trawlable
#   pelagics
#   demersals
#   selected flatfish

# NEUS species group definitions, see AtlantisGroupsSpeciesLookup.xlsx
nontrawl <- c("Pisciv_T_Fish", "Shark_D", "Shark_P", "Reptile", "Mesopel_M_Fish")
pelagics <- c("Planktiv_L_Fish", "Planktiv_S_Fish", "Benthopel_Fish", "Pisciv_S_Fish")
demersals <- c("Pisciv_D_Fish", "Demersal_D_Fish","Demersal_E_Fish", "Demersal_S_Fish",
               "Demersal_B_Fish", "Demersal_DC_Fish", "Demersal_O_Fish",  "Demersal_F_Fish",
               "Shark_B", "SkateRay")
selflats <- c("Pisciv_B_Fish")

# general specifications for bottom trawl survey:
#   once per year at mid year
#   could generalize from the run.prm file: tstop/365 is n years of run
#   max result$biomass_ages$time/ n years of run is steps per year
#   take midpoint of 0, steps per year to start seq and go to max time by steps per year
#   hardcoded for NEUS 1.0 in trunk here
annualmidyear <- seq(3,251,5)

#other options for NEUS
annualspring <- seq(2,251,5)
annualfall <- seq(4,251,5)
annualJan1 <- seq(0,251,5)

#   all fish (and sharks!! need dogfish in NEUS)
survspp <- result$fgs$Name[result$fgs$IsTurnedOn==1 &
                           result$fgs$GroupType %in% c("FISH", "SHARK")]

#   ~75% of boxes (leave off deeper boxes?)
#   cant think of a way to generalize across models, must be hard coded
#   hardcoded for NEUS 1.0 in trunk here
boxsurv <- c(1:21)

#   mixed efficiency
#     0.01 for large pelagics, reef dwellers, others not in trawlable habitat
#     0.1 for pelagics
#     0.7 for demersals
#     1.1 for selected flatfish

# survey efficiency specification by species group
effnontrawl <- data.frame(species=nontrawl, efficiency=rep(0.01,length(nontrawl)))
effpelagics <- data.frame(species=pelagics, efficiency=rep(0.1,length(pelagics)))
effdemersals <- data.frame(species=demersals, efficiency=rep(0.7,length(demersals)))
effselflats <- data.frame(species=selflats, efficiency=rep(1.1,length(selflats)))

efficmix <- bind_rows(effnontrawl, effpelagics, effdemersals, effselflats)

#   mixed selectivity (using 10 agecl for all species)
#     flat=1 for large pelagics, reef dwellers, others not in trawlable habitat
#     sigmoid 0 to 1 with 0.5 inflection at agecl 3 for pelagics, reaching 1 at agecl 5, flat top
#     sigmoid 0 to 1 with 0.5 inflection at agecl 5 for most demersals and flatfish, reaching 1 at agecl 7, flat top
#     dome shaped 0 to 1 at agecl 6&7 for selected demersals, falling off to 0.7 by agecl 10

sigmoid <- function(a,b,x) {
  1 / (1 + exp(-a-b*x))
}

# survey selectivity specification by species group
selnontrawl <- data.frame(species=rep(nontrawl, each=10),
                          agecl=rep(c(1:10),length(nontrawl)),
                          selex=rep(1.0,length(nontrawl)*10))
selpelagics <- data.frame(species=rep(pelagics, each=10),
                          agecl=rep(c(1:10),length(pelagics)),
                          selex=sigmoid(5,1,seq(-10,10,length.out=10)))
seldemersals <- data.frame(species=rep(demersals, each=10),
                          agecl=rep(c(1:10),length(demersals)),
                          selex=sigmoid(1,1,seq(-10,10,length.out=10)))
selselflats <- data.frame(species=rep(selflats, each=10),
                          agecl=rep(c(1:10),length(selflats)),
                          selex=sigmoid(1,1,seq(-10,10,length.out=10)))
# same selectivity for selflats and demersals for NEUS

# optional, visualze selectivity curves for each group
if(V) {
  par(mfrow=c(2,2))
  par(mar=c(4,4,1,1))
  plot(selnontrawl$agecl, selnontrawl$selex)
  plot(selpelagics$agecl, selpelagics$selex)
  plot(seldemersals$agecl, seldemersals$selex)
  plot(selselflats$agecl, selselflats$selex)
  par(mfrow=c(1,1))
}

# implement dome shaped selectivity for a particular species and replace default
# not done yet staying simple for now

selexmix <- bind_rows(selnontrawl, selpelagics, seldemersals, selselflats)

# this uses result$nums to sample numbers at age directly
# NOTE THAT THE BIOMASS TIME SERIES COMES FROM biomass_ages

survey_testNstd <- create_survey(dat = result$nums,
                                 time = annualJan1,
                                 species = survspp,
                                 boxes = boxsurv,
                                 effic = efficmix,
                                 selex = selexmix)

# from here can see what difference just the survey sampling makes
# apply a huge effN to the standard survey first

# what is true composition? need annual by species, use code from sample_fish
# WARNING assumes survey_testNall from PoseidonTest_SurveyCensus_LenAgeDiet in memory
dat<-survey_testNall
dat2 <- aggregate(dat$atoutput,list(dat$species,dat$agecl,dat$time),sum)
names(dat2) <- c("species","agecl","time","numAtAge")

totN <- aggregate(dat2$numAtAge,list(dat2$species,dat2$time),sum )
names(totN) <- c("species","time","totN")

dat2totN <- merge(dat2, totN)

# a huge effN to see the impact of standard survey with no sampling effect
effNhigh <- data.frame(species=survspp, effN=rep(1e+8, length(survspp)))

comptesthigh <- sample_fish(survey_testNstd, effNhigh)
names(comptesthigh) <- c("species","agecl","polygon", "layer","time","numAtAgesamp")

comptesttot <- aggregate(comptesthigh$numAtAgesamp,list(comptesthigh$species,comptesthigh$time),sum )
names(comptesttot) <- c("species","time","totsamp")

comptestprop <- merge(comptesthigh, comptesttot)

# compare individual years, these proportions at age wont match
comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==0), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==0), aes(x=agecl, y=numAtAgesamp/totsamp, color="samp"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("stdsurvcomp_higheffN_time0.png", width=11, height=11)

comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==100), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==100), aes(x=agecl, y=numAtAgesamp/totsamp, color="samp"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("stdsurvcomp_higheffN_time100.png", width=11, height=11)

comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==250), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==250), aes(x=agecl, y=numAtAgesamp/totsamp, color="samp"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("stdsurvcomp_higheffN_time250.png", width=11, height=11)

# can clearly see impact of selectivities here
# now actually subsample fish to see impact on composition

# sample_fish uses an effN effective sample size for multinomial
# but this should be the REAL sample size taken (annual average over whole area)
# sample size of fish for lengths--a fairly large number
# a proportion of this is used for ages below
# assign by groups as above

effNnontrawl <- data.frame(species=nontrawl, effN=rep(25,length(nontrawl)))
effNpelagics <- data.frame(species=pelagics, effN=rep(1000,length(pelagics)))
effNdemersals <- data.frame(species=demersals, effN=rep(1000,length(demersals)))
effNselflats <- data.frame(species=selflats, effN=rep(1000,length(selflats)))

effNmix <- bind_rows(effNnontrawl, effNpelagics, effNdemersals, effNselflats)

comptestmix <- sample_fish(survey_testNstd, effNmix)
names(comptestmix) <- c("species","agecl","polygon", "layer","time","numAtAgesamp")

comptestmixtot <- aggregate(comptestmix$numAtAgesamp,list(comptesthigh$species,comptesthigh$time),sum )
names(comptestmixtot) <- c("species","time","totsamp")

comptestmixprop <- merge(comptestmix, comptestmixtot)

# compare individual years, these proportions at age wont match
comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==0), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==0), aes(x=agecl, y=numAtAgesamp/totsamp, color="samphigh"), alpha = 0.3) +
  geom_point(data=subset(comptestmixprop, time==0), aes(x=agecl, y=numAtAgesamp/totsamp, color="sampreal"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("stdsurvcomp_realeffN_time0.png", width=11, height=11)

comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==100), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==100), aes(x=agecl, y=numAtAgesamp/totsamp, color="samphigh"), alpha = 0.3) +
  geom_point(data=subset(comptestmixprop, time==100), aes(x=agecl, y=numAtAgesamp/totsamp, color="sampreal"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("stdsurvcomp_realeffN_time100.png", width=11, height=11)

comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==250), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==250), aes(x=agecl, y=numAtAgesamp/totsamp, color="samphigh"), alpha = 0.3) +
  geom_point(data=subset(comptestmixprop, time==250), aes(x=agecl, y=numAtAgesamp/totsamp, color="sampreal"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("stdsurvcomp_realeffN_time250.png", width=11, height=11)

