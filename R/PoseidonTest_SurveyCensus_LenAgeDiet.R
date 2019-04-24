# script for using create_survey with the true numbers at age directly
# and then sample_fish to get a biolgical sample dataset
# from that dataset:
#   age samples from sample_ages
#   length samples from sample_lengths (TO BE WRITTEN)
#   weight samples from sample_weights (TO BE WRITTEN)
#   diet samples from sample_diet
#
# S. Gaichas Feb 2017, revised Apr 2019

library(tidyr)
library(dplyr)
library(ggplot2)
library(data.table)
library(atlantisom)
library(here)

# for reference, this is the call to run_truth for NEUS1.0 in trunk
# this model produces required files, but is untuned

#define directories using here
d.name <- here("atlantisoutput", "NEUStest20160303")

#define group names
#groups2 <- load_fgs(dir = "test20160303", "NeusGroups.csv")
groups2 <- load_fgs(d.name, "NeusGroups.csv") 
groups2 <- groups2[groups2$IsTurnedOn > 0, "Name"]

#this takes a long time
run_truth(scenario = "neusDynEffort_Test1_",
          dir = d.name,
          file_fgs = "NeusGroups.csv",
          file_bgm = "neus30_2006.bgm",
          select_groups = groups2,
          file_init = "inneus_2012.nc",
          file_biolprm = "at_biol_neus_v15_DE.prm",
          file_runprm = "at_run_neus_v15_DE.xml")

load(file.path(d.name, "outputneusDynEffort_Test1_run_truth.Rdata"))

# this is the create survey call using the loaded run_truth output "result"

# make defaults that return a census, implement in create_survey
# should return a perfectly scaled survey using groups2
effic1 <- data.frame(species=groups2, efficiency=rep(1.0,length(groups2)))

# should return all lengths fully sampled
selex1 <- data.frame(species=rep(groups2, each=10),
                     agecl=rep(c(1:10),length(groups2)),
                     selex=rep(1.0,length(groups2)*10))

# should return all model areas
# WARNING boxnumbers are hard coded for NEUS1.0 in trunk
boxall <- c(0:29)

# this uses result$nums to sample numbers at age directly
# NOTE THAT THE BIOMASS TIME SERIES COMES FROM biomass_ages

survey_testNall <- create_survey(dat = result$nums,
                                 time = seq(0,251,5),
                                 species = groups2,
                                 boxes = boxall,
                                 effic = effic1,
                                 selex = selex1)

# what is true composition? need annual by species, use code from sample_fish
# do tidyly
dat2 <- survey_testNall %>%
  group_by(species, agecl, time) %>%
  summarize(numAtAge = sum(atoutput))

#dat<-survey_testNall
#dat2 <- aggregate(dat$atoutput,list(dat$species,dat$agecl,dat$time),sum)
#names(dat2) <- c("species","agecl","time","numAtAge")

totN <- dat2 %>%
  group_by(species, time) %>%
  summarize(totN = sum(numAtAge))

#totN <- aggregate(dat2$numAtAge,list(dat2$species,dat2$time),sum )
#names(totN) <- c("species","time","totN")

dat2totN <- merge(dat2, totN)

ageclcomp <- ggplot(dat2totN, aes(x=agecl, y=numAtAge/totN, col=time)) +
  geom_point()

ageclcomp + facet_wrap(~species, ncol=5) #, scales="free")


# setting the effN higher than actual numbers results in sampling all
effNall <- data.frame(species=groups2, effN=rep(1e+15, length(groups2)))
# rmultinom broke with that sample size
#comptestall <- sample_fish(survey_testNall, effNall)
#names(comptestall) <- c("species","agecl","polygon", "layer","time","numAtAgesamp")

# this one is high but not equal to total for numerous groups
effNhigh <- data.frame(species=groups2, effN=rep(1e+8, length(groups2)))

comptesthigh <- sample_fish(survey_testNall, effNhigh)
names(comptesthigh) <- c("species","agecl","polygon", "layer","time","numAtAgesamp")

comptesttot <- aggregate(comptesthigh$numAtAgesamp,list(comptesthigh$species,comptesthigh$time),sum )
names(comptesttot) <- c("species","time","totsamp")

comptestprop <- merge(comptesthigh, comptesttot)

# compare individual years, these proportions at age should match
comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==0), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==0), aes(x=agecl, y=numAtAgesamp/totsamp, color="samp"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("censuscomposition_time0.png", width=11, height=11)

comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==100), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==100), aes(x=agecl, y=numAtAgesamp/totsamp, color="samp"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("censuscomposition_time100.png", width=11, height=11)

comparecomps <- ggplot() +
  geom_point(data=subset(dat2totN, time==250), aes(x=agecl, y=numAtAge/totN, color="true"), alpha = 0.3) +
  geom_point(data=subset(comptestprop, time==250), aes(x=agecl, y=numAtAgesamp/totsamp, color="samp"), alpha = 0.3)

comparecomps + facet_wrap(~species, ncol=5)

ggsave("censuscomposition_time250.png", width=11, height=11)

# and they do
# differences for baleen and toothed whales are due to small total numbers

# now turn this into length comp with calc_age2length
# function takes resn and nums as inputs so run survey and sample functions on resn as well

survey_testresnall <- create_survey(dat = result$resn,
                                 time = seq(0,251,5),
                                 species = groups2,
                                 boxes = boxall,
                                 effic = effic1,
                                 selex = selex1)

lengthtesthigh <- sample_fish(survey_testNall, effNhigh)
# use fish sampled from N along with survey of resn in function
# calc_age2length uses nums (lengthtest) and finds matches in resn

# provides ouput for an individual species and time slice
# breaks for whole dataset (NA output all rows)

length_census <- calc_age2length(structn = lengthtesthigh,
                                 resn = survey_testresnall,
                                 nums = lengthtesthigh,
                                 biolprm = result$biolprm, fgs = result$fgs,
                                 CVlenage = 0.1, remove.zeroes=TRUE)
