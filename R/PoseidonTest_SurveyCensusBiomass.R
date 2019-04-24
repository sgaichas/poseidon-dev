# script for using create_survey with the true biomass directly
# and then sample_survey_biomass
#
# S. Gaichas December 2016

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

# this uses result$biomass_ages to sample biomass directly
# WARNING! time and species group numbers are hard-coded for NEUS1.0 in trunk

survey_testBall <- create_survey(dat = result$biomass_ages,
                                 time = c(0:251),
                                 species = groups2[1:21],
                                 boxes = boxall,
                                 effic = effic1,
                                 selex = selex1)

# make up a constant 0 cv for testing
surv_cv <- data.frame(species=groups2, cv=rep(0.0,length(groups2)))

# call sample_survey_biomass with a bunch of 1s for weight at age
# in the code it multiplies atoutput by wtatage so this allows us to use
# biomass directly
wtage <- data.frame(species=rep(groups2, each=10),
                    agecl=rep(c(1:10),length(groups2)),
                    wtAtAge=rep(1.0,length(groups2)*10))

surveyB_frombio <- sample_survey_biomass(survey_testBall, surv_cv, wtage)

# test plot for one species
plot(surveyB_frombio$time[surveyB_frombio$species=="Planktiv_S_Fish"],
     surveyB_frombio$atoutput[surveyB_frombio$species=="Planktiv_S_Fish"])

# plot some comparisons with Atlantis output

# NEUS 1.0 in trunk for 50 years, Atlantis output file
atBtxt2 <- read.table("test20160303/neusDynEffort_Test1_BiomIndx.txt", header=T)

groupslookup <- load_fgs(dir = "test20160303", "NeusGroups.csv")
groupslookup <- groupslookup %>%
  filter(IsTurnedOn > 0)

# lookup the matching names, put in time, species, biomass column format
# WARNING hardcoded for NEUS1.0 in trunk
atBtxt2tidy <- atBtxt2 %>%
  select(Time, FPL:DIN) %>%
  rename_(.dots=with(groupslookup, setNames(as.list(as.character(Code)), Name))) %>%
  gather(species, biomass, -Time)

#all species comparison
compareB <-ggplot() +
  geom_line(data=surveyB_frombio, aes(x=time/5,y=atoutput, color="trueB")) +
  geom_point(data=atBtxt2tidy, aes(x=Time/365,y=biomass, color="txttrueB"),
             alpha = 1/10)

compareB + facet_wrap(~species, ncol=5, scales="free")

ggsave("compareBtxt.png", width=11, height=11)

