# script for using create_survey with the true biomass directly
# and then sample_survey_biomass
#
# S. Gaichas January 2017

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
# visualze selectivity curves for each group
par(mfrow=c(2,2))
par(mar=c(4,4,1,1))
plot(selnontrawl$agecl, selnontrawl$selex)
plot(selpelagics$agecl, selpelagics$selex)
plot(seldemersals$agecl, seldemersals$selex)
plot(selselflats$agecl, selselflats$selex)
par(mfrow=c(1,1))

# implement dome shaped selectivity for a particular species and replace default
# not done yet staying simple for now

selexmix <- bind_rows(selnontrawl, selpelagics, seldemersals, selselflats)

# this uses result$biomass_ages to sample biomass directly

survey_testBstd <- create_survey(dat = result$biomass_ages,
                                 time = annualmidyear,
                                 species = survspp,
                                 boxes = boxsurv,
                                 effic = efficmix,
                                 selex = selexmix)

# use this constant 0 cv for testing
surv_cv_0 <- data.frame(species=groups2, cv=rep(0.0,length(groups2)))

# specify cv by species groups
surv_cv_nontrawl <- data.frame(species=nontrawl, cv=rep(1.0,length(nontrawl)))
surv_cv_pelagics <- data.frame(species=pelagics, cv=rep(0.5,length(pelagics)))
surv_cv_demersals <- data.frame(species=demersals, cv=rep(0.3,length(demersals)))
surv_cv_selflats <- data.frame(species=selflats, cv=rep(0.3,length(selflats)))

surv_cv_mix <- bind_rows(surv_cv_nontrawl, surv_cv_pelagics, surv_cv_demersals, surv_cv_selflats)

# call sample_survey_biomass with a bunch of 1s for weight at age
# in the code it multiplies atoutput by wtatage so this allows us to use
# biomass directly
wtage <- data.frame(species=rep(survspp, each=10),
                    agecl=rep(c(1:10),length(survspp)),
                    wtAtAge=rep(1.0,length(survspp)*10))

surveyB_frombio_effselcv <- sample_survey_biomass(survey_testBstd, surv_cv_mix, wtage)

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

#all species comparison (WARNING needs full census frombio dataset in memory to work)
compareB <-ggplot() +
  geom_line(data=surveyB_frombio, aes(x=time/5,y=atoutput, color="trueB")) +
  geom_point(data=atBtxt2tidy, aes(x=Time/365,y=biomass, color="txttrueB"),
             alpha = 1/10) +
  geom_line(data=surveyB_frombio_eff, aes(x=time/5,y=atoutput, color="stdeffB")) +
  geom_line(data=surveyB_frombio_effsel, aes(x=time/5,y=atoutput, color="stdeffselB")) +
  geom_point(data=surveyB_frombio_effselcv, aes(x=time/5,y=atoutput, color="stdeffcvselB"),
              alpha = 1/2)


compareB + facet_wrap(~species, ncol=5, scales="free")

ggsave("compareBtxt_stdeffselcv.png", width=11, height=11)

