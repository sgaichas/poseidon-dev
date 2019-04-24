# Testing Poseidon R code, December 2015

  install.packages("devtools")
  devtools::install_github("r4atlantis/atlantisom")

  setwd("~/Documents/0_Data/Atlantis/Poseidon")
  #devtools::install_local("atlantisom-master")
  devtools::install_local("atlantisom")

  setwd("~/Documents/0_Data/Atlantis/Poseidon/atlantisom")

  sapply(list.files(pattern="[.]R$", path="R/", full.names=TRUE), source);

  require(atlantisom)

  #' run_atlantis(scenario = scenario,
  #'   dir = directory,
  #'   file_fgs = "functionalGroups.csv",
  #'   file_bgm = "VMPA_setas.bgm",
  #'   select_groups = groups,
  #'   file_init = "INIT_VMPA_Jan2015.nc",
  #'   file_biolprm = "VMPA_setas_biol_fishing_Trunk.prm")

  #fgs <- load_fgs(dir = "NEUS_Atlantis_Dynamic_Effort", "functionalGroups.csv")


  # NEUS SPECIAL NOTES
  #   1. had to add column "IsTurnedOn" with all 1 entries to functionalGroups.csv
  #   2. had to change run_atlantis; renamed run_atlantis_NEUS.R
  #       a. for old naming conventions (no "output" beginning filename), lines 61-64 replaced with 67-70; line 142 replaced with 143
  #       b. line 170 replaced with 171 NOTE THIS WAS A BUG; HARD CODED TEST MODEL
  #       c. added print functions for reading in components in run_atlantis_NEUS.R
  #       d. NEUS has no DietCheck output file so had to comment line 160 out--equivalent?
  #       e. As a result of 4 commented out line 163 that calculates pred consumption
  #       f. commented out 169-185 checking catch nc vs catch txt
  #       g. commented out line 187 consumption portion of final dataset

  library(tidyr)
  library(dplyr)
  library(data.table)

  #this is NEUS 1.0 original
  groups <- load_fgs(dir = "NEUS_Atlantis_Dynamic_Effort", "functionalGroups.csv")
  groups <- groups[groups$isVertebrate > 0, "Name"]

  run_atlantis_NEUS(scenario = "neusDynEffort_Base_Effort_",
               dir = "NEUS_Atlantis_Dynamic_Effort",
               file_fgs = "functionalGroups.csv",
               file_bgm = "neus30_2006.bgm",
               select_groups = groups,
               file_init = "inneus_2007.nc",
               file_biolprm = "at_biol_neus_DE.prm")

  load("NEUS_Atlantis_Dynamic_Effort/outputneusDynEffort_Base_Effort_run_atlantis.RData")

  #this is NEUS 1.0 in trunk code to produce required files, but untuned
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

  #survey parameters for 50% efficiency and selectivity by length
  effic <- data.frame(species=groups2, efficiency=rep(0.5,length(groups2)))

  selex <- data.frame(species=rep(groups2, each=10),
                      agecl=rep(c(1:10),length(groups2)),
                      selex=rep(0.5,length(groups2)*10))

  #and only some areas
  boxsub <- seq(5,20,1)

  #make defaults that return a census, implement in create_survey

  #should return a perfectly scaled survey using groups2
  effic1 <- data.frame(species=groups2, efficiency=rep(1.0,length(groups2)))

  #should return all lengths fully sampled
  selex1 <- data.frame(species=rep(groups2, each=10),
                      agecl=rep(c(1:10),length(groups2)),
                      selex=rep(1.0,length(groups2)*10))

  #should return all model areas
  boxall <- c(0:29)

  #sampling in time: annual given model output timestep
  #NEUS1t <- seq(50,100,5)

  #neustrunkt <- seq(83,283,5)

  survey_testB <- create_survey(dat = result$biomass_ages,
                                time = seq(0,251,5),
                                species = groups2[1:21],
                                boxes = seq(5,20,1),
                                effic = effic,
                                selex = selex)

  #this should be the same as "truth" unless I'm missing something
  #time is all timesteps but sampling a subset of species
  survey_testBall <- create_survey(dat = result$biomass_ages,
                                   time = c(0:251),
                                   species = groups2[1:21],
                                   boxes = boxall,
                                   effic = effic1,
                                   selex = selex1)

  surv_cv <- data.frame(species=groups2, cv=rep(0.3,length(groups2)))

  #check against biomass output text file? probably a bad idea
  #this one is NEUS 1.0 straight up
  #atBtxt <- read.table("NEUS_Atlantis_Dynamic_Effort/neusDynEffort_Base_Effort_BiomIndx.txt", header=T)

  #Gavin's test of NEUS 1.0 in trunk
  #atBtxt1 <- read.table("test20160122/neusDynEffort_Test1_BiomIndx.txt", header=T)

  #NEUS 1.0 in trunk for 50 years, Atlantis output file
  atBtxt2 <- read.table("test20160303/neusDynEffort_Test1_BiomIndx.txt", header=T)

    # sum over polygon and age class
  # truth
  PLFtrue <- with(result$biomass_ages[result$biomass_ages$species=="Planktiv_L_Fish",],
                  tapply(atoutput, list(time), sum))
  # after create_survey run as a census
  PLFsurvcensus <- with(survey_testBall[survey_testBall$species=="Planktiv_L_Fish",],
                        tapply(atoutput, list(time), sum))

  #plot(atBtxt$Time/365, atBtxt$FPL, ylim=c(0, max(atBtxt$FPL)))
  plot(atBtxt2$Time/365, atBtxt2$FPL, ylim=c(0, 3000000))
  #lines(totB$time[totB$species=="Planktiv_L_Fish"]/5, totB$biomass[totB$species=="Planktiv_L_Fish"], col="blue")
  #lines(atBtxt$Time/365, atBtxt$FPL)

  lines(seq(0,251,1)/5, PLFtrue, col="red")
  lines(seq(0,251,1)/5, PLFsurvcensus, col="green")

  #create_survey is wrong. (units??) similar trend but blown up.
  # FIXED code works need to defite selex correctly--ADD DEFAULT

  #try with numbers, see if same result, function was built for natage?
  survey_testNall <- create_survey(dat = result$nums,
                                   time = c(0:251),
                                   species = groups2[1:21],
                                   boxes = boxall,
                                   effic = effic1,
                                   selex = selex1)

  #NEUS 1.0 in trunk for 50 years, Atlantis output file--no N output?
  #atBtxt2 <- read.table("test20160303/neusDynEffort_Test1_BiomIndx.txt", header=T)

  # sum over polygon and age class
  # truth
  PLFtrueN <- with(result$nums[result$nums$species=="Planktiv_L_Fish",],
                  tapply(atoutput, list(time), sum))
  # after create_survey run as a census
  PLFsurvcensusN <- with(survey_testNall[survey_testNall$species=="Planktiv_L_Fish",],
                        tapply(atoutput, list(time), sum))

  plot(seq(0,251,1)/5, PLFtrueN, col="red", ylim=c(0,max(PLFtrueN, PLFsurvcensusN)))
  lines(seq(0,251,1)/5, PLFsurvcensusN, col="green")

  #numbers still off, same way, after survey blown up relative to true
  # FIXED spot on now, selex definition was the problem

  #following is testing individual components

  #test aggregateData.R

  #make smaller datasets for one species
  PLFresultB <- result$biomass_ages[result$biomass_ages$species=="Planktiv_L_Fish",]
  PLFresultN <- result$nums[result$nums$species=="Planktiv_L_Fish",]
  names(PLFresultB)
  #[1] "species"  "agecl"    "time"     "polygon"  "atoutput"
  dim(PLFresultB)
  #[1] 35830     5
  names(PLFresultN)
  #[1] "species"  "agecl"    "polygon"  "layer"    "time"     "atoutput"
  dim(PLFresultN)
  #[1] 62900     6

  testaggPLFB <- aggregateData(PLFresultB,
                                time = c(0:251),
                                species = groups2[1:21],
                                boxes = boxall)
  names(testaggPLFB)
  #[1] "species"  "agecl"    "polygon"  "time"     "numAtAge" #not natage, B
  dim(testaggPLFB)
  #[1] 35830     5

  testaggPLFN <- aggregateData(PLFresultN,
                               time = c(0:251),
                               species = groups2[1:21],
                               boxes = boxall)
  names(testaggPLFN)
  #[1] "species"  "agecl"    "polygon"  "time"     "numAtAge" #change this default name
  dim(testaggPLFN)
  #[1] 35830     5  aggregated over layer. correctly?

  PLFresultNsub <- PLFresultN %>%
    filter(polygon==20, time %in% (0:5))

  PLFaggNsub <- testaggPLFN %>%
    filter(polygon==20, time %in% (0:5))

  with(PLFresultNsub, tapply(atoutput, list(agecl, time), sum)) #looks correct

  #test merge portions of create_survey with small dataset
  effic <- effic1
  selex <- selex1
  aggDat <- testaggPLFN

  #surv <- merge(dens,boxes,by="polygon",all.x=T)
  surv <- aggDat   #this can be removed and uncomment density stuff above. Make sure to think how this plays into sampling
  #merge in efficiency
  surv <- merge(surv,effic,by="species",all.x=T)
  #merge in selex
  #this is where it blows up; repeats each obs x10
  # PROBLEM WAS SELEX DEFINITION, GIVE A DEFAULT IN CODE
  surv <- merge(surv,selex,by=c("species","agecl"),all.x=T)


#####################################################################
  #why don't we just use the biomass_ages directly? splicing out bits of sample_survey_biomass
   dat2<-survey_testB
   cv <- surv_cv

   dat2<-survey_testBall

  #> names(dat2)
  #[1] "species"  "agecl"    "polygon"  "layer"    "time"     "atoutput"
   dat2$biomass <- dat2$atoutput
  #>
    totB <- aggregate(dat2$biomass,list(dat2$species,dat2$time),sum)
    names(totB) <- c("species","time","biomass")
  #>
  #  > 	#add observation error
    totBobs <- merge(totB,cv,by="species",all.x=T)
    totBobs$var <- log(totBobs$cv^2+1)
    totBobs$obsBiomass <- rlnorm(nrow(totBobs), log(totBobs$biomass)-totBobs$var/2, sqrt(totBobs$var))

  #plot of true (after survey sampling) and survey bio with obs error
  maxplot <- with(totBobs, max(biomass[species=="Planktiv_S_Fish"],
                               obsBiomass[species=="Planktiv_S_Fish"]))

  #add true bio for the whole region, no selectivity or efficiency
  maxplot <- max(totB$biomass[totB$species=="Planktiv_S_Fish"])
  with(totBobs, plot(time[species=="Planktiv_S_Fish"],
                     biomass[species=="Planktiv_S_Fish"],
                     ylim=c(0,maxplot),
                     pch=16))
  with(totBobs, points(time[species=="Planktiv_S_Fish"],
                       obsBiomass[species=="Planktiv_S_Fish"]))
  with(totB, lines(time[species=="Planktiv_S_Fish"],
                   biomass[species=="Planktiv_S_Fish"]))

  library(ggplot2)
  trueB <- ggplot(totB, aes(time,biomass)) + geom_line()
  trueB + facet_wrap(~species, ncol=5, scales="free")

  survB <- ggplot() +
          geom_point(data=totBobs, aes(x=time,y=biomass, color="survey")) +
          geom_point(data=totBobs, aes(x=time,y=obsBiomass, color="+obserr")) +
          geom_line(data=totB, aes(x=time,y=biomass, color="trueB"))
  survB + facet_wrap(~species, ncol=5, scales="free")

  ggsave("surveyBtesttrunk_sampDec2016.png", width=11, height=8.5)


  #plot(atBtxt$Time/365, atBtxt$FPS, ylim=c(0,3000000))
  #lines(totB$time[totB$species=="Planktiv_S_Fish"], totB$biomass[totB$species=="Planktiv_S_Fish"])

  #plot(atBtxt$Time/365, atBtxt$FDE, ylim=c(0,300000))
  #lines(totB$time[totB$species=="Demersal_E_Fish"], totB$biomass[totB$species=="Demersal_F_Fish"])

  groupslookup <- load_fgs(dir = "test20160303", "NeusGroups.csv")
  groupslookup <- groupslookup %>%
    filter(IsTurnedOn > 0)

  #lookup the matching names, put in time, species, biomass column format
  atBtxt2tidy <- atBtxt2 %>%
    select(Time, FPL:DIN) %>%
    rename_(.dots=with(groupslookup, setNames(as.list(as.character(Code)), Name))) %>%
    gather(species, biomass, -Time)

#this works now
  compareB <-ggplot() +
    geom_line(data=totB, aes(x=time/5,y=biomass, color="trueB")) +
    geom_point(data=atBtxt2tidy, aes(x=Time/365,y=biomass, color="txttrueB"),
               alpha = 1/10)

  compareB + facet_wrap(~species, ncol=5, scales="free")

  ggsave("compareBtxt.png", width=11, height=11)

    #if I do biomass_age$atoutput divided by nums$atoutput I should have wt@age, yes?

  nums_test <- with(result$nums, aggregate(atoutput, list(species, agecl), sum))
  names(nums_test) <- c("species", "agecl", "nums")

  bio_test  <- with(result$biomass_ages, aggregate(atoutput, list(species, agecl), sum))
  names(bio_test) <- c("species", "agecl", "wt")

 calcwtage <- merge(bio_test, nums_test) %>%
     mutate(wtage=wt/nums)


  wtAtAge <- data.frame(species=rep(groups2, each=10),
                      agecl=rep(c(1:10),length(groups2)),
                      wtAtAge=wtage)


  survey_testN <- create_survey(dat = result$nums,
                                time = seq(50,100,5),
                                species = groups[1:21],
                                boxes = seq(5,20,1),
                                effic = effic,
                                selex = selex)


  NEUS_survB <- sample_survey_biomass(dat = survey_test,
                                      cv = surv_cv,
                                      )
