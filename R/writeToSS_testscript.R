#devtools::install_github("r4ss/r4ss", dependencies = FALSE)
require(r4ss)
#source("./config/sardine_config.R") this is above for illustration

stocksynthesis.data <- r4ss::SS_readdat_3.30(paste0("./inst/extdata/",
                                                    model_dir,
                                                    datfile_name))
#Test writing CPUE in biomass
stocksynthesis.data <- SS_write_ts(ss_data_list = stocksynthesis.data,
                                   ts_data = list(survObsBiom$atoutput[fish_years],
                                                  truecatchbio_ss$atoutput[truecatchbio_ss$time %in% fish_years]),
                                   CVs = c(CVs$survey,
                                           CVs$fishery),
                                   data_years = list((survObsBiom$time[fish_years]-survey_sample_time)/timestep+1,             fish_years),
                                   sampling_month = list(rep(survey_month,nyears),
                                                         rep(fishing_month,nyears)),
                                   units = c("biomass","biomass"),
                                   data_type=c("CPUE","catch"),
                                   fleets = c(2,1))
#Get the age bins
age_bin_names <- names(stocksynthesis.data$agecomp)[10:length(names(stocksynthesis.data$agecomp))]
age_bins <- sub("a","",age_bin_names)

age_comp_flat <- reformat_compositions(age_comp_data,
                                       round.places = 4,
                                       comp_type = "agecomp")

## Write age composition data for survey
stocksynthesis.data <- SS_write_comps(ss_data_list = stocksynthesis.data,
                                      comp_matrix = list(age_comp_flat[burnin:(burnin+nyears-1),]),
                                      data_rows = list(30:79),
                                      sampling_month = list(rep(survey_month,nyears)),
                                      data_type = c("agecomp"),
                                      fleet_number = c(1),
                                      bins = list(age_bins),
                                      caal_bool = c(FALSE))

len_comp_data <- ss_length_stdsurv$natlength
fish_len_comp_data <- catch_lengthwt_samp$natlength

if(fstepperyr>1){
  fish_len_comp_anndata <- fish_len_comp_data %>%
    mutate(yr = floor(time/fstepperyr)) %>%
    group_by(species, agecl, lower.bins, upper.bins, time=as.integer(yr)) %>%
    summarise(annnatlength=sum(atoutput)) %>%
    rename(atoutput = annnatlength)
} else {
  fish_len_comp_anndata <- fish_len_comp_data
}

caal_comp_flat <- reformat_compositions(len_comp_data,                                round.places=4,
                                        comp_type="caalcomp")


#remove burnin
caal_comp_final <- filter(caal_comp_flat,
                          time %in% survey_years)


#Add over age classes to get sample size
len_comp_flat <- reformat_compositions(len_comp_data,
                                       round.places = 0,
                                       comp_type="lencomp")
#remove burnin
len_comp_final <- filter(len_comp_flat,
                         time %in% survey_years)

length_bins <- as.integer(names(len_comp_final))
length_bins <- length_bins[!is.na(length_bins)]

# fishery length comps are still 5 timesteps per year
# need to aggregate to annual (done above)
# also,  make effN annual goal/fstepsperyr (done above)
fish_len_comp_flat <- reformat_compositions(fish_len_comp_anndata,
                                            round.places = 0,
                                            comp_type="lencomp")

#remove burnin works after adjustment above
fish_len_comp_final <- filter(fish_len_comp_flat,
                              time %in% fish_years)

notbins <- c("time", "nsamp")

# fish_length_bins <- as.integer(names(fish_len_comp_final))
# fish_length_bins <- fish_length_bins[!is.na(fish_length_bins)]

# need to fill empty length bins with 0s to have same bins as survey for SS_write_comps
missing.lengths <- setdiff(length_bins, names(fish_len_comp_final)[!names(fish_len_comp_final) %in% notbins])
fish_len_comp_final[as.character(missing.lengths)] <- 0                    # Add them, filled with '0's
fish_len_comp_final <- fish_len_comp_final[c("time", length_bins, "nsamp")]


# fishery age comps also 5 timesteps per year
if(fstepperyr>1){
  fish_age_comp_anndata <- catch_numsss_samp %>%
    mutate(yr = floor(time/fstepperyr)) %>%
    group_by(species, agecl, time=as.integer(yr)) %>%
    summarise(annnatage=sum(atoutput)) %>%
    rename(atoutput = annnatage)
} else {
  fish_age_comp_anndata <- catch_numsss_samp
}

fish_age_comp_flat <- reformat_compositions(fish_age_comp_anndata,
                                            comp_type="agecomp")

#remove burnin (not necessary?fish comps made with fish_years only)
fish_age_comp_final <- filter(fish_age_comp_flat,
                              time %in% fish_years)

# #SS_write_comps breaking because fishery age bins start with 2 not 1; extracting bins from fish file may help?
# fish_age_bins <- names(fish_age_comp_flat)[!names(fish_age_comp_flat) %in% notbins]

# that leaves an empty column in data file, so instead fill with 0s
missing.ages <- setdiff(age_bins, names(fish_age_comp_final)[!names(fish_age_comp_final) %in% notbins])
fish_age_comp_final[missing.ages] <- 0                    # Add them, filled with '0's
fish_age_comp_final <- fish_age_comp_final[c("time", age_bins, "nsamp")]

comp_list <- list(caal_comp_final,len_comp_final, fish_age_comp_final, fish_len_comp_final)

apply_month <- list(rep(survey_month, nrow(comp_list[[1]])),
                    rep(survey_month, nrow(comp_list[[2]])),
                    rep(fishing_month,nrow(comp_list[[3]])),
                    rep(fishing_month,nrow(comp_list[[4]])))


# This now runs by ensuring that survey and fishery compositions have the same bins
# (filled with 0s for missing bins in fishery relative to survey)

# Write CAAL and length composition data
stocksynthesis.data <- SS_write_comps(ss_data_list = stocksynthesis.data,
                                      comp_matrix = comp_list,
                                      data_rows = list((comp_list[[1]]$time-survey_sample_time)/timestep + 1 , (survey_years-survey_sample_time)/timestep + 1,fish_years,fish_years),
                                      sampling_month = apply_month,
                                      data_type = rep(c("agecomp", "lencomp"),2),
                                      fleet_number = c(2,2,1,1),
                                      bins = list(age_bins,
                                                  length_bins,
                                                  age_bins,
                                                  length_bins),
                                      caal_bool = c(TRUE, rep(FALSE,3)))

head(stocksynthesis.data$lencomp)
head(stocksynthesis.data$agecomp)

#Change length bin structure to match atlantis data
stocksynthesis.data$lbin_vector <- length_bins

#Get correct number of length bins
stocksynthesis.data$N_lbins <- length(length_bins)

#Set lbin_method to 1 - this makes the population length bins match the data bins
#When lbin_method==1, we just comment out the binwidth, minimum, and maximum size arguments since they aren't used
stocksynthesis.data$lbin_method <- 1
stocksynthesis.data$binwidth <- "#"
stocksynthesis.data$minimum_size <- "#"
stocksynthesis.data$maximum_size <- "#"

#Change minimum sample size to 0.001 for CAAL data (SS won't let it go lower than this)
stocksynthesis.data$age_info$minsamplesize <- rep(0.001,2)

SS_writedat_3.30(stocksynthesis.data, outfile = paste0("./inst/extdata/",model_dir,
                                                       datfile_name),
                 overwrite=TRUE)

#Load needed inputs for biological parameters
source(here("config/CC3Config.R"))
#biological parameters
biolprm <- load_biolprm(dir = d.name, file_biolprm = biol.prm.file)#"C:/Users/chris/Documents/GitHub/atlantisom",
#d.name <- "C:/Users/chris/Documents/GitHub/atlantisom"
#functional groups
fgs <- truth$fgs
runprm <- load_runprm(dir = d.name, file_runprm = run.prm.file)
YOY <- load_yoy(d.name, paste0("output", scenario.name, "YOY.txt"))
truenums_ss <- truth$nums[results$nums$species==species,]

YOY_ss <- YOY %>%
  select(Time, "SAR.0")

fullresZ <- calc_Z(yoy = YOY_ss,
                   nums = truenums_ss,
                   fgs = fgs,
                   biolprm = biolprm,
                   toutinc = runprm$toutinc)


meanwt_spp <- ss_length_stdsurv$muweight %>%
  filter(time>burnin) %>%
  group_by(agecl) %>%
  summarize(meanwt = mean(atoutput))

meanln_spp <- ss_length_stdsurv$mulen %>%
  filter(time>burnin) %>%
  group_by(agecl) %>%
  summarize(meanln = mean(atoutput), cvln=sd(atoutput)/mean(atoutput))

ctlfile_name <-  "sardEM_3_3.ctl"
sardine.ctl <-r4ss::SS_readctl_3.30(paste0("./inst/extdata/",
                                           model_dir,
                                           ctlfile_name))
sardine.ctl <- SS_write_biol(sardine.ctl, biolprm, "SAR", Z = mean(fullresZ$atoutput), wtsage=meanwt_spp)

li_a_use <- biolprm$wl[match(fgs$Code[match(species,fgs$Name)],biolprm$wl[, 1]), 2]/1000
li_b_use <- biolprm$wl[match(fgs$Code[match(species,fgs$Name)],biolprm$wl[, 1]), 3]


len_nonburn <- ss_length_stdsurv$mulen %>%
  filter(time > burnin*timestep)
plot(len_nonburn$atoutput~len_nonburn$agecl, col=len_nonburn$time)

length.data <- data.frame("Year"=(len_nonburn$time-survey_sample_time)/timestep+1, length=len_nonburn$atoutput, Weight=NA, Sex="Female", age=as.integer(len_nonburn$agecl))

#require(bbmle)
#vb_estimates <-sample_fit_vbgf(length.data, 25, 45, 0.4,
#  0.1, 0.1, 20, 40, 0.05, 0.01, 0.01,
#  30, 50, 0.6, 0.3, 0.3, 0.5, 10)


#Run stock synthesis model
# for estimation
run_stocksynthesis(species = species, model_dir = model_dir, admb_options = "", show_output = TRUE)
