#Directory with SS files
model_dir <- "./NOBA_cod_files"
#Name of SS data file
#read in existing dat file (Atlantic cod from NE US)
datfile_name <- "ss3.dat"
dat_in <- r4ss::SS_readdat(file.path(model_dir,datfile_name))

#Load in model object from SAM
noba_cod_out <- get(load(file.path(model_dir,"NEAcod-2020","run","model.RData")))
#This is the SAM object
#Extract the estimated values
noba_estimates <- broom::tidy(noba_cod_out$sdrep$value)

#Get estimated F
filter(noba_estimates, names=="logfbar")
#Get estimated R
filter(noba_estimates, names=="logR")
#Get catch

#Get the ages from the conf file
properties <- c("minAge","maxAge", "maxAgePlusGroup")
age_info <- lapply(properties, get, noba_cod_out$conf)
names(age_info) <- properties

dat_in$Nages <- length(age_info$minAge:age_info$maxAge)


plot(exp(noba_fit$sdrep$value[idx]))

#install SAM
remotes::install_github("fishfollower/SAM/stockassessment")
require(stockassessment)