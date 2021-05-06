#Directory with SS files
model_dir <- "NOBA_cod_files/"

#Name of SS data file
datfile_name <- "ss3.dat"

#CVs for length at age, catch, and survey
CVs <- list("lenage"=0.1, "fishery"=0.01, "survey"=0.1)

#Load in model object from SAM
noba_cod_out <- system.file("inst","extdata","NOBA_cod_files","NEAcod-2020","run","model.RData",
                            package = "atlantisom") 
#This is the SAM object
noba_fit <- noba_cod_out$fit

noba_fit


properties <- c("minAge","maxAge", "maxAgePlusGroup")

age_info <- lapply(properties, get, noba_fit$conf)
names(age_info) <- properties

idx <- which(names(noba_fit$sdrep$value)=="logfbar")
names(noba_fit$sdrep$value)

plot(exp(noba_fit$sdrep$value[idx]))

#install SAM
remotes::install_github("fishfollower/SAM/stockassessment")
require(stockassessment)