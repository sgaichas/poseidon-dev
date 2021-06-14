# remotes::install_github("Bai-Li-NOAA/saconvert")


if(here()){
  args <- c("--no-multiarch")
} else{
  args <- c("")
}

remotes::install_github("r4ss/r4ss", branch ="development")
devtools::install_github("fishfollower/SAM/stockassessment", args)


library(here)
library(stockassessment) # For using SAM
library(r4ss) # For using SS
library(saconvert)

# Read SAM input data (NAE cod) ---------------------------------------------------------------


sam_input_path <- here::here("NOBA_cod_files", "NEAcod-2020", "data")

# Read in data files
regdat <- grep(".dat",list.files(sam_input_path))
filenames_ICES <- list.files(sam_input_path)[regdat]

#All objects are now on input_file_list
input_file_list <- lapply(file.path(sam_input_path,filenames_ICES), read.ices)
names(input_file_list) <- gsub(".dat","",filenames_ICES)

#Load into function environment as objects
list2env(input_file_list, environment())

# Create a collected data object
sam_dat <- setup.sam.data(
  surveys = survey,
  residual.fleet = cn,
  prop.mature = mo,
  stock.mean.weight = sw,
  catch.mean.weight = cw,
  dis.mean.weight = dw,
  land.mean.weight = lw,
  prop.f = pf,
  prop.m = pm,
  natural.mortality = nm,
  land.frac = lf
)

# Generate a default/minimalistic model configuration
sam_conf <- defcon(sam_dat)

# Generate default initial values for all our model parameters
sam_par <- defpar(sam_dat, sam_conf)

# Fit the SAM model
sam_fit <- sam.fit(sam_dat, sam_conf, sam_par)

# Plot figures
par(mfrow = c(2, 2))
ssbplot(sam_fit)
fbarplot(sam_fit)
recplot(sam_fit)
catchplot(sam_fit)

# Plot SAM selectivity
par(mfrow = c(1,1))
slex_age <- sam_conf$minAge:sam_conf$maxAge
plot(slex_age, apply(cn, 2, mean, na.rm = TRUE), 
     type="o", 
     xlab="Age",
     ylab="Number of fish")
legend("topleft", "Catch",
       bty="n")

par(mfrow = c(2, 2))
slex_age <- sam_conf$minAge:sam_conf$maxAge
survey_slex <- matrix(NA, ncol=length(surveys), nrow=length(slex_age))
row.names(survey_slex) <- slex_age

for (i in 1:length(surveys)) {
  
  survey_slex[colnames(surveys[[i]]),i] <- matrix(apply(surveys[[i]], 2, mean, na.rm = TRUE))
  
  plot(slex_age, survey_slex[,i], 
       col = i,
       type="o", 
       lty = i,
       xlab="Age",
       ylab="Number of fish")
  legend("topleft", names(surveys)[i],
         bty="n")
  
}

# SAM2SS --------------------------------------------------------------------------------------

#### Recruitment age is 0 and bias adjustment in recruitment = TRUE
i <- 1
path_final <- c("age0","age0_slx20", "age1")
output_path <- here::here("NOBA_cod_files", "output", path_final[i])
slx_pattern <- c(12,20,12)
start_age <- c(0,0,1)
bias_adj <- c(TRUE, TRUE, FALSE)

saconvert::ICES2SS(
  user.wd = sam_input_path,
  user.od = output_path,
  ices.id = "",
  slx = slx_pattern[i],
  tvslx = FALSE,
  ages = NULL,
  nsexes = 1, # 1: one sex; 2: two sex; -1: one sex and multiply the spawning biomass by the fraction female in the control file
  forN = 2,
  q.extra.se = FALSE,
  q.float = FALSE,
  sigma.init = 0.5,
  steep.init = 1,
  start.age = start_age[i],
  bias.adj = bias_adj[i]
) # steep.init: http://sedarweb.org/docs/wpapers/SEDAR19_DW_06_SteepnessInference.pdf

ss_ouput <- SS_output(dir = output_path, verbose = TRUE, printstats = TRUE)
SS_plots(ss_ouput)

# Compare SAM and SS estimates ----------------------------------------------------------------
ss_path <- here::here("NOBA_cod_files", "output", "age0_slx20")
# ss_path <- here::here("NOBA_cod_files", "output", "age0")
ss_fit <- SS_output(dir = ss_path, verbose = TRUE, printstats = TRUE)
par(mfrow = c(4, 2), mar = c(1, 4, 1, 0))
ssbplot(sam_fit)
plot(sam_dat$years,
  ss_fit$timeseries$SpawnBio[ss_fit$timeseries$Yr %in% sam_dat$years],
  ylim = range(0, 3000000),
  type = "o", lty = 1,
  xlab = "Year",
  ylab = "SSB"
)

recplot(sam_fit)
plot(sam_dat$years,
  ss_fit$timeseries$Recruit_0[ss_fit$timeseries$Yr %in% sam_dat$years],
  ylim = range(0, 2500000),
  type = "o", lty = 1,
  xlab = "Year",
  ylab = "R (age 0)"
)

fbarplot(sam_fit)
plot(sam_dat$years,
  ss_fit$timeseries$`F:_1`[ss_fit$timeseries$Yr %in% sam_dat$years],
  ylim = range(0, 2),
  type = "o", lty = 1,
  xlab = "Year",
  ylab = "F"
)

catchplot(sam_fit)
plot(sam_dat$years,
  ss_fit$timeseries$`sel(N):_1`[ss_fit$timeseries$Yr %in% sam_dat$years],
  ylim = range(0, 1500000),
  type = "o", lty = 1,
  xlab = "Year",
  ylab = "Catch"
)
