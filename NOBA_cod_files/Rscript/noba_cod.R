# devtools::load_all("C:/Users/bai.li/Documents/saconvert")
remotes::install_github("Bai-Li-NOAA/saconvert")

library(here)
library(stockassessment) # For using SAM
library(r4ss) # For using SS
library(saconvert)

# Read SAM input data (NAE cod) ---------------------------------------------------------------

# devtools::install_github("fishfollower/SAM/stockassessment")
sam_input_path <- here::here("NOBA_cod_files", "NEAcod-2020", "data")

# Read in data files
cn <- read.ices(file.path(sam_input_path, "cn.dat"))
cw <- read.ices(file.path(sam_input_path, "cw.dat"))
dw <- read.ices(file.path(sam_input_path, "dw.dat"))
lf <- read.ices(file.path(sam_input_path, "lf.dat"))
lw <- read.ices(file.path(sam_input_path, "lw.dat"))
mo <- read.ices(file.path(sam_input_path, "mo.dat"))
nm <- read.ices(file.path(sam_input_path, "nm.dat"))
pf <- read.ices(file.path(sam_input_path, "pf.dat"))
pm <- read.ices(file.path(sam_input_path, "pm.dat"))
sw <- read.ices(file.path(sam_input_path, "sw.dat"))
surveys <- read.ices(file.path(sam_input_path, "survey.dat"))

# Create a collected data object
sam_dat <- setup.sam.data(
  surveys = surveys,
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
# fit <- sam.fit(sam_dat, sam_conf, sam_par)

# Plot figures 
# par(mfrow=c(2, 2))
# ssbplot(fit)
# fbarplot(fit)
# recplot(fit)
# catchplot(fit)

# SAM2SS --------------------------------------------------------------------------------------

#### Recruitment age is 0 and bias adjustment in recruitment = TRUE
output_path <- here::here("NOBA_cod_files", "output", "age0")

saconvert::ICES2SS(
  user.wd = sam_input_path, 
  user.od = output_path,
  ices.id = "",
  slx = 12,
  tvslx = FALSE,
  ages = NULL,
  nsexes = 1, #1: one sex; 2: two sex; -1: one sex and multiply the spawning biomass by the fraction female in the control file
  forN = 2,
  q.extra.se = FALSE,
  q.float = FALSE,
  sigma.init = 0.5,
  steep.init = 1,
  start.age = 0,
  bias.adj = TRUE
) # steep.init: http://sedarweb.org/docs/wpapers/SEDAR19_DW_06_SteepnessInference.pdf

ss_ouput <- SS_output(dir = output_path, verbose=TRUE, printstats=TRUE)
SS_plots(ss_ouput)

#### Recruitment age is 1 and bias adjustment in recruitment = FALSE
output_path <- here::here("NOBA_cod_files", "output", "age1")

saconvert::ICES2SS(
  user.wd = sam_input_path, 
  user.od = output_path,
  ices.id = "",
  slx = 12,
  tvslx = FALSE,
  ages = NULL,
  nsexes = 1, #1: one sex; 2: two sex; -1: one sex and multiply the spawning biomass by the fraction female in the control file
  forN = 2,
  q.extra.se = FALSE,
  q.float = FALSE,
  sigma.init = 0.5,
  steep.init = 1,
  start.age = 1,
  bias.adj = FALSE
) # Source of steep.init: http://sedarweb.org/docs/wpapers/SEDAR19_DW_06_SteepnessInference.pdf

ss_ouput <- SS_output(dir = output_path, verbose=TRUE, printstats=TRUE)
SS_plots(ss_ouput)
