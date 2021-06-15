# Install and library packages --------------------------------------------------------

# install.packages("here")
# 
# install_64bit <- TRUE
# 
# if(install_64bit){
#   args <- c("--no-multiarch")
# } else{
#   args <- c("")
# }
# 
# devtools::install_github("fishfollower/SAM/stockassessment", INSTALL_opts=args)
# 
# remotes::install_github("r4ss/r4ss", branch ="development")
# 
# remotes::install_github("Bai-Li-NOAA/saconvert")
# 
# remotes::install_github("nmfs-general-modeling-tools/nmfspalette")

library(here)
library(stockassessment) # For using SAM
library(r4ss) # For using SS
library(saconvert)
library(nmfspalette)

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

# SAM2SS --------------------------------------------------------------------------------------
scenario_path <- c("A", "B")
slx_pattern <- c(20, 20)
f_method <- c(3, 2)


for (scenario_id in seq_along(scenario_path)){
  output_path <- here::here("NOBA_cod_files", "output", scenario_path[scenario_id])
  if(!dir.exists(output_path)) dir.create(output_path)
  
  saconvert::ICES2SS(
    user.wd = sam_input_path,
    user.od = output_path,
    ices.id = "",
    tvslx = FALSE,
    ages = NULL,
    nsexes = 1, # 1: one sex; 2: two sex; -1: one sex and multiply the spawning biomass by the fraction female in the control file
    forN = 2,
    q.extra.se = FALSE,
    q.float = FALSE,
    slx = slx_pattern[scenario_id],
    f.method = f_method[scenario_id]
  ) # steep.init: http://sedarweb.org/docs/wpapers/SEDAR19_DW_06_SteepnessInference.pdf
  
}

# Compare SAM and SS estimates ----------------------------------------------------------------
sam_output <- data.frame(
  "Year" = c(sam_dat$years),
  "SSB" = exp(sam_fit$sdrep$value[names(sam_fit$sdrep$value) %in% "logssb"]),
  "Recruits" = exp(sam_fit$sdrep$value[names(sam_fit$sdrep$value) %in% "logR"]),
  "F" = exp(sam_fit$sdrep$value[names(sam_fit$sdrep$value) %in% "logfbar"]),
  "Catch" = c(exp(sam_fit$sdrep$value[names(sam_fit$sdrep$value) %in% "logCatch"]), NA)
)

ss_output <- list()
for (scenario_id in seq_along(scenario_path)){
  ss_output_path <- here::here("NOBA_cod_files", "output", scenario_path[scenario_id])
  ss_output_data <- SS_output(dir = ss_output_path, verbose = T, printstats = T)
  ss_output[[scenario_id]] <- data.frame(
    "Year" = sam_output$Year,
    "SSB" = ss_output_data$timeseries$SpawnBio[ss_output_data$timeseries$Yr %in% sam_output$Year],
    "Recruits" = ss_output_data$timeseries$Recruit_0[ss_output_data$timeseries$Yr %in% (sam_output$Year-1)],
    "F" = ss_output_data$timeseries$`F:_1`[ss_output_data$timeseries$Yr %in% sam_output$Year],
    "Catch" = ss_output_data$timeseries$`sel(B):_1`[ss_output_data$timeseries$Yr %in% sam_output$Year]
  )
}

var <- c("SSB", "Recruits", "F", "Catch")
var_label <- c("SSB", "Recruits (Age 3)", expression(F[9-13]), "Catch (in weight)")
colors <- nmfspalette::nmfs_palette("regional web")(length(scenario_path)+1)

par(mfrow = c(2, 2), mar = c(4, 4, 1, 1))

for (i in seq_along(var)){
  plot(NA,
       type="n",
       xlim = range(sam_output$Year),
       ylim = range(sam_output[, var[i]], na.rm = T),
       xlab = "Year",
       ylab = var_label[i]
  )
  lines(sam_output$Year,
        sam_output[, var[i]],
        # type="o", 
        # pch=1, 
        # cex=0.6, 
        lty=1,
        col=colors[1])
  for (j in seq_along(scenario_path)){
    lines(ss_output[[j]]$Year,
          ss_output[[j]][, var[i]],
          # type="o", 
          # pch=j+1, 
          # cex=0.6, 
          lty=j+1,
          col=colors[j+1])
  }
}

legend("top", 
       c("SAM", paste("SS_", scenario_path)),
       # pch = seq_along(colors),
       lty = seq_along(colors),
       col = colors,
       bty="n")


