# test new load_biolprm to bring in gape width pars

dir <- "/Users/sarah.gaichas/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/NOBA_sacc_38"

file_biolprm <- "nordic_biol_incl_harv_v_011_1skg.prm"

load_biolprm(dir, file_biolprm)

dir <- "/Users/sarah.gaichas/Documents/0_Data/Atlantis/Poseidon/poseidon-dev/atlantisoutput/CC2063_Sep2022"

file_biolprm <- "CalCurrentV3_Biol.prm"

load_biolprm(dir, file_biolprm)

config <- "/Users/sarah.gaichas/Documents/0_Data/ms-keyrun/data-raw/simulated-data/config/NOBA_sacc38Config.R"

d.name <- "/Users/sarah.gaichas/Documents/0_Data/ms-keyrun/simulated-data/atlantisoutput/NOBA_sacc_38"
functional.groups.file <- "nordic_groups_v04.csv"
biomass.pools.file <- "nordic_biol_v23.nc"
biol.prm.file <- "nordic_biol_incl_harv_v_011_1skg.prm"
box.file <- "Nordic02.bgm"
initial.conditions.file <- "nordic_biol_v23.nc"
run.prm.file <- "nordic_run_v01.xml"
scenario.name <- "nordic_runresults_01"
bioind.file <- "nordic_runresults_01BiomIndx.txt"
catch.file <- "nordic_runresults_01Catch.txt"
annage <- TRUE
fisheries.file <- "NoBAFisheries.csv"

