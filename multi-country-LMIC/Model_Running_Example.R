# ----------------------------------------------
# Example of running the model
# (no-new-vaccine baseline)
# Rebecca Clark
# 21 July 2023
# --------------------------------------------

rm(list=ls())

# Load in the requirements
suppressPackageStartupMessages({
  model = new.env()
  source(here::here("R","include-v11.R"), model)
  source(here::here("R","TBVx-run-v1.R"), model)
  library(ggplot2)
  library(cowplot)
  theme_set(theme_minimal_grid() + panel_border(color = "black"))
})

# Set the country code
cc = "IDN"

# Load in fitted parameter sets for the country 
parameters <- fread(paste0("./param_sets/", cc, "_params.csv"))


# Select a parameter set to run
j = 1
params <- parameters[j, ]
params_uid <- params[, uid]


# Remove the unique ID variable and the n_hit variable
params <- params[, !c("uid", "n_hit")]
params <- unlist(params)


# Set the locations of the files for the code to read in
paths = model$set.paths(countries   = "countries", 
                        countrycode = cc, 
                        xml         = "XMLinput.xml", 
                        parameters  = "input.csv",
                        targets     = "target.csv")


# Run the model with the parameter set
output = model$run(model, new.parameter.values = params)



# ---- end




