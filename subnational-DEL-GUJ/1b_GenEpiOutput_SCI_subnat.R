#-------------------
# GenEpiOutput_subnat.R
# Rebecca Clark
# Last updated 7 June 2024
#-------------------

# Updates to run on 100 nodes (10 params each)

# Generate the epi output for Delhi and Gujarat

# 1. Set-up
suppressPackageStartupMessages({
  rm(list=ls())
  model = new.env()
  library(here)
  library(data.table)
  library(renv)
  library(digest)
  library(log4r)
  library(fst)
  library(arrow)
  library(logger)
  library(getopt)
  
  source(here("R","include-v11.R"), model)
  source(here::here("R","TBVx-run-v1.R"), model)
  source(here("R", "workflow", "run_param_set_epi.R"))
  
})

# 2. Load in the country code and set the name of the csv with the vx_scenarios
if (T){ # on local machine
  cc <- "DEL"
  vx_scenarios <- "SCI_vx_scenarios.csv"
  param_sets <- "DEL_SCI_params.csv"
  grid_task_int <- 1
  
} else{ # on cluster
  opts = getopt(matrix(c('cc','c', 1, "character",
                         'scenario', 's', 1, "character",
                         'params', 'p', 1, "character"),
                       byrow=TRUE, ncol=4))
  
  grid_task_int <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID"))
  cc <- opts$cc
  vx_scenarios <- opts$scenario
  param_sets <- opts$params
  
}

# Load in the fitted parameters for the country
parameters <- fread(here("./processing_files/param_sets/", param_sets))
#parameters <- parameters[(10*(grid_task_int-1) + 1):(10*grid_task_int), ]

# Read in the csv with the different vx_scenarios to run 
vx_scenarios <- fread(here("./processing_files/", vx_scenarios))


# 3. Make Directories

if(!dir.exists(here("epi_output"))) dir.create(here("epi_output"), showWarnings = F)
if(!dir.exists(here("epi_output/n_epi"))) dir.create(here("epi_output/n_epi"), showWarnings = F)
if(!dir.exists(here("epi_output/n_epi", cc))) dir.create(here("epi_output/n_epi", cc), showWarnings = F)


# 4. Generate the output and write out 
for (j in 1:nrow(parameters)){
  
  print(paste0("Start time = ", Sys.time()))
  
  print(paste0("Starting parameter set number ", j))
  
  # select row j of parameters
  params <- parameters[j, ]
  
  # save the uid
  params_uid <- params[, uid]
  
  print(paste0("uid = ", params_uid))
  
  if (paste0(params_uid, ".parquet") %in% list.files(paste0("epi_output/n_epi/", cc, "/"))){
    
    print("Already done parameter set!")
    
  } else {
    
    # Get rid of everything except parameters
    params <- params[, !c("uid", "nhits")]
    params <- unlist(params)
    
    # Run through all the vaccine scenarios for each parameter set
    cc_n_epi_param <- list()
    
    for (i in 1:nrow(vx_scenarios)){

      # subset to the i-th row of characteristics
      vx_chars <- vx_scenarios[i,]
      
      print(paste0("Running scenario number ", i, ": ", vx_chars$runtype))
      
      # run the model with the row of parameters
      vx_scen_output <- run_param_set_epi(model, cc, params, params_uid, vx_chars)
      
      cc_n_epi_param[[i]]  <- vx_scen_output[["n_epi"]]
      
    }
    
    write_parquet(rbindlist(cc_n_epi_param), here("epi_output", "n_epi", cc, paste0(params_uid, ".parquet")))
    rm(cc_n_epi_param)
    
    print(paste0("End time for parameter set ", j, " = ", Sys.time()))
    
    
  }
}


# ---- end

