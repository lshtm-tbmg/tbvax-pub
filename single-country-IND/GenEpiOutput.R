#-------------------
# Generate Epi Output - India
# Rebecca Clark
# Updated 23 January 2023
#-------------------

# Objectives: 
#  Run the model with the parameter sets from calibration. 
#  Generate the necessary epi output

# 1. Set-up
suppressPackageStartupMessages({
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
  source(here("R", "workflow", "run_param_set_epi_new.R"))
  
})


# 2. Load in information

# Country code
cc <- "IND"

# Fitted parameters
parameters <- fread("./processing_files/param_sets/IND_params.csv")

# Vaccine parameters
vx_scenarios <- fread("./processing_files/epi_vx_scenarios.csv")


# 3. Make Directories
if(!dir.exists(here("epi_output"))) dir.create(here("epi_output"), showWarnings = F)
if(!dir.exists(here("epi_output/n_epi"))) dir.create(here("epi_output/n_epi"), showWarnings = F)


# 4. Generate the output and write out 
for (j in 1:nrow(parameters)){
  
  print(paste0("Start time = ", Sys.time()))
  
  print(paste0("Starting parameter set number ", j))
  
  # select row j of parameters
  params <- parameters[j, ]
  
  # save the uid
  params_uid <- params[, uid]
  
  print(paste0("uid = ", params_uid))
  
  if (paste0(params_uid, ".parquet") %in% list.files(paste0("epi_output/n_epi/"))){
    
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
    
    write_parquet(rbindlist(cc_n_epi_param), here("epi_output", "n_epi", paste0(params_uid, ".parquet")))
    rm(cc_n_epi_param)
    
    print(paste0("End time for parameter set ", j, " = ", Sys.time()))
    
    
  }
}


# ---- end

