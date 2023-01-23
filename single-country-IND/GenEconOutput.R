#-------------------
# Generate Econ Output - India
# Rebecca Clark
# Updated 23 January 2023
#-------------------

# Objectives: 
#  Run the model with the parameter sets from calibration. 
#  Generate the necessary econ output

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
  source(here("R", "workflow", "run_param_set_econ_new.R"))
  
})


# 2. Load in information

# Country code
cc <- "IND"

# Percent RR-TB
rr_pct <- 0.028   

# Fitted parameters
parameters <- fread("./processing_files/param_sets/IND_params.csv")

# Vaccine parameters
vx_scenarios <- fread("./processing_files/econ_vx_scenarios.csv")


# 3. Make Directories
if(!dir.exists(here("econ_output"))) dir.create(here("econ_output"), showWarnings = F)
if(!dir.exists(here("econ_output/IND_TB"))) dir.create(here("econ_output/IND_TB"), showWarnings = F)
if(!dir.exists(here("econ_output/IND_TB_HIV"))) dir.create(here("econ_output/IND_TB_HIV"), showWarnings = F)
if(!dir.exists(here("econ_output/IND_alldeaths"))) dir.create(here("econ_output/IND_alldeaths"), showWarnings = F)


# 4. Generate the output and write out 
for (j in 1:nrow(parameters)) {
  
  print(paste0("Start time = ", Sys.time()))
  
  print(paste0("Starting parameter set number ", j))
  
  # select row j of parameters
  params <- parameters[j, ]
  
  # save the uid
  params_uid <- params[, uid]
  
  print(paste0("uid = ", params_uid))
  
  if (paste0(params_uid, ".parquet") %in% list.files(paste0("econ_output/", cc, "_alldeaths/"))){
    
    print("Already done parameter set!")
    
  } else {
    
    # get rid of everything except parameters
    params <- params[, !c("uid", "nhits")]
    params <- unlist(params)
    
    
    # Run through all the vaccine scenarios for each parameter set
    
    cc_TB_param     <- list()
    cc_deaths_param <- list()
    cc_TB_HIV_param <- list()
    
    
    for (i in 1:nrow(vx_scenarios)){
      
      # subset to the i-th row of characteristics
      vx_chars <- vx_scenarios[i,]
      
      print(paste0("Running scenario number ", i, ": ", vx_chars$runtype))
      
      # run the model with the row of parameters
      vx_scen_output <- run_param_set_econ(model, cc, params, params_uid, vx_chars, rr_pct)
      
      cc_TB_param[[i]] <- vx_scen_output[["cc_TB"]]
      cc_TB_HIV_param[[i]] <- vx_scen_output[["cc_TB_HIV"]]
      cc_deaths_param[[i]] <- vx_scen_output[["cc_deaths"]]
      
    }
    
    
    cc_TB <- rbindlist(cc_TB_param)
    rm(cc_TB_param)
    
    cc_TB_HIV <- rbindlist(cc_TB_HIV_param)
    rm(cc_TB_HIV_param)
    
    cc_alldeaths <- rbindlist(cc_deaths_param)
    rm(cc_deaths_param)
    
    
    write_parquet(cc_TB, file.path("econ_output", paste0(cc, "_TB"),
                                   paste0(params_uid, ".parquet")))
    rm(cc_TB)
    
    
    write_parquet(cc_TB_HIV, file.path("econ_output", paste0(cc, "_TB_HIV"),
                                       paste0(params_uid, ".parquet")))
    rm(cc_TB_HIV)
    
    
    write_parquet(cc_alldeaths, file.path("econ_output", paste0(cc, "_alldeaths"),
                                          paste0(params_uid, ".parquet")))
    rm(cc_alldeaths)
    
    
    print(paste0("End time for parameter set ", j, " = ", Sys.time()))
    
  }
  
}

#----end



