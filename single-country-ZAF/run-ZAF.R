rm(list=ls())
model = new.env()
# model = globalenv()
source(here::here("R","include-v11.R"),model)
source(here::here("R","TBVx-run-v1.R"),T)
taskenvvar="taskID"

model$paths = model$set.paths(countries   = "countries-examples", 
                              countrycode = "ZAF", 
                              xml         = "XML38HITS.xml", 
                              targets     = "target_L0_LTBI_all.csv",
                              lglevel     = "INFO")



output = run(model,write.to.file = T)

