require(here)
require(tools)
require(assertthat)
require(stringi)
require(Matrix)
require(xml2)
require(digest)
require(deSolve)
require(data.table)
setDTthreads(threads = 1)
require(fst)
require(minpack.lm)
require(lubridate)
require(log4r)
require(arrow)
# require(magrittr)

paths <- new.env()
paths$src <- here("R")

source(here(paths$src, "/TBVx-age-related-fncs-v14.R"), T)
source(here(paths$src, "/TBVx-matrix-fncs-v12.R"), T)
source(here(paths$src, "/TBVx-data-reading-fncs-v13.R"), T)
source(here(paths$src, "/TBVx-parsing-xml-fncs-v13.R"), T)
source(here(paths$src, "/TBVx-initialization-fncs-v20.R"), T)
source(here(paths$src, "/TBVx-derivs-v24.R"), T)
source(here(paths$src, "/TBVx-output-fncs-v18.R"), T)
source(here(paths$src, "/TBVx-output-query-fncs-v5.R"), T)
source(here(paths$src, "/TBVx-mod-xml-fncs-v2.R"), T)
source(here(paths$src, "/TBVx-optimization-fncs-v1.R"), T)
source(here(paths$src, "/TBVx-hi-level-fncs-v1.R"), T)


model_version_ <- "3_2_0"
modelversion <- function() {
  return(model_version_)
}
schema_version_ <- "TB-Vx-schema-S.xsd"
schemaversion <- function() {
  return(schema_version_)
}
