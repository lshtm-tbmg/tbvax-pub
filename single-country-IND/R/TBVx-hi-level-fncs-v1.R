set.options=function(){
  options("warnPartialMatchDollar"=T)
}
redirect.errors=function(paths){
  sink(file(paths[["logfile"]],'wt'),append=T,type="message")
}

setup.model.log=function(paths,lglevel="DEBUG"){
  assert_that(!is.na(paths[["countries"]]),msg="paths$countries should not be NULL")
  assert_that(!is.na(paths[["country.code"]]),msg="paths$country.code should not be NULL")
  RUNID_TS = Sys.getenv("RUNID_TS")
  if (RUNID_TS == "") {
    LTS <- format.Date(Sys.time(), format = "%Y-%m-%d-%H%M", tz = "UTC")
    RUNID_TS <- sprintf("%s_%s_%s", LTS, "LOCAL", paths$country.code)
  }
  logr <- create.logger(logfile = here(paths$log.dir, paste0(RUNID_TS, "_model.log")), level = lglevel)
  return(function(level = level, msg = NULL) { levellog(logr, level = level, message = msg)})
}

update.paths = function(paths         = NULL,
                        mydir         = here(), 
                        countries     = "countries", 
                        countrycode   = NA, 
                        targets       = NA, 
                        xml           = NA, 
                        baseline      = NA, 
                        parameters    = NA, 
                        VXa.incidence.files=NA){
  assert_that(!is.null(paths),msg="paths should not be NULL ")
  assert_that(is.environment(paths),msg="paths should be an R environment")
  if (!(mydir)==here()){ paths$mydir          = mydir}
  paths$country.code  = countrycode   
  paths$xml           = xml
  paths$parameters    = parameters
  paths$targets       = targets
  paths$baseline      = baseline
  paths$VXa.incidence.files = VXa.incidence.files 
  assert_that(!is.na(paths$country.code),msg="country code should not be NA")
  assert_that(!is.na(paths$xml),msg="xml should not be NA")
  paths$countries      = countries 
  paths$country.dir    = here(mydir,countries,countrycode) 
  paths$log.dir        = here(paths$country.dir,"logs")
  paths$data.dir       = here(paths$country.dir,"data")
  paths$params.dir     = here(paths$country.dir,"parameters")
  paths$output.dir     = here(paths$country.dir,"output")
  if (!is.na(paths$xml))       { paths$xml        = here(paths$params.dir,xml)}
  if (!is.na(paths$parameters)){ paths$parameters = here(paths$params.dir,parameters)}
  if (!is.na(paths$targets))   { paths$targets    = here(paths$params.dir,targets)}
  if (!is.na(paths$baseline))  { paths$baseline   = here(paths$params.dir,baseline)}
  if (!is.na(baseline)){
    popadj.fnames=dir(paths$output.dir,pattern=paste0("^\\[",countrycode,"\\].*\\[",baseline,"\\]\\[dfrPOPadj\\]\\.txt"))
    if (length(popadj.fnames)!=1){
      modlog(level='FATAL', msg= paste0("either none or multiple matches for baseline popadj output of ",baseline))
    }
    assert_that(length(popadj.fnames)==1,msg=paste0("either none or multiple matches for baseline popadj output of ",baseline))
    paths$popadj = here::here(paths$output.dir,popadj.fnames[1])
    bgxfr.fnames=dir(paths$output.dir,pattern=paste0("^\\[",countrycode,"\\].*\\[",baseline,"\\]\\[dfrBGx\\]\\.txt"))
    if (length(bgxfr.fnames)!=1){
      modlog(level='FATAL', msg= paste0("either none or multiple matches for baseline background death fractions output of ",baseline))
    }
    assert_that(length(bgxfr.fnames)==1,msg=paste0("either none or multiple matches for baseline background death fractions output of ",baseline))
    paths$bgxfr = here::here(paths$output.dir,bgxfr.fnames[1])
  }
  paths
}

set.paths = function(mydir         = here(), 
                     countries     = "countries", 
                     countrycode   = NA, 
                     xml           = NA, 
                     parameters    = NA, 
                     targets       = NA, 
                     baseline      = NA, 
                     VXa.incidence.files=NA,
                     lglevel       = "DEBUG"){
  
  assert_that(!is.null(paths),msg="model$paths should not be NULL")
  
  paths        = update.paths(paths,mydir,countries,countrycode,targets,xml,baseline,parameters,VXa.incidence.files)
  modlog       <<- setup.model.log(paths,lglevel)
  paths
}

modify.input.csv = function(input.csv=NULL,new.parameter.values=NULL){
  assert_that(!is.null(input.csv),msg="input.csv argument should not be NULL in modify.input.csv()")  
  assert_that(!is.null(new.parameter.values),msg="new.parameter.values argument should not be NULL in modify.input.csv()")  
  constant   = constant.parameters(parameters.df=input.csv)
  err = any(constant$unique.name %in% names(new.parameter.values))
  assert_that(!err,msg="new.parameter.values should not contain parameter marked as constant")  
  if (err) {modlog(level="ERROR",msg="new.parameter.values should not contain parameter marked as constant")}
  sel = input.csv$unique.name %in% names(new.parameter.values)
  n   = nrow(input.csv)
  if (n>0){
   for (i in 1:nrow(input.csv[sel,])){
    input.csv[sel,][i,]$mean=new.parameter.values[input.csv[sel,][i,]$unique.name]
   }
  }
  return(input.csv)
}

selected.parameters = function(parameters.df=NULL, constant=NA){
  fncname = match.call()[[1]]
  assert_that(!is.null(parameters.df),msg=paste("parameters data frame should not be NULL in",fncname))
  assert_that(!is.na(constant),msg=paste("constant is a required argument [T/F] in",fncname))
  const            = grepl("^con.*", parameters.df$dist) | !parameters.df$choose
  if (constant){
    return(parameters.df[const,])
  }else{
    return(parameters.df[!const,])
  }  
}
  
constant.parameters = function(parameters.df=NULL){
  return(selected.parameters(parameters.df,constant=T))
}
fitted.parameters = function(parameters.df=NULL){
  return(selected.parameters(parameters.df,constant=F))
}

sample.fitted.parameters = function(selected.parameters=NULL){
  fncname = match.call()[[1]]
  assert_that(!is.null(selected.parameters),msg=paste("selected.parameters should not be NULL in",fncname))
  constant            = grepl("^con.*", selected.parameters$dist) | !selected.parameters$choose
  varparams           = selected.parameters[!constant,]
  varparams$mean      = as.numeric(varparams$mean)
  varparams$min       = as.numeric(varparams$min)
  varparams$max       = as.numeric(varparams$max)
  newvalues           = runif(nrow(varparams),varparams$min,varparams$max)
  names(newvalues)    = varparams$unique.name
  newvalues
}

get.target.hits = function(output=NULL,model.params=NULL, exclude.intermediates=T){
  assert_that(!is.null(model.params),msg="model.params should not be NULL when evaluating targets")
  if (!is.na(model.params$paths$targets)){
    targets     = read.targets(model.params$paths$targets)
    hits        = eval.output.vs.targets(output,targets,model.params, exclude.intermediates)
    output$hits = cbind(country=model.params$run.params$countrycode,hits)
  }
  output
}

write.targets = function(output=NULL,model.params=NULL, output.format='txt'){
  if(!is.null(output$hits)){
    outfname  = paste0(create.filename(model.params$paths$output.dir, model.params, runtype = "hits"),".txt")
    write.output(output$hits,outfname,output.format)
  }
}

write.stocks.and.flows = function(output=NULL,model.params=NULL, output.format='txt'){
  fncname = match.call()[[1]]
  assert_that(!is.null(model.params$paths),msg=paste("paths should not be NULL in",fncname))
  assert_that(!is.null(output),msg=paste("output should not be NULL in",fncname))
  filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="stocks"),".txt")
  write.output(output$stocks,filename, output.format)
  #filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dead.999"),".txt")
  #write.output(output$dead.999,filename, output.format)
  #filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="count.999"),".txt")
  #write.output(output$count.999,filename, output.format)
  #filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="alive.999"),".txt")
  #write.output(output$alive.999,filename, output.format)
  #filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="alive.500"),".txt")
  #write.output(output$alive.500,filename, output.format)
  if (!is.null(output$flows)){
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="flows"),".txt")
    write.output(output$flows,filename, output.format)
  }
}

write.econ.output = function(output=NULL,model.params=NULL,output.format='txt',suffix=NA){
  
  fncname = match.call()[[1]]
  assert_that(!is.null(model.params),msg=paste("paths should not be NULL in",fncname))
  assert_that(!is.null(output),msg=paste("output should not be NULL in",fncname))
  
  if (!is.null(output$population)){
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="POP"),".txt")
    write.output(output$population,filename,output.format)
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dBGx"),".txt")
    write.output(output$dBGx,filename,output.format)
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dTBHIVx"),".txt")
    write.output(output$dHIVTBx,filename,output.format)
    if (!model.params$intervention){
      filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dPOPadj"),".txt")
      write.output(output$dPOPadj,filename,output.format)
    }
    if (!is.null(output$vaccinated)){
      filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dVx1pyr"),".txt")
      write.output(output$vaccinated,filename,output.format)
    }
  }
  if (!is.na(suffix) & !is.null(output$population)){
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="POP_NEW"),".txt")
    write.output(output$population.new,filename,output.format)
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dBGx_NEW"),".txt")
    write.output(output$dBGx.new,filename,output.format)
    filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dTBHIVx_NEW"),".txt")
    write.output(output$dHIVTBx.new,filename,output.format)
    if (!model.params$intervention){
      filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dPOPadj_NEW"),".txt")
      write.output(output$dPOPadj.new,filename,output.format)
    }
    if (!is.null(output$vaccinated)){
      filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="dVx1pyr_NEW"),".txt")
      write.output(output$vaccinated.new,filename,output.format)
    }
  }
  
}

write.updated.xml = function(parameters=NULL, xml=NULL){
  assert_that(!is.null(parameters))
  assert_that(!is.null(xml))
  write_xml(parameters$xml$doc, file = here(parameters$paths$params.dir, xml), option="as_xml")
}

merge.stocks.and.flows=function(output){
  widened.stocks = cbind(output$stocks[,1:7],dim=NA,subject=NA,flow=NA,output$stocks[,8:10])
  rbind(widened.stocks,output$flows)
}

write.merged.stocks.and.flows = function(onerun=NULL,model.params=NULL,output.format='txt'){
  fncname = match.call()[[1]]
  assert_that(!is.null(model.params$paths),msg=paste("paths should not be NULL in",fncname))
  assert_that(!is.null(onerun),msg=paste("onerun should not be NULL in",fncname))
  filename = paste0(create.filename(model.params$paths$output.dir, model.params,runtype="onerun"),".txt")
  write.output(onerun,filename,output.format)
}
