# RC 18/11/2020 changes to run.blast: line 100, 117-120: log file = csv instead of space. line 123-159: code for parameter/target output

init.model.parameters.and.targets = function(paths=NULL){
  model = new.env()
  with(model,{
    selected.parameters = read.csv(paths$country.params, stringsAsFactors = F, header=T, fileEncoding = 'UTF-8-BOM')
    constant            = grepl("^con.*",selected.parameters$dist) | !as.logical(stri_trim_both(selected.parameters$choose)) 
    targets             = read.targets(paths$country.targets)
    params              = init.parameters(paths=paths)
    params$xml$doc      = update.constant.parameters(params,selected.parameters[constant,])
  })
  model
}

init.xml.parameters.and.targets = function(paths=NULL){
  model = new.env()
  with(model,{
    targets             = read.targets(paths$country.targets)
    params              = init.parameters(paths)
  })
  model
}

create.filename = function(path=NULL, model.params=NULL, runnr=NA, runtype=NA, score=NA){
  assert_that(!is.null(path),msg="path argument to create.filename should not be NULL")
  dattim  = format(Sys.time(), "%F_%Hh%Mm%Ss")
  s = paste0(path,"/[",model.params$run.params$countrycode,"]")
  if (exists("taskenvvar") & length(Sys.getenv(taskenvvar)>0)){ 
    s = paste0(s,"[",Sys.getenv(taskenvvar),"]") 
  }
  s = paste0(s,"[",dattim,"]")
  s = paste0(s,"[",model.params$hash,"]") 
  charv = unlist(strsplit(model.params$xmlfilename,"/"))
  vlast = charv[length(charv)]
  s = paste0(s,"[",modelversion(),"][",unlist(strsplit(vlast,".xml"))[1],"]")
  if (!is.na(runnr)){ s = paste0(s,"[",runnr,"]")}
  if (!is.na(score)){ s = paste0(s,"[",score,"]")}
  if (!is.na(runtype)){s = paste0(s,"[",runtype,"]")}
  s
}

run.wave = function(opts=NULL, paths=NULL){

  targets.file        = paths$country.targets
  fitparams.file      = paths$country.params
  xml.file            = paths$country.xml
  xmlrun.file         = paths$country.xmlrun

  model               = init.xml.parameters.and.targets(targets.file,xml.file,xmlrun.file,opts,paths)
  model$fitted.params = read.csv(fitparams.file,stringsAsFactors = F, header=T, fileEncoding = 'UTF-8-BOM')

  with(model,{
    my_layout=function(level, ...) {
      msg <- paste0("taskID=",Sys.getenv(opts$taskenvvar)," runnr=",runnr,...)
      sprintf("%s\n", msg)
    }
    timestamp     = as.character(as.integer(seconds(now())))
    logfile       = paste0(paths$country.output.dir,"/",unlist(strsplit(opts$xmlinput,".xml"))[1],"-wave-",timestamp,Sys.getenv(opts$taskenvvar),".log")
    log           = file_appender(file=logfile, append = TRUE, layout = my_layout)
    
    from.col = 1+which(names(fitted.params)=="choose")
    thru.col = ncol(fitted.params)
    assert_that(thru.col>=from.col,msg="no additional columns with values in parameters file ?")
    
    for (i in from.col:thru.col){
      
      runnr                   <<- i
      new.param.values        = fitted.params[,i]
      names(new.param.values) = fitted.params$unique.name
      params$xml$doc          = update.fitted.parameters(params,fitted.params,new.param.values)
      temp.params             = init.parameters(params$xmlfile,params$xmlrunfile,params)
      output                  = run.model(temp.params)
      out                     = eval.output.vs.targets(output,targets,p=temp.params)
      # md5mdl                  = digest(params,algo="sha1")
      log(                msg = paste0(" nHITS=",signif(sum(out$fit)),
                                       " SSwR=",signif(sum(out$weighted_residuals^2)),
                                       " ",format(Sys.time(), "%F_%Hh%Mm%Ss"), " ",
                                       paste0(names(new.param.values),"=",signif(new.param.values),collapse=" ")))
    fname=create.filename(path=paths$country.output.dir,params=params,runnr=runnr,runtype=paste0("_col_",i,"_wave"))
    write.table(out,paste0(fname,".txt"), quote=F,sep="\t",row.names=F)
    }
  })
}



run.blast = function(paths=NULL,opts=NULL){
  
  targets.file   = paths$country.targets
  fitparams.file = paths$country.params
  xml.file       = paths$country.xml
  xmlrun.file    = paths$country.xmlrun
  
  model          = init.model.parameters.and.targets(paths=paths)
  
  with(model,{
    my_layout=function(level, ...) {
      msg <- paste0("taskID=",Sys.getenv(opts$taskenvvar),",runnr=",runnr,...)
      sprintf("%s\n", msg)
    }
    # datetime                = format(Sys.time(), "%F_%Hh%Mm%Ss")
    # timestamp     = as.character(as.integer(seconds(now())))
    logfilename   = create.filename(paths$country.log.dir,params=params, runtype="blast")
    log           = file_appender(file=paste0(logfilename,".log"), append = TRUE, layout = my_layout)
    
    fitted.params = selected.parameters[!constant,]
    for (i in 1:opts$nruns){
      runnr                   <<- i
      new.param.values        = runif(nrow(fitted.params),fitted.params$min,fitted.params$max)
      names(new.param.values) = fitted.params$unique.name
      params$xml$doc          = update.fitted.parameters(params,fitted.params,new.param.values)
      temp.params             = init.parameters(paths=paths,params = params)
      
      output                  = run.model(temp.params)
      out                     = eval.output.vs.targets(output,targets,p=temp.params)
      log(                msg = paste0(",nHITS=",signif(sum(out$fit)),
                                       ",SSwR=",signif(sum(out$weighted_residuals^2)),",ts=",format(Sys.time(), "%F_%Hh%Mm%Ss"), ",",
                                       paste0(names(new.param.values),"=",signif(new.param.values),collapse=","), ",",
                                       paste0(selected.parameters[constant,]$unique.name,"=",signif(selected.parameters[constant,]$mean),collapse=",")))
      score = sum(out$fit)/length(out$fit)
      
      local(#RC 18112020 included for generating target and parameter output
        {
          tid <- if (Sys.getenv(opts$taskenvvar) != "") {Sys.getenv(opts$taskenvvar)} else {"TNF"}
          param_hash <- digest::digest(params)
          parameter_row <- c(
            hash = param_hash,
            ID = tid,
            timestamp = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
            nHITs = signif(sum(out$fit)),
            SSwR = signif(sum(out$weighted_residuals^2)),
            as.list(signif(new.param.values)),
            setNames(as.list(selected.parameters[constant, ]$mean), nm = selected.parameters[constant, ]$unique.name)
            )
          
          target_output <- as.data.table(out)
          target_output[, hash := param_hash]
          target_output[, tid := tid]

          if (!dir.exists(file.path(paths$country.output.dir, "extra_params"))) {dir.create(file.path(paths$country.output.dir, "extra_params"), recursive = T)}
          if (!dir.exists(file.path(paths$country.output.dir, "extra_targets"))) {dir.create(file.path(paths$country.output.dir, "extra_targets"), recursive = T)}
          
          params_file_name <- file.path(paths$country.output.dir, "extra_params", sprintf("[%s]-[parameters]-[blast].csv", tid))
          target_file_name <- file.path(paths$country.output.dir, "extra_targets", sprintf("[%s]-[targets]-[blast].csv", tid))

          if (file.exists(params_file_name)) {
            fwrite(as.data.table(parameter_row), append = T, file = params_file_name, na = "NA")
          } else {
            fwrite(as.data.table(parameter_row), file = params_file_name, na = "NA")
          }

          if (file.exists(target_file_name)) {
            fwrite(as.data.table(target_output), append = T, file = target_file_name, na = "NA")
          } else {
            fwrite(as.data.table(target_output), file = target_file_name, na = "NA")
          }
        }
      )
      if (is.na(score)){
        logger(level="ERROR",msg="score NA !?")
      }else if (score>=opts$minhitsfrac){ #RC: pause output of files
       #  fname=create.filename(opts=opts,runnr=runnr,runtype="blast",score=round(100*score,0))
       #  write.table(out,paste0(fname,".txt"), quote=F,sep="\t",row.names=F)
       #  write_xml(temp.params$xml$doc, file = paste0(fname,".xml"), option="as_xml")
       #  selected.parameters[!constant,]$mean = new.param.values
       #  write.csv(selected.parameters,file=paste0(fname,"_params.csv"),quote=F,row.names=F)
       #  write.table(output$z,file = paste0(fname,"_full.txt"), quote=F,sep="\t",row.names=F)
      }
    }
  })
}

run.optimization = function(targets.file,fitparams.file,xml.file,xmlrun.file,opts,lmcontrol,flatmid=0){
  
  model         = init.model.parameters.and.targets(targets.file,fitparams.file,xml.file,xmlrun.file,opts)
  
  model$levmarqcontrol = NULL
  if (!is.null(lmcontrol)){
    df = read.csv(lmcontrol,fileEncoding = "UTF-8-BOM")
    ctrl = new.env()
    eval(parse(text=paste(df$parameter,"=",df$value)),envir = ctrl)
    model$levmarqcontrol = as.list(ctrl)
  }
  
  with(model,{
    
    my_layout=function(level, ...) {
      msg <- paste0("taskID=",Sys.getenv(opts$taskenvvar)," runnr=",runnr,...)
      sprintf("%s\n", msg)
    }
    logfilename   = create.filename(path=paths$country.log.dir,params=params,runtype="optim")
    log           = file_appender(file=paste0(logfilename,".log"), append = TRUE, layout = my_layout)
    fitted.params = selected.parameters[!constant,]

    for (i in 1:opts$nruns){
      runnr                   <<- i
      if (i==1){
        new.param.values      = fitted.params$mean # i.e. we start at mean values
      }else{
        new.param.values      = runif(nrow(fitted.params),fitted.params$min,fitted.params$max) # i.e. we start at random values
      }
      names(new.param.values) = fitted.params$unique.name
      params$xml$doc          = update.fitted.parameters(params,fitted.params,new.param.values)
      temp.params             = init.parameters(params$xmlfile,params$xmlrunfile,params)

      if (!is.null(levmarqcontrol)){
        control = levmarqcontrol
        control$diag = levmarqcontrol$diag/model$fitted.params$mean
      }else{
        control = NULL
      }
      result                  = optimize(fn=fit.model, model=model, control=control, logfn=log, flatmid=flatmid)
      names(result$par)       = fitted.params$unique.name
      params$xml$doc          = update.fitted.parameters(params,fitted.params,result$par)
      temp.params             = init.parameters(params$xmlfile,params$xmlrunfile,params)
      output                  = run.model(temp.params)
      out                     = eval.output.vs.targets(output,targets,p=temp.params,flatmid=flatmid)
      # md5mdl                  = digest(params,algo="sha1")
      log(                msg = paste0(" nHITS=",signif(sum(out$fit)),
                                       " SSwR=",signif(sum(out$weighted_residuals^2)),
                                       " ",format(Sys.time(), "%F_%Hh%Mm%Ss"), " ",
                                       paste0(names(result$par),"=",signif(result$par),collapse=" "),
                                       paste0(selected.parameters[constant,]$unique.name,"=",signif(selected.parameters[constant,]$mean),collapse=" ")))
      score = sum(out$fit)/length(out$fit)
      if (is.na(score)){
        logger(level="ERROR",msg="score NA !?")
      }else if (score>=opts$minhitsfrac){
        fname=create.filename(paths,params=temp.params,runnr=runnr,runtype="optim",score=round(100*score,0))
        write.table(out,paste0(fname,".txt"), quote=F,sep="\t",row.names=F)
        write_xml(temp.params$xml$doc, file = paste0(fname,".xml"), option="as_xml")
        selected.parameters[!constant,]$mean = result$par
        write.csv(selected.parameters,file=paste0(fname,"_params.csv"),quote=F,row.names=F)
        write.table(output$z,file = paste0(fname,"_full.txt"), quote=F,sep="\t",row.names=F)
      }
    }
  })
}

optimize = function(fn=NULL, model=NA, control = NA, logfn=NULL, flatmid=0){
  nls.lm(par=model$new.param.values,lower=model$fitted.params$min, upper=model$fitted.params$max,
         fn=fn,control=control,fitted.params=model$fitted.params, fparams=model$temp.params,
         targets=model$targets, logfn=logfn, flatmid=flatmid)
  #modFit(f=fn,p=model$new.param.values,fitted.params=model$fitted.params, fparams=model$temp.params, 
  #       targets=model$targets, logfn=logfn, lower=model$fitted.params$min, upper=model$fitted.params$max, 
  #       method="Marq",control=control, hessian=F)
}

fit.model = function(param.values=NA, fitted.params=NA, fparams=NA, targets=NA, logfn=NULL, flatmid=0){
  cat(param.values,"\n")
  fparams$xml$doc = update.fitted.parameters(fparams,fitted.params,param.values)
  params = init.parameters(fparams$xmlfile,fparams$xmlrunfile, fparams)
  output = run.model(params)
  fit    = eval.output.vs.targets(output,targets,p=params, flatmid=flatmid)
  # print(fit$weighted_residuals)
  #result = data.frame(name=fit$name,year=fit$year,w_resid=fit$weighted_residuals,data=fit$value,model=fit$model,rss=fit$weighted_residuals^2)
  #rez = result[order(result$w_resid^2),]
  #RSS=sum(rez$w_resid^2)
  #print(rez)
  print(sum(fit$weighted_residuals^2))
  logfn(msg=paste0(" SSwR=",signif(sum(fit$weighted_residuals^2),4)," ",format(Sys.time(), "%F_%Hh%Mm%Ss"), " ",paste0(names(param.values),"=",signif(param.values,4),collapse=" ")))
  return(fit$weighted_residuals)
}
# NOTE: though fit.model returns residuals, only Marq uses these. Nelder Mead takes the TSS

