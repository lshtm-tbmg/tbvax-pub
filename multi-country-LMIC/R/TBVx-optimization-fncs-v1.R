run.optimization = function(model=NULL, lmcontrol, nruns=1, pwr=NULL){
  
  selected.parameters = read.csv(model$paths$parameters, stringsAsFactors = F, header=T, fileEncoding = 'UTF-8-BOM')
  constant            = grepl("^con.*",selected.parameters$dist) | !as.logical(stri_trim_both(selected.parameters$choose)) 
  targets             = model$read.targets(model$paths$targets)
  params              = model$read.model.parameters(model$paths)
  params$xml$doc      = model$update.constant.parameters(params,selected.parameters[constant,])

  levmarqcontrol = NULL
  if (!is.null(lmcontrol)){
    df = read.csv(lmcontrol,fileEncoding = "UTF-8-BOM")
    ctrl = new.env()
    eval(parse(text=paste(df$parameter,"=",df$value)),envir = ctrl)
    levmarqcontrol = as.list(ctrl)
  }
  
  my_layout=function(level, ...) {
    msg <- paste0("taskID=",Sys.getenv(taskenvvar)," runnr=",runnr,...)
    sprintf("%s\n", msg)
  }
  logfilename   = model$create.filename(path=model$paths$log.dir,model.params=params,runtype="optim")
  log           = file_appender(file=paste0(logfilename,".log"), append = TRUE, layout = my_layout)
  fitted.params = selected.parameters[!constant,]
  
  assert_that(!any(fitted.params$min > fitted.params$mean),msg="min of fitted parameters > mean ???")
  assert_that(!any(fitted.params$max < fitted.params$mean),msg="max of fitted parameters < mean ???")

  for (i in 1:nruns){
    runnr                   <<- i
    if (i==1){
      new.param.values      = fitted.params$mean # i.e. we start at mean values
    }else{
      lo = fitted.params$mean + extent * (fitted.params$min - fitted.params$mean)
      hi = fitted.params$mean + extent * (fitted.params$max - fitted.params$mean)
      new.param.values      = runif(nrow(fitted.params),lo,hi) # i.e. we start at random values between mean +/- 0.4 min/max range
    }
    names(new.param.values) = fitted.params$unique.name
    params$xml$doc          = model$update.fitted.parameters(params,fitted.params,new.param.values)
    temp.params             = model$initialize.model.parameters(params)

    if (!is.null(levmarqcontrol)){
      control = levmarqcontrol
      control$diag = 1.0/pmax(1e-8,pmin(fitted.params$mean,abs(fitted.params$max-fitted.params$min))) # in case mean is (close to) 0
    }else{
      control = NULL
    }

    result = nls.lm(par            = new.param.values,
                    lower          = fitted.params$min, 
                    upper          = fitted.params$max,
                    fn             = fit.model,
                    control        = control,
                    fitted.params  = fitted.params, 
                    xmlparams      = temp.params,
                    targets        = targets, 
                    logfn          = log,
                    mdl            = model,
                    pwr            = pwr)
    
    names(result$par) = fitted.params$unique.name
    params$xml$doc    = model$update.fitted.parameters(params,fitted.params,result$par)
    temp.params       = model$initialize.model.parameters(params)
    output            = model$run.model(temp.params)
    out               = model$eval.output.vs.targets(output,targets,p=temp.params)
    # md5mdl            = digest(params,algo="sha1")
    log(msg = paste0("nHITS=",signif(sum(out$fit)),
                     "\tSSwR=",signif(sum(out$weighted_residuals^2)),
                     "\t",format(Sys.time(), "%F_%Hh%Mm%Ss"), " ",
                     paste0(names(result$par),"=",signif(result$par),collapse="\t"),
                     paste0(selected.parameters[constant,]$unique.name,"=",signif(selected.parameters[constant,]$mean),collapse="\t")))
    score = sum(out$fit)/length(out$fit)
    if (is.na(score)){
      logger(level="ERROR",msg="score NA !?")
    }else{
        fname=model$create.filename(model$paths$log.dir,model.params=temp.params,runnr=runnr,runtype="optim",score=round(100*score,0))
        write.table(out,paste0(fname,".txt"), quote=F,sep="\t",row.names=F)
        write_xml(temp.params$xml$doc, file = paste0(fname,".xml"), option="as_xml")
        selected.parameters[!constant,]$mean = result$par
        write.csv(selected.parameters,file=paste0(fname,"_params.csv"),quote=F,row.names=F)
        write.table(output$z,file = paste0(fname,"_full.txt"), quote=F,sep="\t",row.names=F)
    }
  }
}

fit.model = function(param.values=NULL, fitted.params=NULL, xmlparams=NULL, targets=NULL, logfn=NULL, mdl=NULL, pwr=NULL){
  cat(param.values,"\n")
  xmlparams$xml$doc = mdl$update.fitted.parameters(xmlparams,fitted.params,param.values)
  initialized.params = mdl$initialize.model.parameters(xmlparams)
  output = mdl$run.model(initialized.params)
  fit    = mdl$eval.output.vs.targets(output,targets,p=initialized.params)
  print(sum(fit$weighted_residuals^2))
  #logfn(msg=paste0("\tnHITS=",sum(fit$fit),"/",length(fit$fit),"\tSAbsR=",signif(sum(abs(fit$residuals)),4),"\tSSwR=",signif(sum(fit$weighted_residuals^2),4),"\t",format(Sys.time(), "%F_%Hh%Mm%Ss"), "\t",paste0(names(param.values),"=",formatC(param.values,digits=5,format="f"),collapse="\t")))
  if (is.null(pwr)){
    y1 = fit$weighted_residuals
    logfn(msg=paste0("\tnHITS=",sum(fit$fit),"/",length(fit$fit),"\tRESID=",signif(sum(abs(y1)),4),"\tSSwR=",signif(sum(fit$weighted_residuals^2),4),"\t",format(Sys.time(), "%F_%Hh%Mm%Ss"), "\t",paste0(names(param.values),"=",formatC(param.values,digits=5,format="f"),collapse="\t")))
    return(y1)
  }else if(pwr>0){
    sg  = sign(fit$weighted_residuals)
    y1  = abs(fit$weighted_residuals)^pwr
    logfn(msg=paste0("\tnHITS=",sum(fit$fit),"/",length(fit$fit),"\tRESID=",signif(sum(y1),4),"\tSSwR=",signif(sum(fit$weighted_residuals^2),4),"\t",format(Sys.time(), "%F_%Hh%Mm%Ss"), "\t",paste0(names(param.values),"=",formatC(param.values,digits=5,format="f"),collapse="\t")))
    return(sg*y1)
  }else if (pwr==0){ # 0 if abs(weighted residual) < 1  
    sg = sign(fit$weighted_residuals)
    y1  = pmax(0,abs(fit$weighted_residuals)-0.8)
    logfn(msg=paste0("\tnHITS=",sum(fit$fit),"/",length(fit$fit),"\tRESID=",signif(sum(y1),4),"\tSSwR=",signif(sum(fit$weighted_residuals^2),4),"\t",format(Sys.time(), "%F_%Hh%Mm%Ss"), "\t",paste0(names(param.values),"=",formatC(param.values,digits=5,format="f"),collapse="\t")))
    return(sg*y1)
  }else{ # penalty when outside range
    sg = sign(fit$weighted_residuals)
    y1  = abs(fit$weighted_residuals) + (!fit$fit)*(-pwr)^2
    logfn(msg=paste0("\tnHITS=",sum(fit$fit),"/",length(fit$fit),"\tRESID=",signif(sum(y1),4),"\tSSwR=",signif(sum(fit$weighted_residuals^2),4),"\t",format(Sys.time(), "%F_%Hh%Mm%Ss"), "\t",paste0(names(param.values),"=",formatC(param.values,digits=5,format="f"),collapse="\t")))
    return(sg*y1)
  }
}

# NOTE: though fit.model returns residuals, only Marq uses these. Nelder Mead takes the TSS
