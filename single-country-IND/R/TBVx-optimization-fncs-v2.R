fit.model = function(param.values=NULL, fitted.params=NULL, xmlparams=NULL, targets=NULL, logfn=NULL, mdl=NULL, pwr=NULL){
  cat(param.values,"\n")
  updated.fitted.params = mdl$modify.input.csv(fitted.params,param.values)
  xmlparams$xml$doc = mdl$update.parameters(xmlparams$xml$doc,updated.fitted.params)
  initialized.params = mdl$initialize.model.parameters(xmlparams)
  output = mdl$run.model(initialized.params)
  fit    = mdl$eval.output.vs.targets(output,targets,p=initialized.params)
  print(sum(fit$weighted_residuals^2))
  if (is.null(pwr)){
    logfn(fit=fit, pvalues=param.values)
    return(fit$weighted_residuals)
  }else if(pwr>0){
    logfn(fit=fit, pvalues=param.values)
    sg  = sign(fit$weighted_residuals)
    y1  = (abs(fit$weighted_residuals))^pwr
    return(sg*y1)
  }else if (pwr==0){ # 0 if abs(weighted residual) < 1  
    logfn(fit=fit, pvalues=param.values)
    sg = sign(fit$weighted_residuals)
    y1  = pmax(0,abs(fit$weighted_residuals)-0.8)
    return(sg*y1)
  }else{ # penalty when outside range
    logfn(fit=fit, pvalues=param.values)
    sg = sign(fit$weighted_residuals)
    y1  = abs(fit$weighted_residuals) + (!fit$fit)*(-pwr)^2
    return(sg*y1)
  }
}
# NOTE: though fit.model returns residuals, only Marq uses these. Nelder Mead takes the TSS

create.logr = function(model=NULL,params=NULL){
  my_layout=function(level=NULL, fit=NULL, pvalues=NULL, header=F) {
    if (header){
      msg = paste("taskID","runnr","nHITS","of","RESID","SSwR","datetime",paste0(names(pvalues),collapse="\t"),sep="\t")
    }else{
      msg <- paste(Sys.getenv(taskenvvar),runnr,sum(fit$fit),length(fit$fit),
                   signif(sum(abs(fit$weighted_residuals)),4),signif(sum(fit$weighted_residuals^2),4),
                   format(Sys.time(), "%F_%Hh%Mm%Ss"),
                   paste0(formatC(pvalues,digits=5,format="f"),collapse="\t"),
                   sep="\t")
    }
    sprintf("%s\n", msg)
  }
  logfilename   = model$create.filename(path=model$paths$log.dir,model.params=params,runtype="optim")
  logr          = file_appender(file=paste0(logfilename,".log"), append = TRUE, layout = my_layout)
  logr
}

setup.lmcontrol = function(fitted.params=NULL,lmcontrolcsv=NULL){
  if (!is.null(lmcontrolcsv)){
    df = read.csv(lmcontrolcsv,fileEncoding = "UTF-8-BOM")
    ctrl = new.env()
    eval(parse(text=paste(df$parameter,"=",df$value)),envir = ctrl)
    control = as.list(ctrl)
    assert_that(!is.null(fitted.params),msg="in setup.lmcontrol(): fitted.params should not be NULL when lmcontrol csv is not NULL")
    control$diag = 1.0/pmax(1e-8,pmin(fitted.params$mean,abs(fitted.params$max-fitted.params$min))) # in case mean is (close to) 0
  }else{
    control = NULL
  }
  control
}

final.run.and.write.result = function(model, params, constant.params, fitted.params, logfn=NULL, result=NULL, targets=NULL){
  
  new.fitted.params  = modify.input.csv(input.csv=fitted.params,new.parameter.values=result$par)
  params$xml$doc     = model$update.parameters(params$xml$doc,new.fitted.params)
  initialized.params = model$initialize.model.parameters(params)
  output             = model$run.model(initialized.params)
  out                = model$eval.output.vs.targets(output,targets,p=initialized.params)
  
  logfn(fit=out, pvalues=result$par)
  score = sum(out$fit)/length(out$fit)
  if (is.na(score)){
    logger(level="ERROR",msg="score NA !?")
  }else{
    fname=model$create.filename(model$paths$log.dir,model.params=initialized.params,runnr=runnr,runtype="optim",score=round(100*score,0))
    write.table(out,paste0(fname,".txt"), quote=F,sep="\t",row.names=F)
    write_xml(initialized.params$xml$doc, file = paste0(fname,".xml"), option="as_xml")
    fitted.params$mean = result$par
    write.csv(rbind(constant.params,fitted.params),file=paste0(fname,"_params.csv"),quote=F,row.names=F)
  }
}

run.optimizer = function(model=NULL, new.parameter.values=NULL, constant.new.parameter.values=F, lmcontrolcsv=NULL, nruns=1, pwr=NULL){

  selected.parameters = read.csv(model$paths$parameters, stringsAsFactors = F, header=T, fileEncoding = 'UTF-8-BOM')
  constant            = grepl("^con.*",selected.parameters$dist) | !as.logical(stri_trim_both(selected.parameters$choose)) 
  if (!is.null(new.parameter.values)){
    if (constant.new.parameter.values){
      selected.parameters = modify.input.csv(input.csv = selected.parameters, new.parameter.values = new.parameter.values)
      constant = constant | (selected.parameters$unique.name %in% names(new.parameter.values))
    }else{
      subsel = !(selected.parameters[!constant,]$unique.name %in% names(new.parameter.values))
      values = selected.parameters[!constant,][subsel,]$mean
      names  = selected.parameters[!constant,][subsel,]$unique.name
      oldnames = names(new.parameter.values)
      new.parameter.values = c(new.parameter.values,values)
      names(new.parameter.values)=c(oldnames,names)
    }
  }
  constant.params    = selected.parameters[constant,]
  
  fitted.params      = selected.parameters[!constant,]
  fitted.params      = fitted.params[order(fitted.params$unique.name),]
  assert_that(!any(fitted.params$min > fitted.params$mean),msg="min of fitted parameters > mean ???")
  assert_that(!any(fitted.params$max < fitted.params$mean),msg="max of fitted parameters < mean ???")

  targets             = model$read.targets(model$paths$targets)
  params              = model$read.model.parameters(model$paths)
  params$xml$doc      = model$update.parameters(params$xml$doc,constant.params)
  
  logr               = create.logr(model,params)  
  
  control            = setup.lmcontrol(fitted.params, lmcontrolcsv)

  if (!is.null(new.parameter.values)){
    assert_that(nruns<=1,msg="nruns should not be > 1 when new.parameter.values is not NULL")
    if (constant.new.parameter.values){
      new.params = fitted.params$mean
      names(new.params)=fitted.params$unique.name
    }else{
      assert_that(all(names(new.parameter.values) %in% fitted.params$unique.name),msg="new parameters do not match unique names of fitted parameters")
      new.params       = new.parameter.values[order(names(new.parameter.values))] # [order(ord.fitted)]
    }
    runnr            <<- 1
    logr(pvalues=new.params, header=T)
    result           = nls.lm(par             = new.params, 
                              lower           = fitted.params$min[fitted.params$unique.name %in% names(new.params)], 
                              upper           = fitted.params$max[fitted.params$unique.name %in% names(new.params)], 
                              fn              = fit.model, 
                              control         = control, 
                              fitted.params   = fitted.params, 
                              xmlparams       = params, 
                              targets         = targets, 
                              logfn           = logr, 
                              mdl             = model, 
                              pwr             = pwr)
    final.run.and.write.result(model, params, constant.params, fitted.params, logr, result, targets)
  }else{
    for (i in 1:nruns){
      new.params        = runif(nrow(fitted.params),fitted.params$min,fitted.params$max) # i.e. we start at random values between min and max
      names(new.params) = fitted.params$unique.name
      runnr             <<- i
      logr(pvalues=new.params, header=T)
      result            = nls.lm(par             = new.params, 
                                 lower           = fitted.params$min, 
                                 upper           = fitted.params$max, 
                                 fn              = fit.model, 
                                 control         = control, 
                                 fitted.params   = fitted.params, 
                                 xmlparams       = params, 
                                 targets         = targets, 
                                 logfn           = logr, 
                                 mdl             = model, 
                                 pwr             = pwr)
      final.run.and.write.result(model, params, constant.params, fitted.params, logr, result, targets)
    }
  }
}  



