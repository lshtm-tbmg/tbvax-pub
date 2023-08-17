add.vaccination.results = function(t,p,dvacc,rownamesZ,subject="VXa"){
  ages.from = p$run.params$output$age.from
  M = aggregate.by.age.groups(dvacc,ages.from, sumcols=F, avg = F)
  colnames(M)=paste0("A",colnames(M))
  VXa  = calc.names.for.dim(p,"VXa")
  SES  = calc.names.for.dim(p,"SES")
  RISK = calc.names.for.dim(p,"RISK")
  HIV  = calc.names.for.dim(p,"HIV")
  TB   = calc.names.for.dim(p,"TB")
  assert_that(all(paste(VXa,SES,RISK,HIV,TB,sep="-") == rownamesZ),msg="problem in setting up df for vx reporting: rownames not correct")
  Min   = M ; Min[M<0]=0. ; Mout = M ; Mout[M>0]=0.
  dfin  = cbind(year=t,VXa=VXa,SES=SES,RISK=RISK,HIV=HIV,TB=TB,dim="VXa",subject=subject,flow="in" ,as.data.frame(Min) )
  dfout = cbind(year=t,VXa=VXa,SES=SES,RISK=RISK,HIV=HIV,TB=TB,dim="VXa",subject=subject,flow="out",as.data.frame(Mout))
  result = rbind(dfin,dfout)
  if (is.null(p$vaccinated)){
    return(result)    
  }else{
    return(rbind(p$vaccinated,result))
  }
}

aging.and.births.and.update.contact.matrix.event <- function(t,X,p){

  if (t>p$run.params$simulation$years[1]){
    events.at = unique(trunc(p$run.params$simulation$years))[-1]
    if (t==events.at[1] & !p$intervention){
      p$popadj = data.frame(matrix(0,length(events.at),p$nAGES))
      colnames(p$popadj)=p$AGES
      rownames(p$popadj)=events.at
    }
    if (t==events.at[1] & p$intervention & length(p$inci$VXa)>0){
      p$vaccinated = NULL
    }
    Y = Y.matrix.from.X.vector(X,p)
    if (t>=p$run.params$rescale.pop.yr & !p$run.params$rescaled.pop){
      Y = scale.alive.population(Y,p)
      p$run.params$rescaled.pop=T
    }
    # 1. age population (see Schenzle et al.) ; we use deltaY to not confuse with a rate (dY)
    Y[!p$ALIVE,]       = 0. 
    deltaY             = t(tcrossprod(p$aging.matrix, Y))
    deltaY[!p$ALIVE,]  = 0. # the dead do not age
    deltaY[,1]         = deltaY[,1] + p$birthrate(t)*sum(colSums(Y))*p$at.birth 
    Y                  = Y + deltaY
    # 2. update the population to match UNPOP
    if (!p$intervention){
      unpop    = p$unpopdem[p$unpopdemfn(t),]
      popbyage = colSums(Y)
      zeros  = popbyage==0
      if (any(zeros)){
        modlog(level='ERROR',paste0(" at t = ",t," empty age groups in population:",colnames(Y)[zeros]))
        assert_that(!any(zeros),msg="zeros in popbyage")
      }
      p$popadj[as.integer(rownames(p$popadj))==t,] = unpop/popbyage
      Z = t(unpop/popbyage * t(Y)) 
    }else{
      assert_that(p$intervention.start<2200,msg=paste("intervention start =",p$intervention.start,"; is that intended?"))
      if (t<p$intervention.start){
        unpop    = p$unpopdem[p$unpopdemfn(t),]
        popbyage = colSums(Y)
        zeros  = popbyage==0
        if (any(zeros)){
          modlog(level='ERROR',paste0(" at t = ",t," empty age groups in population:",colnames(Y)[zeros]))
          assert_that(!any(zeros),msg="zeros in popbyage")
        }
        Z = t(unpop/popbyage * t(Y)) 
      }else{ # i.e. from intervention start year
        Z = t((p$popadjrateintv[p$popadjfnintv(t),]) * t(Y))
      }
    }
    deltaZ = 0 * Z
    offset = 1e-4 # should be copied from some other location ....
    for (i in seq_along(p$inci$HIV)){
      if (p$inci$HIV[[i]]$inci.once.per.year){
        deltaZ = derivs.inci(t+offset,Z,p,p$inci$HIV[[i]],T)
      }
    }
    Z = Z + deltaZ
    for (i in seq_along(p$inci$TB)){
      if (p$inci$TB[[i]]$inci.once.per.year){
        deltaZ = derivs.inci(t+offset,Z,p,p$inci$TB[[i]],T)
      }
    }
    Z = Z + deltaZ
    for (i in seq_along(p$inci$VXa)){
      if (p$inci$VXa[[i]]$inci.once.per.year & t>=p$intervention.start & p$inci$VXa[[i]]$inci.fn(t+offset)>0){
        dvacc  = derivs.inci(t+offset,Z,p,p$inci$VXa[[i]],T)
        colnames(dvacc)=p$AGES
        p$vaccinated = add.vaccination.results(t,p,dvacc,rownames(Z),subject=paste0("VXaXi_",i))
        deltaZ = dvacc
      }
    }
    Z = Z + deltaZ

    if (p$contacts$defined){
      p$contacts$M=update.contact.matrix(p, colSums(Z))
    }

    if (sum(is.nan(Z))>0){
      modlog(level='FATAL',msg=paste0(" at t = ",t," NaNs in state variables ; reduce euler / rk2 / rk4 time step"))
    }else if (sum(Z<0.)>0){
      neg = p$run.params$num.int$min.value.for.state.var
      Z[Z<0. & Z>=neg] = 0.
      if (sum(Z<neg)>0){
        idx = which(Z<neg)
        r   = idx %% nrow(Z)
        c   = (idx %/% nrow(Z))+1 
        for (i in seq_along(idx)){
          modlog(level='FATAL',paste0(" at t = ",t," state variables < ",neg," :",
          " state = ",rownames(Z)[r[i]]," ; age group = ",colnames(Z)[c[i]],"; value = ",signif(Z[r[i],c[i]])," ; please reconsider parameter values, incidence data or setting min.value.for.state.var to a more negative value"))
        }
        if (!(is.null(p$paths$parameters) | is.na(p$paths$parameters))){
          if (!dir.exists(file.path(p$paths$output.dir,"weird"))){
            dir.create(file.path(p$paths$output.dir, "weird"))
          }
          outfname  = paste0(create.filename(paste0(p$paths$output.dir,"/weird"), p, runtype = "weird"),".csv")
          df = data.frame(unique.name = model$modified.params$unique.name, value=unlist(model$modified.params$mean))
          write.csv(df,outfname,row.names = F)
        }
        assert_that(sum(Z<neg)==0,msg = paste("state variables <",neg))
        return(0*X)
      }
    }
    return(as.vector(Z))
  }else{
    return(X)
  }
}

derivs.Tr=function(t,Y,Tr,fn){
  tparams = Tr$timed.parameters
  if (is.null(tparams)){
    return(fn(Tr,Y))
  }else{
    mult=1.0
    nms = names(tparams)
    for (nm in nms)
      mult = mult * get(nm,envir=tparams)(t)
    return(fn(Tr,Y,mult))
  }
}

foi=function(t,Y,parms){
  tparams = parms$Tm$timed.parameters
  if (!is.null(tparams)){
    mult = get(names(tparams),envir=tparams)(t)
  }else{
    mult = 1
  }
  I_ = matmul.by.age.group(parms$Infc, Y)
  i_ = colSums(I_)/colSums(Y[parms$ALIVE,])
  i_[is.nan(i_) | is.infinite(i_)]=0.
  if (parms$contacts$defined){
    return(parms$DAYSPERYEAR * mult * as.numeric(parms$contacts$M %*% i_)) # return the foi ; foi is a column vector of foi by age 
    #return(as.numeric(mult * parms$DAYSPERYEAR * (parms$contacts$M %*% i_ ))) # return the foi ; foi is a column vector of foi by age 
    # NOTE: this may seem incorrect as originally the contactees are in the columns of the contact matrix read
    #       however adjusting the contact matrix takes care of this ... the new contact matrix M depends on M plus t(M) anyway
    #       and the adjustment takes care of balancing i.e. the number of individuals in a row age group (i.e. susc) multiplied with an
    #       off-diagonal contact rate and the number of individuals in that column balances that same number mirrored wrt the diagonal
    #       effectively, the rows nown the contactees (susceptibles) and therefore the foi is calculated correctly
  }else{
    avg.i_  = sum(I_)/sum(Y[parms$ALIVE,]) # i.e. the average fraction infectious
    avg.i_[is.nan(avg.i_) | is.infinite(avg.i_)]=0.
    i_      = rep(avg.i_,ncol(Y))
    return(parms$DAYSPERYEAR * mult * as.numeric(i_)) # return the foi ; foi is a column vector with identical average fraction infectious times mult and DAYSPERYEAR i.e. contact rate = 1 / day
  }
} 
derivs.Tm.in.out=function(t,Y,parms){
  susc   = matmul.by.age.group.in.out(parms$Tm, Y)
  foi    = foi(t=t,Y=Y,parms=parms)
  dY.in  = t(t(susc$dY.in ) * foi )
  dY.out = t(t(susc$dY.out) * foi )
  return( list(dY=(dY.in+dY.out),dY.in=dY.in,dY.out=dY.out) )
}
derivs.Tm=function(t,Y,parms){
  susc = matmul.by.age.group(parms$Tm, Y)
  foi  = foi(t=t,Y=Y,parms=parms)
  return(t ( t(susc) * foi ) )
}

derivs.inci.common=function(t=NA,Y=NA,p=NA,p.inci=NA){ 
  dX=NULL
  x_inci = as.numeric(p.inci$inci[p.inci$inci.fn(t),])
  if (sum(x_inci)>0){
    if (!is.null(p.inci$inci.trend.fn)){
      x_inci = x_inci * p.inci$inci.trend.fn(t)
    }
    X = t(Y)
    dX = 0*X
    if (!p.inci$inci.proportions){
      if (p.inci$inci.denominator=="all"){
        denom   = rowSums(X[,p.inci$susc])
        denom   = sapply(denom, max, 1e-6)
        scaleby = rowSums(X[,p.inci$alive])/denom
        scaleby = sapply(sapply(scaleby, max, 1.),min,10)
      }else{
        scaleby = 1.0
      }
    }else{
      if (p.inci$inci.denominator=="all"){
        denom   = rowSums(X[,p.inci$alive])
      }else{
        denom   = rowSums(X[,p.inci$susc])
      }
      denom   = sapply(denom, max, 1)
      scaleby = sum(denom)/denom
      scaleby = sapply(sapply(scaleby, max, 1.),min,50)
    }
    dX[,p.inci$susc]   = x_inci * X[,p.inci$susc] * scaleby # OK - that is simple
  }
  dX
}  


derivs.inci=function(t=NA,Y=NA,p=NA,p.inci=NA,once=F){ 
  if (once==p.inci$inci.once.per.year){
    dX = derivs.inci.common(t=t,Y=Y,p=p,p.inci=p.inci)
    if (!is.null(dX) & sum(dX)>1e-30){
      #return(as.matrix(tcrossprod(p.inci$inci.matrix,dX)))
      return(matrix(tcrossprod(p.inci$inci.matrix,dX),ncol(dX),nrow(dX)))
    }
  }
  return(0.*Y)
}

derivs.inci.in.out=function(t=NA,Y=NA,p=NA,p.inci=NA,once=F){ 
  if (once==p.inci$inci.once.per.year){
    dX = derivs.inci.common(t=t,Y=Y,p=p,p.inci=p.inci)
    if (!is.null(dX)){
      P = Q = p.inci$inci.matrix
      P[p.inci$inci.matrix<0]=0.
      Q[p.inci$inci.matrix>0]=0.
      dY.in  = tcrossprod(P,dX)
      dY.out = tcrossprod(Q,dX) 
      return(list(dY=as.matrix(dY.in+dY.out),dY.in=as.matrix(dY.in),dY.out = as.matrix(dY.out)))     
    }
  }
  return(list(dY=0.*Y,dY.in=0.*Y,dY.out = 0.*Y))     
}

# in principe kunnen ART matrixen worden gecombineerd (van HIV1 -> ART1 en van HIV2 -> ART2)
# de techniek voor HIV en ART incidentie kunnen ook worden gebruikt voor vaccinatie
# data: file met kolommen YEAR from to en leeftijden in de overige kolommen
# YEAR from to    0 5 10 etc
# 2025 never vac  0.2 0.4 0.8 etc
# In feite is de techniek identiek voor incidentie van sterft, HIV, vaccinatie .....

scale.alive.population=function(Y,parms){
  Y[!parms$ALIVE,] = 0.
  ini.age.dist     = apply(parms$y.ini,2,sum)
  current.age.dist = apply(Y,2,sum)
  Y = scale(Y,center=F,scale = current.age.dist / ini.age.dist) 
  Y[is.nan(Y) | is.infinite(Y)]=0 
  Y 
}

colsums = function(M){
  if (is.null(dim(M)))
    return(M)
  else
    return(colSums(M))
}

Y.matrix.from.X.vector = function(X,p){
  dim(X) = c(p$nVXaSESRISKHIVTB,p$nAGES)
  rownames(X)=p$VXaSESRISKHIVTB
  colnames(X)=names(p$AGES)
  X  
}

derivs.deSolve=function(t,X,p){
  # cat(t,"\t")
  Y = Y.matrix.from.X.vector(X,p)
  ntimesteps <<- ntimesteps+1
  dY = matmul.by.age.group(p$TBp,Y)                     # TB progression
  dY = dY + derivs.Tm(t,Y,p)     # TB transmission
  if (!is.null(p$TBtr)){                                # TB treatment
    for (i in seq_along(p$TBtr)){
      dY = dY + derivs.Tr(t,Y,p$TBtr[[i]],fn=matmul.by.age.group)
    }
  }
  for (i in seq_along(p$inci$TB)){                      # TB incidence
    dY = dY + derivs.inci(t,Y,p,p$inci$TB[[i]])
  }
  if (p$nHIV>1){                                        # HIV: progression, treatment, incidence  
    dY = dY + matmul.by.age.group(p$HIVp, Y)
    if (!is.null(p$HIVtr)){
      for (i in seq_along(p$HIVtr)){
        dY = dY + derivs.Tr(t,Y,p$HIVtr[[i]],fn=matmul.by.age.group)
      }
    }
    for (i in seq_along(p$inci$HIV)){
      dY = dY + derivs.inci(t,Y,p,p$inci$HIV[[i]])
    }
  }
  if (p$nRISK>1 & !is.null(p$RISKp)){                   # RISK: progression
    dY = dY + matmul.by.age.group(p$RISKp, Y)
  }
  if (p$nSES>1  & !is.null(p$SESp)){
    dY = dY + matmul.by.age.group(p$SESp, Y)
  }
  if (p$nVXa>1){                                       # VXa: progression, incidence
    if(!is.null(p$VXap))
      dY = dY + matmul.by.age.group(p$VXap, Y)
    for (i in seq_along(p$inci$VXa)){
      dvac = derivs.inci(t,Y,p,p$inci$VXa[[i]])
      rownames(dvac)=p$VXaSESRISKHIVTB
      rows = calc.indices.for.dim(p,"VXa")==which(p$VXa == "vac")
      dY = dY + dvac
    }
    
  }
  assert_that(sum(is.nan(dY))==0,msg=paste0("NaNs in derivs.deSolve @ t=",t))
  assert <<- F # disable special type of assert 
  # NOTE: HIV and TB deaths are included in dY (as negative contributions) already due to progression)
  list(dY = as.numeric(dY), dY.HIVTBdeaths = as.numeric(colsums(dY[p$DEAD,])))
}

run.deSolve = function(params = NULL){
  assert <<- T
  
  ntimesteps <<- 0
  started.at = proc.time()
  if (params$contacts$defined){
    params$contacts$M=update.contact.matrix(params,colSums(params$y.ini[params$ALIVE,]))
  }
  params$run.params$rescaled.pop = F
  output.times = params$run.params$simulation$years
  event.times  = unique(trunc(output.times))[-1]
  # clean.event.times = cleanEventTimes(output.times, event.times)
  rawout = rk(   y = as.vector(params$y.ini), 
             times    = output.times, 
             func     = derivs.deSolve, 
             parms    = params, 
             rtol     = params$run.params$num.int$rtol, 
             atol     = params$run.params$num.int$atol, 
             method   = params$run.params$num.int$method,
             maxsteps = params$run.params$num.int$maxsteps,
             hini     = params$run.params$num.int$hini,
             hmin     = params$run.params$num.int$hmin,
             events   = list(func = aging.and.births.and.update.contact.matrix.event, 
                          time = event.times))
  if (ntimesteps>(max(params$run.params$simulation$years)-min(params$run.params$simulation$years))*50){
    modlog(level='WARN',msg=paste0(" ntimesteps = ",ntimesteps," in ",timetaken(started.at)," which may be excessive ; please reconsider parameter values"))
  }else{
    modlog(level='INFO',msg=paste0(" ntimesteps = ",ntimesteps," in ",timetaken(started.at)))
  }

  t         = rawout[,1]
  out       = vector("list",length(t)) 
  dHIVTBx   = vector("list",length(t)) 
  nr        = params$nVXaSESRISKHIVTB
  nc        = params$nAGES
  N         = nr*nc
  for (i in seq_along(t)){
      tnow = t[i]
      M = matrix(rawout[i,2:(N+1)],nr,nc) ; colnames(M)=params$AGES ; rownames(M)=params$VXaSESRISKHIVTB
      out[[i]] = M
      range = (N+2):(N+nc+1)
      dHIVTBx[[i]]   = as.vector(rawout[i,range])    ; names(dHIVTBx[[i]]) = params$AGES
  }
  list(times=t, state=out, dHIVTBx=dHIVTBx)
}

calc.vac.flows=function(p){
  df = p$vaccinated
  y = as.data.frame(melt(setDT(df), measure.vars = patterns("^A\\d+$"),variable.name = "age_from", value.name = "value"))
  z = cbind(y[,1:(ncol(y)-1)],age_thru=as.integer(rep(0,nrow(y))),value=y[,ncol(y)])
  z$age_from = as.integer(substring(z$age_from,2))
  agesfrom   = unique(z$age_from)
  agesthru   = c(agesfrom[2:length(agesfrom)],100)-1
  for (i in seq_along(agesfrom)){
    sel = z$age_from==agesfrom[i]
    z[sel,'age_thru']=agesthru[i]
  }
  z
}
run.model = function(model.params=NULL, output.flows=T){
  out  = run.deSolve(model.params)
  modlog(level="DEBUG",msg="successful result of run.deSolve()")
  options = model.params$run.params$output
  for (j in seq_along(out$times)){
    neg = model.params$run.params$num.int$min.value.for.state.var
    Y = out$state[[j]]
    tocorrect = Y<0 & Y>neg
    ntocorrect = sum(tocorrect)
    if (ntocorrect>0){
      modlog(level='WARN',paste(" at t =",out$times[j],ntocorrect,"state variables < 0 and > ",neg," reset to 0 after simulation run and before processing output"))
    }
    Y[Y<0 & Y>neg]=0
    out$state[[j]]=Y
    if (sum(Y<0)>0){
      idx = which(Y<0)
      r   = idx %% nrow(Y)
      c   = (idx %/% nrow(Y))+1 
      for (i in seq_along(idx)){
        modlog(level='ERROR',paste0(" at t = ",out$times[j]," state variables < min.value.for.state.var :",
                                    " state = ",rownames(Y)[r[i]],
                                    " ; age group = ",colnames(Y)[c[i]],
                                    "; value = ",signif(Y[r[i],c[i]])))
      }
    }
  }
  result = list()
  result$stocks = generate.prevalence.output(t=out$times,state=out$state,fparams=model.params)
  gc()
  # if (options$suppress_zeros_stocks) { stocks = stocks[value>1e-6,] }
  modlog(level="DEBUG",msg="successful generation of stocks output from generate.prevalence.output()")
  if (output.flows){
    result$flows  = generate.output(incidence.from.model.run(out,model.params),params=model.params)
    gc()
    # if (options$suppress_zeros_flows){ flows = flows[abs(value)>1e-6,] }
    modlog(level="DEBUG",msg="successful generation of flow output from generate.flow.output()")
  }
  if (options$econ.output){ 
    demography.output = new.demography.from.model.run(out,params=model.params) 
    result$population = demography.output$pop
    result$dHIVTBx    = demography.output$dHIVTBx
    result$dBGx       = demography.output$dBGx
    result$dPOPadj    = demography.output$dPOPadj
    if (model.params$intervention & !is.null(model.params$vaccinated)){
      result$vaccinated  = calc.vac.flows(model.params)
    } 
  }
  gc()
  result
}
old.run.model = function(model.params=NULL){
  out  = run.deSolve(model.params)
  modlog(level="DEBUG",msg="successful result of run.deSolve()")
  options = model.params$run.params$output
  stocks = generate.prevalence.output(t=out$times,state=out$state,fparams=model.params)
  if (options$suppress_zeros_stocks) { stocks = stocks[value>1e-4,] }
  modlog(level="DEBUG",msg="successful generation of stocks output from generate.prevalence.output()")
  flows  = generate.output(incidence.from.model.run(out,model.params),params=model.params)
  if (options$suppress_zeros_flows){ flows = flows[abs(value)>1e-6,] }
  modlog(level="DEBUG",msg="successful generation of flow output from generate.flow.output()")
  result = list(stocks=stocks,flows=flows)
  if (options$econ.output){ 
    demography.output = demography.from.model.run(out,params=model.params) 
    result$population = demography.output$pop
    result$dHIVTBx    = demography.output$dHIVTBx
    result$dBGx       = demography.output$dBGx
    result$dPOPadj    = demography.output$dPOPadj
    if (model.params$intervention & !is.null(model.params$vaccinated)){
      result$vaccinated  = calc.vac.flows(model.params)
    } 
  }
  result
}

new.demography.from.model.run=function(out,params){
  sel         = (out$times %% 1) == 0 | (out$times %% 1) == 0.5
  t           = out$times[sel]
  indexes     = (1 : length(out$state))[sel]
  assert_that(sum(sel)>0, msg="no time points at ####.5 or ####.0 found ....")
  Y           = matrix(0,nrow=length(t),ncol=ncol(out$state[[1]]))
  colnames(Y) = as.integer(colnames(out$state[[indexes[1]]]))
  selendyear  = (out$times %% 1) == 0
  tmod        = t
  tmod[selendyear]=tmod[selendyear]-1e-3
  rownames(Y) = tmod
  country     = params$run.params$countrycode
  dHIVTBx     = Y # HIV and TB deaths (numbers / yr)
  dBGx        = Y # BG  deaths (numbers / yr)
  dPOPadj     = Y # fractional adj rates (fraction of the pop / yr)
  
  if (!params$intervention){  
    for (i in seq_along(tmod)){ 
      Y[i,]         = colSums(out$state[[indexes[i]]][params$ALIVE,])
      dHIVTBx[i,]   = out$dHIVTBx[[indexes[i]]]
      dBGx[i,]      = get.deathrate.allcauses(params,tmod[i]) * Y[i,] - dHIVTBx[i,]
      n = sum(dBGx[i,]<0)
      if (n>0){
        modlog(level='DEBUG',msg=paste0("at t=",t[i]," ",n," age groups with HIV TB deaths exceeding all cause mortality (WHO deathrates) ; capping background deaths to ensure background deaths >= 0"))
      }
      dBGx[i,] = pmax(0,dBGx[i,])
      # if (t[i]>3020){cat("BASELINE: t=",t[i],"background deaths=",sum(dBGx[i,]),"\t","BASELINE: t=",t[i],"HIV TB     deaths=",sum(dHIVTBx[i,]),"\n")}
      if (i>1){
        dPOPadj[i,] = as.numeric(params$popadj[as.integer(rownames(params$popadj))==trunc(tmod[i]),])
      }else{
        dPOPadj[i,] = rep(1,ncol(Y))
      }
      # if (t[i]>2020){ cat("t=",t[i]," pop adj = ",dPOPadj[i,1:8],"\n") }
    }
  }else{
    for (i in seq_along(tmod)){
      Y[i,]         = colSums(out$state[[indexes[i]]][params$ALIVE,])
      dHIVTBx[i,]   = out$dHIVTBx[[indexes[i]]]
      dBGx[i,]      = get.bgdeathrate(params,tmod[i]) * Y[i,]
      # if (t[i]>3020){ cat("INTERVENTION: t=",t[i],"background deaths=",sum(dBGx[i,]),"\t","INTERVENTION: t=",t[i],"HIV TB     deaths=",sum(dHIVTBx[i,]),"\n")}
    }
  }
  pop          = cbind(year=tmod,country=country,as.data.frame(Y,row.names = F))
  dHIVTBx.df   = cbind(year=tmod,country=country,as.data.frame(dHIVTBx,row.names = F))
  dBGx.df      = cbind(year=tmod,country=country,as.data.frame(dBGx,row.names = F))
  if (!params$intervention){
    dPOPadj.df = cbind(year=as.numeric(rownames(params$popadj)),country=country,as.data.frame(params$popadj))
  }else{
    dPOPadj.df = NULL
  }
  modlog(level="DEBUG",msg="successful call of demography.from.model.run()")
  list(pop=pop, dHIVTBx=dHIVTBx.df, dBGx=dBGx.df, dPOPadj=dPOPadj.df)
}
