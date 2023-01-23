optimize.params=function(Ti){ # for example the transition matrix ...
  M.list = Ti$parameter.matrices
  n = length(M.list)
  if (n==1)
    return(Ti)
  M = M.list[[1]]
  n.elements = nrow(M)*ncol(M)
  uniq = 0*(1:n)+T
  uniq[1]=F
  for (i in 2:length(M.list)){
    if (is.na(sum(sum(M.list[[i]]==M)==n.elements))){
      print("NA in function optimize.params() ???")
    }            
    if (sum(M.list[[i]]==M)==n.elements)
      uniq[i]=F
    else
      return(Ti)
  }
  if (sum(uniq)==0){
    Ti$parameter.matrices = list(M)
    Ti$age.groups = Ti$age.groups[1]
    Ti$age.from.index = Ti$age.from.index[1]
    Ti$age.thru.index = Ti$age.thru.index[n]
    return(Ti)
  }
}

fractions.by.stage.at.start=function(p, df=NULL){

  assert_that(!is.null(df),msg="error in fraction.by.age.at.start(): data frame with initial TB distribution required ")
  
  rownames = p$VXaSESRISKHIVTB
  colnames = p$DIMNAMES[!p$DIMNAMES=="TB"] # exclude TB
  M = matrix(0,length(rownames),length(colnames),dimnames=list(rownames,colnames))

  for (dim in colnames){
    ind = calc.indices.for.dim(p,dim)
    for (i in 1:p$DIMLENGTHS[dim])
      M[,dim][ind==i]=p$ATBIRTH[[dim]][i]
  }

  x = apply(M,1,prod) # we now have a vector of fractions of all dims except TB

  P = matrix(0, nrow = p$nVXaSESRISKHIVTB, ncol = p$nAGES, dimnames = list(p$VXaSESRISKHIVTB,names(p$AGES)))
  for (i in 1:nrow(df)){
    rows = calc.indices.for.dim(p,"TB")==which(p$TB == df$stage[i])
    range = p$AGES >= df$age.from[i] & p$AGES <= df$age.thru[i]
    P[rows,range] = df$fraction[i]
  }
  y = x * P
  assert_that(sum(colSums(y)-1.0)<1e-9,msg="something rotten in setting up the at.start matrix .....")
  y 
}


fractions.by.stage.at.birth=function(p){
  rownames = p$VXaSESRISKHIVTB ; colnames = p$DIMNAMES
  M = matrix(0,length(rownames),length(colnames),dimnames=list(rownames,colnames))
  for (dim in p$DIMNAMES){
    ind = calc.indices.for.dim(p,dim)
    for (i in 1:p$DIMLENGTHS[dim])
      M[,dim][ind==i]=p$ATBIRTH[[dim]][i]
  }
  x = apply(M,1,prod) # we now have a vector of fractions of all dims 
  x
}

replace.NAs.by=function(nodes,attrib,replace=0,first=NA,n=NA){
  v = as.numeric(xml_attr(nodes,attrib))
  if (sum(is.na(v))==n){
    v = rep(0.,n)
    if (!is.na(first))
      v[1]=first
  }else{
    v[is.na(v)]=0.
  }
  v
}

check.sum.to.1 = function(v){
  if (abs(sum(v)-1.0)>1e-4)
    stop(paste("values of ",names(v)," should add up to 1.0",sep=""))
  v
}

create.approx.func.from.rownames=function(df,offset=0.0){
  x = as.numeric(rownames(df))+offset
  y = 1:nrow(df)
  if (nrow(df)==1){
    return(function(t){y})
  }else{
    return(approxfun(x,y,rule=2))
  }
}
get.deathrate.allcauses=function(p, t){
  return(p$deathrate[p$deathfn(t),])
}

get.bgdeathrate=function(p, t){
  return(p$bgdeathrate[p$bgdeathfn(t),])
}

create.approx.fun=function(row, df=NULL){
  s = as.character(row["values"]) ; values = unlist(strsplit(s,","))
  NAs = sum(is.na(suppressWarnings(as.numeric(values))))
  if (!is.null(df) & (NAs>0)){
    selcols  = which(names(df) %in% c("name","value"))
    df.left  = as.data.frame(df[,-selcols])
    sel = rowSums(is.na(df.left)) == ncol(df.left)
    df  = df[sel,selcols]
    eval(parse(text = paste0(df$name,"=",df$value)))
    s = unlist(strsplit(as.character(row["times"]),","))
    if (length(s)==1){
      eval(parse(text = paste0("x=",s)))
    }else{
      x = as.numeric(s)
    }
    y = 0 * x
    if (length(values)==1){
      eval(parse(text = paste0("y=",values)))
    }else{
      for (i in 1:length(y)){
        eval(parse(text = paste0("y[i]=",values[i])))
      }
    }
    y = as.numeric(y)
    return(approxfun(x,y,rule=2))
  }else{
  s = as.character(row["times"])
  x = as.numeric(unlist(strsplit(s,",")))
  y = as.numeric(values)
  return(approxfun(x,y,rule=2))
  }
}

calc.birthrate = function(fname=NA, countrycode=NA){
  if (!is.na(countrycode)){
    pop = read.csv(fname,header=F, fileEncoding = 'UTF-8-BOM')
    pop = pop[pop[,1]==countrycode,]
    nr  = nrow(pop)
    nc  = ncol(pop)
    x   = pop[2:nr,2]
    y   = pop[2:nr,3]/rowSums(pop[1:(nr-1),3:102])
    return(approxfun(x,y,rule=2))
  }
  return(NULL)
}

scale.population = function(pop=NA,fname=NA){
    if (!is.na(fname)){
      sumpop        = sum(pop)
      pop           = 0*pop
      pop.fracs     = as.matrix(read.delim(file=fname,sep="\t",header=T,stringsAsFactors = F, row.names=1, fileEncoding = 'UTF-8-BOM'))
      assert_that(dim(pop)[2]==dim(pop.fracs)[2])
      sum.pop.fracs = sum(pop.fracs)
      if (sum.pop.fracs<0.99 | sum.pop.fracs>1.01)
        warning(paste("The file ",fname, " does not appear to contain fractions adding up to 1.0 ?",sep=""))
      rowsel = rownames(pop) %in% rownames(pop.fracs)
      pop[rowsel,] = pop.fracs
      return(pop * sumpop)
    }
  NULL
}

seed.initial.population=function(parms, df){
  z = t(parms$y.ini.read[1,] * t(parms$at.start))
  assert_that(sum(abs(colSums(z)-parms$y.ini.read[1,]))<1e-3,msg="seeded infections not specified correctly. NOTE: do not specify a fraction for the 1st stage !")
  parms$y.ini = z
}

init.constants = function(p,xml){
  p$xml = xml
  with(p,{
    DAYSPERYEAR = 365.2425
    TB   = xml_attr(xml_find_all(xml$doc,"//TB/TB.stages/stage"),"name")     ; nTB   = length(TB) 
    HIV  = xml_attr(xml_find_all(xml$doc,"//HIV/HIV.stages/stage"),"name")   ; nHIV  = length(HIV) 
    RISK = xml_attr(xml_find_all(xml$doc,"//RISK/RISK.stages/stage"),"name") ; nRISK = length(RISK) 
    SES  = xml_attr(xml_find_all(xml$doc,"//SES/SES.stages/stage"),"name")   ; nSES  = length(SES) 
    VXa  = xml_attr(xml_find_all(xml$doc,"//VXa/VXa.stages/stage"),"name")   ; nVXa  = length(VXa)

    iTB   = check.sum.to.1(replace.NAs.by(xml_find_all(xml$doc,"//TB/TB.stages/stage"),"fraction.at.birth",0,1,nTB))  
    iHIV  = check.sum.to.1(replace.NAs.by(xml_find_all(xml$doc,"//HIV/HIV.stages/stage"),"fraction.at.birth",0,1,nHIV))     
    iRISK = check.sum.to.1(replace.NAs.by(xml_find_all(xml$doc,"//RISK/RISK.stages/stage"),"fraction.at.birth",0,1,nRISK))     
    iSES  = check.sum.to.1(replace.NAs.by(xml_find_all(xml$doc,"//SES/SES.stages/stage"),"fraction.at.birth",0,1,nSES))     
    iVXa  = check.sum.to.1(replace.NAs.by(xml_find_all(xml$doc,"//VXa/VXa.stages/stage"),"fraction.at.birth",0,1,nVXa))     
    
    ATBIRTH = list(VXa=iVXa,SES=iSES,RISK=iRISK,HIV=iHIV,TB=iTB)
    
    # NOTE : the order below VXa - SES - RISK - HIV - TB should NEVER be changed !!!
    
    DIMNAMES     = c("VXa","SES","RISK","HIV","TB")
    DIMNAMESLIST = list(VXa=VXa,SES=SES,RISK=RISK,HIV=HIV,TB=TB)
    DIMLENGTHS   = c(nVXa,nSES,nRISK,nHIV,nTB)
    names(DIMLENGTHS)=DIMNAMES
    
    HIVTB = NULL ; RISKHIVTB = NULL ; SESRISKHIVTB = NULL ; VXaSESRISKHIVTB = NULL;
    nHIV  = length(HIV)
    for (i in 1:nHIV) 
      HIVTB = c(HIVTB,paste(HIV[i],TB,sep="-")) 
    nHIVTB  = length(HIVTB)
    for (i in 1:nRISK) 
      RISKHIVTB = c(RISKHIVTB,paste(RISK[i],HIVTB,sep="-")) 
    nRISKHIVTB  = length(RISKHIVTB)
    for (i in 1:nSES)  
      SESRISKHIVTB = c(SESRISKHIVTB,paste(SES[i],RISKHIVTB,sep="-"))
    nSESRISKHIVTB  = length(SESRISKHIVTB)
    for (i in 1:nVXa)  
      VXaSESRISKHIVTB = c(VXaSESRISKHIVTB,paste(VXa[i],SESRISKHIVTB,sep="-"))
    nVXaSESRISKHIVTB  = length(VXaSESRISKHIVTB)

    inci.dim.names = list(VXa=dim.names.for.all(environment(),"VXa"),
                         NULL,
                         NULL,
                         HIV=dim.names.for.all(environment(),"HIV"),
                         NULL)

    eval(parse(text=paste("AGES=c(",xml_attr(xml_find_all(xml$doc,"//ages"),"lower.limits"),")",sep="")))
    names(AGES)      = paste("A",AGES,sep="") ; nAGES = length(AGES) 
    DEAD =  rep(F,nVXaSESRISKHIVTB)
    COUNT = rep(F,nVXaSESRISKHIVTB)
    for(name in names(p$DIMNAMESLIST)){
      DEAD  = DEAD  | grepl( "dead$",calc.names.for.dim(environment(),name))
      COUNT = COUNT | grepl("count$",calc.names.for.dim(environment(),name))
    }
    ALIVE     = !(DEAD | COUNT)   
  })
}
create.contacts.matrices=function(paths=NULL, model.params=NULL){
  assert_that(!is.null(paths),msg=paste("paths argument should not be NULL in",match.call()[[1]]))
  assert_that(is.environment(paths),msg=paste("paths argument should be an environment in",match.call()[[1]]))
  assert_that(!is.null(model.params),msg=paste("model.params argument should not be NULL in",match.call()[[1]]))
  if (!is.na(model.params$run.params$contact.matrix.txt)){
    temp=here(paths$country.dir,model.params$run.params$contact.matrix.txt)
    contactmatrixfname = stri_replace_last(str=temp,replacement=paste0("/",model.params$run.params$countrycode,"_"),fixed="/")
    M = Matrix(expand.contact.matrix(model.params,read.contact.matrix(contactmatrixfname)))
    MplustM = M+t(M)
    return(list(defined=T,M=M,MplustM=MplustM))
  }else{
    return(list(defined=F,M=NULL,MplustM=NULL))
  }
}
initialize.incidence=function(paths=NULL,p=NULL){
  output = p$run.params$output
  inci.data = list()
  inci.data$TB  = inci.data.frame.from.xpath(p$xml,"//TB/TB.incidence/incidence.data")
  inci.data$HIV = inci.data.frame.from.xpath(p$xml,"//HIV/HIV.incidence/incidence.data")      
  inci.data$VXa = inci.data.frame.from.xpath(p$xml,"//VXa/VXa.incidence/incidence.data")
  
  for (dim in c("TB","HIV","VXa")){
    row = output$options$dim==dim
    tf  = output$options[row,"incidence"] & !is.null(inci.data[[dim]])
    output$options[row,"incidence"] = tf                                                
  }
  
  inci = list()
  for (name in names(inci.data)){
    df = inci.data[[name]]
    if (nrow(df)>0){
      inci[[name]]=list()
      for (i in seq_along(1:nrow(df))){
        inci[[name]][[i]]=init.incidence(p, fname=here(paths$country.dir,df$file[i]), 
                                            countrycode  = p$run.params$countrycode, 
                                            times=df$times[i], 
                                            values=df$values[i], 
                                            proportions=df$proportions[i], 
                                            denominator=df$denominator[i],
                                            once.per.year=df$once.per.year[i]==T)
      }
    }
  }
  
  
  if (!is.na(p$intervention.start.from)){
    from = p$intervention.start.from
    rownr = 1
    assert_that(all(inci[[from]][[1]]$inci[rownr,]==0),msg=paste("the first row of the",from,"incidence data should only contain 0 values"))
    if (nrow(inci[[from]][[1]]$inci)>1){ rownr = 2 }
    p$intervention.start = as.numeric(rownames(inci[[from]][[1]]$inci)[rownr])
  }
  if (p$intervention){
    assert_that(p$intervention.start<2200,msg=paste("intervention start =",p$intervention.start,"; is that intended?"))
    modlog(level='WARN',paste0("intervention start = ",p$intervention.start," ??? ; is that intended?"))
  }
  inci
}

initialize.TB.transmission=function(p=NULL){
  output = p$run.params$output
  xmltransitions = xml_find_all(p$xml$doc,"//TB/TB.transmission/transition.matrix/transition")
  if (length(xmltransitions)>0){    
    xmltimedparameters = xml_find_all(p$xml$doc,"//TB/TB.transmission/contact.rate.multiplier")
    xmlauxparameters   = xml_find_all(p$xml$doc,"//TB/TB.transmission/transition.matrix/parameter")
    
    Tm = init.parameters.from.xml(p,xmlpath="//TB/TB.transmission", xmlparameter="TB.parameter",
                                    xmltransitions=xmltransitions, xmlauxparameters=xmlauxparameters,xmltimedparams=xmltimedparameters)
    #Tm = optimize.params(Tm) 
    output$options[output$options$dim=="TB",]$transmission = T
    return(Tm)
  }else{
    return(NULL)
  }
}

initialize.TB.infectivity=function(p=NULL){
  xmltransitions = xml_find_all(p$xml$doc,"//TB/TB.infectivity/infectivity.matrix/infectivity")
  optimize.params(init.parameters.from.xml(p, xmlpath="//TB/TB.infectivity", xmlparameter="TB.parameter",xmltransitions=xmltransitions,values=T)) 
}

initialize.treatment=function(p=NULL,dim=NA){
  if (p$DIMLENGTHS[dim]>1){
    output = p$run.params$output
    xmlpath = paste0("//",dim,"/",dim,".progression")
    nodeset = xml_find_all(p$xml$doc,paste0(xmlpath,"/treatment.matrix"))
    if (length(nodeset)>0){
      tr = list()
      for (i in 1:length(nodeset)){
        tr[[i]] = init.parameters.from.xml(p, xmlpath=xmlpath,
                                            xmlparameter=   paste0(dim,".parameter"),
                                            xmltransitions= xml_find_all(nodeset[i],"transition"),
                                            xmlauxparameters   = xml_find_all(nodeset[i],"parameter"),
                                            xmltimedparams= xml_find_all(nodeset[i],"multiplier"))
        names(tr)[i]=xml_attr(nodeset[i],"name")
        if (sum(is.na(as.matrix(tr[[i]]$parameter.matrices[[1]])))){
          print("found")
        }
        #tr[[i]] = optimize.params(tr[[i]]) 
        tr[[i]]$aggregated=F
      }
      output$options[output$options$dim==dim,]$treatment = T
      
      return(tr)
    }
  }
  return(NULL)
}

initialize.progression=function(p=NULL,dim=NA){
  if (p$DIMLENGTHS[dim]>1){
    output = p$run.params$output
    xmlpath        = paste0("//",dim,"/",dim,".progression")
    xmltransitions = xml_find_all(p$xml$doc,paste0(xmlpath,"/transition.matrix/transition"))
    xmlauxparameters = xml_find_all(p$xml$doc,paste0(xmlpath,"/transition.matrix/parameter"))
    if (length(xmltransitions)>0){
      progression = init.parameters.from.xml(p,xmlpath=xmlpath,xmlparameter=paste0(dim,".parameter"),xmltransitions=xmltransitions, xmlauxparameters=xmlauxparameters)
      #progression = optimize.params(progression) 
      output$options[output$options$dim==dim,]$progression = T
      for (i in seq_along(p$inci[[dim]])){
        p$inci[[dim]][[i]]$inci.trend.fn = parse.time.series(p, p$inci[[dim]][[i]], progression$fixed.parameters)
      }
      progression$aggregated=F
      return(progression)
    }
  }
  return(NULL)
}


read.model.parameters=function(paths=NULL){
  assert <<- T
  assert_that(!is.null(paths),msg=paste("paths should not be NULL",match.call()[[1]]))
  assert_that(is.environment(paths),msg=paste("paths argument should be an environment in",match.call()[[1]]))
  assert_that(is.readable(paths$xml),msg=paste0("cannot read XML",paths$xml))

  p = new.env()
  with(p,{
    paths         = paths
    run.params    = parse.run.spec(paths$xml)
    if (!is.null(paths[["country.code"]])){ run.params$countrycode = paths$country.code }
    xmlfilename   = paths$xml
    init.constants(environment(),xml=read.xml(paths$xml,"/TB.Vx.model.inputfile"))
    intervention  = !is.na(paths[["baseline"]])
    aging.matrix  = create.aging.matrix(environment())
    if (!any(is.na(paths[["VXa.incidence.files"]]))){
      set.node.attrs(xml$doc, "//VXa/VXa.incidence/incidence.data", attrname="file", newvalues=paths$VXa.incidence.files)
    }
    assert_that(!is.na(run.params$population.csv),msg="path to population.csv is required")
    popfname = here(paths$country.dir,run.params$population.csv)
    y.ini.read = read.population(p           = environment(),
                            rownms      = p$VXaSESRISKHIVTB, 
                            colnms      = p$AGES, 
                            fname       = popfname, 
                            countrycode = run.params$countrycode, 
                            year        = run.params$simulation$years[1])
      
    assert_that(run.params$birthrate.from.data,msg="<birthrate from.population.data='true'/> is required in this model version")
    birthrate = calc.birthrate(fname=popfname, countrycode = run.params$countrycode)
    
    unpopdem       = get.demography(environment(),fname=popfname, countrycode=run.params$countrycode)
    unpopdemfn     = create.approx.func.from.rownames(unpopdem)
    assert_that(!is.na(run.params$deathrate.txt),msg="path to deathrates.csv is required")
    deathrate      = deathrate.from.data(paths, environment())
    deathfn        = create.approx.func.from.rownames(deathrate)
    contacts       = create.contacts.matrices(paths, environment())
    intervention.start.from = xml_attr(xml_find_all(xml$doc,"//simulation/options/intervention.start"),"from.incidence.data")
    if (length(intervention.start.from) == 0){
      intervention.start.from = NA
    }
    intervention.start = xml_attr(xml_find_all(xml$doc,"//simulation/options/intervention.start"),"year")
    if (length(intervention.start) == 0){
      intervention.start = 1e6
    }
    inci           = initialize.incidence(paths, environment()) # i.e. read the incidence data and initialize all but the (parameter dependent) values series
  })
  modlog(level = 'DEBUG',msg="successful result of read.model()")
  p
}


set.baseline.in.model.parameters=function(params=NULL,output=NULL){
  assert_that(!is.null(output),msg="output argument should not be NULL in set.baseline.in.model.parameters()")
  assert_that(!params$intervention,msg="set baseline pop adjustment by EITHER providing a baseline arg pointing to files OR using the output data")
  params$popadjrateintv = matrix.from.popdf(params,pop=output$dPOPadj)
  params$popadjfnintv   = create.approx.func.from.rownames(params$popadjrateintv)
  Y                     = matrix.from.popdf(params,pop=output$dBGx)
  Z                     = matrix.from.popdf(params,pop=output$population)
  params$bgdeathrate    = Y/Z
  params$bgdeathfn      = create.approx.func.from.rownames(params$bgdeathrate)
  params$intervention=T
}
initialize.aggregated.TBHIVptr=function(TBp=NULL,TBtr=NULL,HIVp=NULL,HIVtr=NULL){
  agg=new.env()
  agg$age.ranges = TBp$age.ranges
  agg$age.groups = TBp$age.groups
  agg$parameter.matrices = TBp$parameter.matrices
  TBp$aggregated=T
  for (i in seq_along(TBtr)){
    if (is.null(TBtr[[i]]$timed.parameters)){
      TBtr[[i]]$aggregated = T
      for (j in seq_along(TBtr[[i]]$parameter.matrices)){
        agg$parameter.matrices[[j]]=agg$parameter.matrices[[j]]+TBtr[[i]]$parameter.matrices[[j]]
      }
    }
  }
  if (is.null(HIVp)){
    return(agg)
  }
  if (all(TBp$age.groups==HIVp$age.groups)){
    HIVp$aggregated=T
    for (i in seq_along(HIVp$parameter.matrices)){
      agg$parameter.matrices[[i]]=agg$parameter.matrices[[i]]+HIVp$parameter.matrices[[i]]
    }
    for (i in seq_along(HIVtr)){
      if (is.null(HIVtr[[i]]$timed.parameters)){
        HIVtr[[i]]$aggregated = T
        for (j in seq_along(HIVtr[[i]]$parameter.matrices)){
          agg$parameter.matrices[[j]]=agg$parameter.matrices[[j]]+HIVtr[[i]]$parameter.matrices[[j]]
        }
      }
    }  
  }
  return(agg)
}
  
initialize.model.parameters=function(params=NULL){
   assert_that(!is.null(params),msg="pass (updated) parameters as an argument to initialize.model.parameters")
   with(params,{
    at.birth = fractions.by.stage.at.birth(environment())
    at.start = fractions.by.stage.at.start(environment(),df=get.seeded.infections(xml$doc))
    TBp   = initialize.progression(environment(),dim="TB")
    TBtr  = initialize.treatment(environment(),dim="TB")
    Tm    = initialize.TB.transmission(environment())
    Infc  = initialize.TB.infectivity(environment())
    HIVp  = initialize.progression(environment(),dim="HIV")
    HIVtr = initialize.treatment(environment(),dim="HIV")
    RISKp = initialize.progression(environment(),dim="RISK")
    SESp  = initialize.progression(environment(),dim="SES")
    VXap  = initialize.progression(environment(),dim="VXa")
    TBHIVptr = initialize.aggregated.TBHIVptr(TBp=TBp,TBtr=TBtr,HIVp=HIVp,HIVtr=HIVtr)
    #TBHIVptr = optimize.params(TBHIVptr)
    seed.initial.population(environment())
  })
  params$hash  =  digest(params)
  modlog(level = 'DEBUG',msg="successful result of init.var.parameters.and.matrices()")
  params
}

