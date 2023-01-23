age.group.from.lower.age.limits = function(age.from,lower.age.limits){
  if (is.null(lower.age.limits)){
    age.group = rep("ALL",length(age.from))
  }else{
    lower=lower.age.limits
    upper=c(lower.age.limits[-1],200)
    age.group = rep(NA,length(age.from))
    for (i in seq_along(lower)){
      age.group[age.from %in% lower[i]:upper[i]]=paste0("A",lower[i])
    }
  }
  age.group
}
validate.target.states=function(target=NULL,names=NULL,p=NULL){
  names(target)=names
  substrs = stri_split_fixed(target,",")
  for (i in seq_along(names)){
    check = substrs[[i]] %in% p$DIMNAMESLIST[[names(target)[i]]]
    assert_that(all(check),msg = paste("target states",paste(substrs[[i]][!check], collapse=","),"not found in",paste(p$DIMNAMESLIST[[names(target)[i]]],collapse=",")))
  }
}
query.from.target=function(target=NA,p=NULL){ # NOTE the dead are excluded in the function calling this fn
  n = length(target)
  assert_that(n==8,msg="target row should be 8 elements")
  target[] = stri_replace_all_regex(target,replacement = "",pattern="[:space:]")
  assert_that(!is.na(target[1]),msg="NA not allowed in year column")
  mult = rep(F,n)
  mult[c(2:6)]=T # i.e. multiple states
  #num = rep(F,n)
  #num[c(1,7,8)]=T # i.e. year, age_from, age_thru
  sel = mult & (target=="all" | target=="NA" | is.na(target))
  target[sel]=NA
  NAs = is.na(target)
  target[NAs]="" 
  validate.target.states(target[!NAs & mult],names(target)[!NAs & mult],p)
  target[!NAs & mult] = stri_replace(target[!NAs & mult],"','",regex=",",mode="all")
  target[!NAs & mult] = stri_replace(target[!NAs & mult],"c('",regex="^")
  target[!NAs & mult] = stri_replace(target[!NAs & mult],"')",regex="$")
  target[!NAs & mult] = paste0(" %in% ",target[!NAs & mult])
  target[!NAs & mult] = paste(names(target)[!NAs & mult],target[!NAs & mult])
  target[1] = paste0(names(target)[1],"==",target[1])
  from = names(target)[7]
  thru = names(target)[8]
  fromage = as.integer(target[7])
  thruage = as.integer(target[8])
  if (!(fromage==0 & thruage==99)){
    agesfrom = c(p$run.params$output$age.from,100)
    q = " & ("
    for (j in seq_along(agesfrom)){
      if (fromage<=agesfrom[j] & j<length(agesfrom) & thruage>=(agesfrom[j+1]-1)){
        q = paste0(q,"(",from,"==",agesfrom[j]," & ",thru,"==",agesfrom[j+1]-1,")|")
      }
    }
    q=paste0(substr(q,1,nchar(q)-1),")")
  }else{
    q=paste0(" & (",from,"==",fromage," & ",thru,"==",thruage,")")
  }
  target=target[1:6]
  # target[num] = paste0(names(target)[num],"==",target[num])
  s = stri_join(target[target!=""],collapse=" & ")
  # print(paste(s,q))
  return(paste(s,q))
}

read.targets = function(fname){
  df = read.csv(fname,header=T,stringsAsFactors = F, fileEncoding = 'UTF-8-BOM')
  assert_that(length(unique(df[,1]))==1,msg="a target file should contain only one country")
  df = df[,-1]
  df[,2:16] = as.data.frame(sapply(df[,2:16], function (x) gsub ("\"", "", x)))
  df[,2:16] = as.data.frame(sapply(df[,2:16], function (x) gsub ("\'", "", x)))
  assert_that(sum(is.na(df$value))==0, msg="NAs not allowed in value column of targets file")
  assert_that(sum(is.na(df$lo)   )==0, msg="NAs not allowed in lo column of targets file")
  assert_that(sum(is.na(df$hi)   )==0, msg="NAs not allowed in hi column of targets file")
  assert_that(sum(is.na(df$err)  )==0, msg="NAs not allowed in err column of targets file")
  assert_that(all((df$hi-df$lo)>0), msg="not all hi > lo")
  assert_that(all(df$value>=df$lo), msg="not all values >= lo")
  assert_that(all(df$value<=df$hi), msg="not all values <= hi")
  assert_that(all(df$err>0 | df$err==-1), msg="not all err > 0 or -1")
  df  
}

exclude.not.alive=function(z){
  sesnotalive  = grepl("dead$",z$SES)  | grepl("count$",z$SES) 
  vxanotalive  = grepl("dead$",z$VXa)  | grepl("count$",z$VXa) 
  hivnotalive  = grepl("dead$",z$HIV)  | grepl("count$",z$HIV) 
  tbnotalive   = grepl("dead$",z$TB)   | grepl("count$",z$TB)
  risknotalive = grepl("dead$",z$RISK) | grepl("count$",z$RISK)
  z[!(vxanotalive | sesnotalive | hivnotalive | tbnotalive | risknotalive),]
}
calc.count=function(z,target,p=NULL){
  assert_that(target$type=="count",msg="count type should be 'count' and stock cause should be NA")
  target[1]=target[1]+0.499 # count output for e.g. year 2010.5 is at 2011
  q = query.from.target(target[c(1:6,9,10)],p)
  modlog(level='DEBUG',msg=paste0("querying count in target evaluation with: ",q))
  as.numeric(z[eval(parse(text=q)),.(value=sum(value))])
}
calc.stock=function(z,target,p=NULL){
  assert_that(target$type=="stock",msg="stock type should be 'stock' and stock cause should be NA")
  q = query.from.target(target[c(1:6,9,10)],p)
  modlog(level='DEBUG',msg=paste0("querying stock in target evaluation with: ",q))
  as.numeric(z[eval(parse(text=q)),.(value=sum(value))])
}

calc.flow=function(z,target,p=NULL){
  causes = stri_trim_both(unlist(strsplit(target$cause,",")))
  allowed_causes = c('Tm',paste0(names(p$inci.dim.names[!is.null(p$inci.dim.names)]),'Xi'),paste0(p$DIMNAMES,'p'),paste0('TBtr_',names(p$TBtr)),paste0('HIVtr_',names(p$HIVtr)))
  err = sum(causes %in% allowed_causes)!=length(causes)
  assert_that(!err, msg="undefined cause for flow")
  if (err){
    modlog(level="ERROR",msg="undefined cause for flow")
  }
  if (target$type=="inflow"){ inflow = T }else if (target$type=="outflow"){ inflow = F }else{ inflow = NULL }
  assert_that(!is.null(inflow), msg="type for flow should be 'inflow' or 'outflow'")
  q = query.from.target(target[c(1:6,9,10)],p)
  q = paste0(q," & flow ==",ifelse(inflow,"'in'","'out'"))
  q = paste0(q," & subject %in% c('",paste0(causes,collapse="','"),"')")
  modlog(level='DEBUG',msg=paste0("querying flow in target evaluation with: ",q))
  as.numeric(z[eval(parse(text=q)),.(value=sum(value))])
}

calc.targets = function(z,targets,p=NULL){
  model = rep(NA, nrow(targets))
  for (i in seq_along(targets[,1])){
    res = list(x=NULL, y=NULL)
    for (j in seq_along(res)){
      range = c(1,(j-1)*7 + 2:8,17,18)
      row = targets[i,range]
      names(row)[2:8]=substring(names(row)[2:8],2)
      if (row$type == "count"){
        res[[j]] = calc.count(z$stocks,row,p=p)
      }else if (row$type == "stock"){
       res[[j]] = calc.stock(exclude.not.alive(z$stocks),row,p=p)
      }else if(row$type == "inflow"){
       res[[j]] = calc.flow(z$flows,row,p=p)
      }else if(row$type == "outflow"){
       res[[j]] = calc.flow(z$flows,row,p=p)
      }
    }
    x = res$x
    y = res$y
    eval(parse(text=paste0("model[i]=",targets[i,]$fn)))
  }
  # subset = targets$err>=0
  # weighted_residuals = residuals = fit = rep(NA,length(targets$err))
  # fit[subset] = model[subset] >= targets[subset,]$lo & model[subset] <= targets[subset,]$hi
  # residuals[subset] = model[subset] - targets[subset,]$value
  # weighted_residuals[subset] = residuals[subset] / targets[subset,]$err
  cbind(targets,model=model)
}

eval.output.vs.targets = function(output,targets,p=NULL,exclude.intermediates=F){
  ok = all(targets$year %in% unique(output$stocks$year))
  if (!ok){
    cat("target(s) specified that are not in output:\n")
    indxs = !(targets$year %in% unique(output$stocks$year))
    apply(targets[indxs,],1,function(x) cat("year:",x["year"]," name:",x["name"],"\n"))
  }
  assert_that(ok,msg="targets specified that are not in output, modify xml file")
  selx = (targets$xtype %in% c("stock","inflow","outflow","count"))
  sely = (targets$ytype %in% c("stock","inflow","outflow","count"))
  assert_that(all(selx == sely),msg="inconsistent definition of combined targets")
  result = calc.targets(output,targets[selx,],p=p)
  result2 = NULL
  subtargets = targets[!selx,]
  if (nrow(subtargets)>0){
    n = nrow(subtargets)
    for (i in 1:n){
      row  =  subtargets[i,]
      x = result[result$name == row$xtype & result$year == row$year,"model"]
      y = result[result$name == row$ytype & result$year == row$year,"model"]
      if (!(is.na(row$xcause) | row$xcause=="")){
        a = result[result$name == row$xcause & result$year == row$year,"model"]
      }
      if (!(is.na(row$ycause) | row$ycause=="")){
        b = result[result$name == row$ycause & result$year == row$year,"model"]
      }
      eval(parse(text=paste0("row$model=",row$fn)))
      result2 = rbind(result2,row)
    }
  }
  z = rbind(result[result$err>=0,], result2)
  weighted_residuals = residuals = fit = z$err * 0
  fit = z$model >= z$lo & z$model <= z$hi
  residuals = z$model - z$value
  weighted_residuals = residuals / z$err
  z = cbind(z,fit,residuals,weighted_residuals)
  modlog(level="DEBUG",msg="successful call of eval.output.vs.targets()")
  z
#  if (exclude.intermediates){
#    return(rbind(result[!is.na(result$fit),],result2))
#  }else{
#    return(rbind(result,result2))
#  }
}

