roll.matrix.by.rows = function(M,by,negate=T){
  n = nrow(M)
  P = 0 * M
  if (by>=0){ # roll down
    P[(by+1):n ,] = -M[      1 :(n-by),] 
    P[    1 :by,] = -M[(n-by+1):n     ,]
  }else{ # roll up
    by = -by
    P[      1 :(n-by),] = -M[(by+1):n ,]  
    P[(n-by+1):n     ,] = -M[    1 :by,] 
  }
  if (negate){
    return(M+P)
  }else{
    return(M-P)
  }
}

create.incidence.matrix=function(p,dim,from,to,sel=NULL){ # this function should work also for VXa (or any other dim)
  index      =  which(p$DIMNAMES==dim)
  Q          =  matrix(0,p$nVXaSESRISKHIVTB,p$nVXaSESRISKHIVTB)
  for (i in seq_along(from)){
    from.idx   =  p$ALIVE & grepl(from[i],p$inci.dim.names[[index]],ignore.case=F) 
    to.idx     =  p$ALIVE & grepl(to[i],  p$inci.dim.names[[index]],ignore.case=F)
    if (!is.null(sel)){
      from.idx = from.idx & sel
      to.idx   = to.idx & sel
    }
    x = y = numeric(p$nVXaSESRISKHIVTB)
    x[from.idx]=-1
    M = diag(x)
    d = which(to.idx)[1]-which(from.idx)[1]
    M = roll.matrix.by.rows(M,by=d)
    assert_that(sum(colSums(M)!=0)==0,msg=paste("Error in creating incidence submatrix for ",dim," from=",from," to=",to,sep=""))
    Q = Q + M
  }
  assert_that(sum(colSums(Q)!=0)==0,msg=paste("Error in creating incidence matrix for ",dim," from=",from," to=",to,sep=""))
  Matrix(Q)
}

dim.names.for.all=function(p,dimname){
  dimindex = which(p$DIMNAMES==dimname)
  names    = rep(NA,p$nVXaSESRISKHIVTB)
  indices  = calc.indices.for.dim(p,dimname)
  for (i in 1:p$DIMLENGTHS[dimindex])
    names[indices==i]=p$DIMNAMESLIST[[dimindex]][i]
  names
}

calc.indices.for.dim=function(p,dimname){
  ndim = length(p$DIMLENGTHS)
  v = (1:p$nVXaSESRISKHIVTB)-1
  switch(dimname,
         "VXa" =  return(1+((v %/% prod(p$DIMLENGTHS[2:ndim])) %% p$nVXa )),
         "SES" =  return(1+((v %/% prod(p$DIMLENGTHS[3:ndim])) %% p$nSES )),
         "RISK"=  return(1+((v %/% prod(p$DIMLENGTHS[4:ndim])) %% p$nRISK)),
         "HIV" =  return(1+((v %/% prod(p$DIMLENGTHS[5:ndim])) %% p$nHIV )),
         "TB"  =  return(1+( v %% p$nTB ))
  )
  NULL
}

calc.names.for.dim=function(p,dimname){
  names    = p$DIMNAMESLIST[[which(p$DIMNAMES==dimname)]]
  v        = calc.indices.for.dim(p,dimname)
  names[v]
}  

calc.indices.from.list=function(p,list){
  # expected order: vax,ses,risk,hiv,tb
  vax = list[[1]]
  ses = list[[2]]
  risk = list[[3]]
  hiv  = list[[4]]
  tb   = list[[5]]
  (vax-1) *p$nSES*p$nRISK*p$nHIV*p$nTB + 
    (ses-1) *p$nRISK*p$nHIV*p$nTB +
    (risk-1)*p$nHIV*p$nTB + 
    (hiv-1) *p$nTB +
    tb
}

calc.index.by.nr=function(p,vax,ses,risk,hiv,tb){
    (vax-1) *p$nSES*p$nRISK*p$nHIV*p$nTB + 
    (ses-1) *p$nRISK*p$nHIV*p$nTB +
    (risk-1)*p$nHIV*p$nTB + 
    (hiv-1) *p$nTB + tb
}

transition = function(M=NULL, from=NA, to=NA, rate=NULL){
  from=as.character(from)
  to  =as.character(to)
  M[from,from]=M[from,from]-rate
  M[  to,from]=M[  to,from]+rate
  M
}

