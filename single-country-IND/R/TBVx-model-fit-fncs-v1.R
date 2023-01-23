require(FME)
require(reshape2)

model.output=function(z){
  y = z %>%
    filter(YEAR %in% c(1970,2000,2020) & TB != 'TBdead') %>%
    group_by(YEAR,TB) %>%
    summarise(A15 = sum(A15, na.rm = TRUE), .groups="drop") %>%
    mutate(prevA15 = A15/sum(A15)) %>%
    select(TB,YEAR,prevA15)
  y = as.data.frame(y)
  # now we need output in columns
  dcast(y,YEAR ~ TB, value.var='prevA15')
}

depr.fit.model = function(params.to.fit, xpaths, fparams, obs){
  for (i in seq_along(params.to.fit)){
    fparams$xml$doc = modify.node.attr(fparams$xml$doc, xpaths[i], names(params.to.fit)[i], params.to.fit[i])
  }
  params = init.parameters(fparams$xmlfile,fparams$xmlrunfile, fparams)
  z = run.model(params,atol=1e-3,rtol=1e-6)
  modl = model.output(z)
  modCost(modl,obs,x="YEAR", y="prevA15")
}

