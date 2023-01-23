tryrun = function(mdl=NULL,initialized.params=NULL){
 tryCatch(
    expr = {
      mdl$run.model(initialized.params)
    }, 
      error = function(e) {
      output = list(stocks=NULL, flows=NULL)
      modlog(level="FATAL",msg=e$message)
      if (stri_startswith_fixed(str=e$message,"state variables <")){
        modlog(level="FATAL",msg="stopped simulation run due to negative state variables ; if input.csv was provided see output/weird folder for parameter values")
        numint = initialized.params$run.params$num.int
        modlog(level="FATAL",msg=paste("method=",numint$method," atol=",numint$atol," rtol=",numint$rtol,"min.value.for.state.var=",numint$min.value.for.state.var))
        if (!is.na(mdl$paths$targets)){
          hits     = mdl$read.targets(mdl$paths$targets)
          hits$model = -1 ; hits$fit=F ; hits$residuals = 1e10 ; hits$weighted_residuals = 1e10
          output$hits = cbind(country=mdl$paths$country.code,hits)
        }
      }
      output
    }
  )
}

run=function(mdl=NULL,new.parameter.values=NULL,write.to.file=F,write.xml=NULL, output.flows=T,
             combine.stocks.and.flows=F, baseline=NULL, output.format='txt', sample.parameters = F, exclude.intermediates=T){

  input.csv = NULL
  
  if (is.na(mdl$paths$parameters)){
    assert_that(is.null(new.parameter.values),msg="path to input.csv should not be NA when replacing parameter values with new.parameter.values")
  }else{
    input.csv = read.csv(mdl$paths$parameters, stringsAsFactors = F, header=T, fileEncoding = 'UTF-8-BOM')
    if (!is.null(new.parameter.values)){
      if(any(class(new.parameter.values)=="data.table")){
        new.parameter.values = as.data.frame(new.parameter.values)
      }
    }else if (sample.parameters){
      new.parameter.values = mdl$sample.fitted.parameters(selected.parameters = input.csv)
    }  
    if (!is.null(new.parameter.values)){
      input.csv  = mdl$modify.input.csv(input.csv,new.parameter.values)
    }
  }
  
  xmlparams  = mdl$read.model.parameters(mdl$paths)
  
  if (!is.null(baseline)){
    mdl$set.baseline.in.model.parameters(xmlparams,output=baseline)
  }

  if (!is.null(input.csv)){
    xmlparams$xml$doc = mdl$update.parameters(xmlparams$xml$doc,input.csv)
  }
  
  initialized.params = mdl$initialize.model.parameters(xmlparams)
  
  if (!is.null(write.xml)){
    mdl$write.updated.xml(initialized.params, xml=write.xml)
  }
  output = mdl$run.model(initialized.params, output.flows)
  gc()
  if (!is.null(output$stocks)){
   output  = mdl$get.target.hits(output, initialized.params, exclude.intermediates)
   gc()
   if (write.to.file){
    if (combine.stocks.and.flows){
      onerun = mdl$merge.stocks.and.flows(output)
      mdl$write.merged.stocks.and.flows(onerun,initialized.params, output.format)
    }else{
      mdl$write.stocks.and.flows(output,initialized.params, output.format) 
    }
    mdl$write.targets(output, initialized.params, output.format) 
    mdl$write.econ.output(output, initialized.params, output.format) 
   }        
  }
  output
}
