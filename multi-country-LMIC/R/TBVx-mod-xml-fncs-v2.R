update.constant.parameters = function(params=NULL, constant.params=NULL){
  assert_that(!is.null(params) & !is.null(constant.params),msg="arguments of update.constant.parameters() should not be NULL")
  return(update.parameters(params$xml$doc,constant.params))
}
  
update.parameters = function(xmldoc, constant.params){
  assert_that(nrow(constant.params)>0,msg="parameter set for updating XML is empty?")
  attrnames = c("VXa.stage","SES.stage","RISK.stage","HIV.stage","age.group")
  xpath     = constant.params$xpath
  replaced  = 0
  toreplace = nrow(constant.params)
  for (i in 1:nrow(constant.params)){
    row = constant.params[i,]
    sel = !is.na(row[attrnames])
    xattr = row[attrnames][sel]
    nattr = length(xattr)
    if (nattr>0){
      names(xattr)=attrnames[sel]
    }
    xpaths=stri_trim_both(unlist(strsplit(xpath[i],";")))
    if (length(xpaths)>1){
      toreplace = toreplace + length(xpaths) - 1
    }
    for (j in seq_along(xpaths)){
      parameters=xml_find_all(xmldoc,paste0(xpaths[j],"[@name='",row$name,"']"))
      if (length(parameters)<=0){
        print("wtf")
      }
        
      assert_that(length(parameters)>0,msg="parameter to replace not found in XML")
      if (nattr==0){
        assert_that(length(parameters)==1,msg="multiple occurrences of unique parameter in XML")
        xml_set_attr(parameters[1],"value",row$mean)
        replaced = replaced + 1
      }else{ # what to do for multiple attr ??
        index = rep(T,length(parameters))
        for (k in seq_along(names(xattr))){
          index = index & xml_attr(parameters,names(xattr)[k]) %in% stri_trim_both(stri_split_fixed(xattr[k],",")[[1]])
        }
        assert_that(sum(is.na(index))==0,msg=paste0(paste0(names(xattr),collapse=" ")," not found for parameter ",row$name," i.e. ",row$unique.name))
        assert_that(sum(index)>0,msg=paste0(paste0(names(xattr),collapse=" ")," not found for parameter ",row$name," i.e. ",row$unique.name))
        # assert_that(sum(index)<2,msg=paste0(paste0(names(xattr),collapse=" ")," more than one match found for parameter ",row$name," i.e. ",row$unique.name))
        xml_set_attr(parameters[index],"value",row$mean)
        replaced = replaced + 1
      }
      # modlog(msg=paste0(as.character(row$unique.name),'=',signif(as.numeric(row$mean)),collapse=" "))
    }
  }
  assert_that(replaced == toreplace,msg="not all constant parameters replaced in XML ??")
  xmldoc
}

update.fitted.parameters=function(params,fitted.params,param.values){
  
  # This function expects three arguments:
  # fparams: the result of parsing an XML input file i.e. an R environment containing all
  # fitted.params: a data frame (from reading input.csv) which is used to find the xpaths to unique parameters
  # param.values:  a named vector with new parameter values
  
  
  if (length(param.values)<1){
    return(params$xml$doc)
  }  
  attrnames = c("VXa.stage","SES.stage","RISK.stage","HIV.stage","age.group")
  xmldoc = params$xml$doc
  
  toreplace = length(param.values)
  replaced  = 0
  
  for (i in seq_along(param.values)){
    row = fitted.params[fitted.params$unique.name==names(param.values)[i],]
    sel = !is.na(row[attrnames])
    xattr = row[attrnames][sel]
    nattr = length(xattr)
    if (nattr>0){
      names(xattr)=attrnames[sel]
    }
    xpaths=stri_trim_both(unlist(strsplit(row$xpath,";")))
    if (length(xpaths)>1){
      toreplace = toreplace + length(xpaths) - 1
    }
    for (j in seq_along(xpaths)){
      parameters=xml_find_all(xmldoc,paste0(xpaths[j],"[@name='",row$name,"']"))
      if (length(parameters)<=0){
        print("wtf")
      }      
      assert_that(length(parameters)>0,msg="parameter to replace not found in XML")
      if (nattr==0){
        assert_that(length(parameters)==1,msg="multiple occurrences of unique parameter in XML")
        xml_set_attr(parameters[1],"value",param.values[i])
        replaced = replaced + 1
      }else{ # what to do for multiple attr ??
        index = rep(T,length(parameters))
        for (k in seq_along(names(xattr))){
          index = index & xml_attr(parameters,names(xattr)[k]) %in% stri_trim_both(stri_split_fixed(xattr[k],",")[[1]])
        }
        assert_that(sum(is.na(index))==0,msg=paste0(paste0(names(xattr),collapse=" ")," not found for parameter ",row$name," i.e. ",row$unique.name))
        assert_that(sum(index)>0,msg=paste0(paste0(names(xattr),collapse=" ")," not found for parameter ",row$name," i.e. ",row$unique.name))
        # assert_that(sum(index)<2,msg=paste0(paste0(names(xattr),collapse=" ")," more than one match found for parameter ",row$name," i.e. ",row$unique.name))
        xml_set_attr(parameters[index],"value",param.values[i])
        replaced = replaced + 1
      }
      # modlog(msg=paste0(as.character(row$unique.name),'=',signif(as.numeric(row$mean)),collapse=" "))
    }
  }
  assert_that(replaced == toreplace,msg="not all fittable parameters replaced in XML ??")
  xmldoc
}

set.node.attrs = function(xmldoc, xpath, attrname=NULL, newvalues=NULL){
  assert_that(!(is.null(attrname) | is.null(newvalues)),msg="attrname and newvalues should not be NA / NULL")
  nodeset=xml_find_all(xmldoc,xpath)
  xml_attr(nodeset,"times")
  for (i in seq_along(newvalues)){
    xml_set_attr(nodeset[i],"file",newvalues[i])
  }
  xmldoc
}

# new version with extra argument
modify.node.attr = function(xmldoc, xpath, name=NA, value=NULL, xattr=NULL){
  assert_that(!(is.na(name) & is.null(xattr)),msg="name and xattr cannot both be NA / NULL")
  parameters=xml_find_all(xmldoc,xpath)
  if (!is.null(xattr)){
    assert_that(!is.null(names(xattr)),msg="xattr should be a named character vector")  
    index = xml_attr(parameters,names(xattr)) == xattr
    index[is.na(index)]=FALSE
  }else{
    index = xml_attr(parameters,"name") == name
  }
  node = parameters[index]
  xml_set_attr(node,"value",value)
  cat(as.character(name),'=',signif(as.numeric(value),5),' ')
  xmldoc
}

mod_xml = function(df){
  i=1
  while (i<=nrow(df)){
    xmldoc = read.xml.file(df$source_file[i])
    xmldoc = modify.node.attr(xmldoc, df$xpath[i], df$attr_name[i], df$attr_value[i])
    while (is.na(df$target_file[i])){
      i = i + 1
      xmldoc = modify.node.attr(xmldoc, df$xpath[i], df$attr_name[i], df$attr_value[i])
    }
    write_xml(xmldoc,df$target_file[i])
    i = i + 1
  }
} 
