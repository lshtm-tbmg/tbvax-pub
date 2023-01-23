library(data.table)

select.and.convert.contact.matrix = function(dt,iso3=NULL,sett="overall",location="all"){
  seldt=dt[iso3c==iso3 & location_contact == location & setting==sett]
  stopifnot(nrow(seldt)==256)
  mat = dcast(seldt,age_contactor~age_cotactee,value.var="mean_number_of_contacts")
  names(mat)[2:17] = as.integer(substr(names(mat)[2:17],1,2))
  rownames(mat)=as.integer(substr(mat$age_contactor[1:16],1,2))
  mat=mat[,-1]
  z = mat[order(as.integer(rownames(matok))),order(as.integer(names(matok)))]
  names(z)=paste0("A",names(z))
  z
}
path = "./countries/contact_matrices/"
dt=as.data.table(read.csv(paste0(path,"synthetic_contacts_2020.csv")))
for (iso3 in unique(dt$iso3c)){
  x = select.and.convert.contact.matrix(dt,iso3)
  write.table(x,file=paste0(path,iso3,"_all_contacts_2020.txt"),quote = F,row.names = F, sep="\t")
}
