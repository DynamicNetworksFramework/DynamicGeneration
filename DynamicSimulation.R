
prepare.dy.data <- function(data,pdatainicial,ndata=nrow(data)){
  initial.ndata = round(ndata*pdatainitial)
  
  dy.data = as.matrix(sample(1:ndata,initial.ndata,replace=F))
  dy.data = cbind(dy.data,data[dy.data,])
  return(dy.data)
}



add.object <- function(dy.d,data,ndata=nrow(data)){
  if(ndata==nrow(dy.d)){return(dy.d)}
  indexes.candidates = which(!c(1:ndata) %in% dy.d[,1])
  if(length(indexes.candidates)>=2){
    aux = sample(indexes.candidates,1)
    dy.d = rbind(dy.d,c(aux,data[aux,]))
  }else{
    aux = indexes.candidates
    dy.d = rbind(dy.d,c(aux,data[aux,]))
  }
  return(dy.d)
}


delete.object <- function(dy.d){
  if(nrow(dy.d)==1){return(dy.d)}
  dy.d = as.matrix(dy.d[-sample(1:nrow(dy.d),1),])
  return(dy.d)
}


trigger.change <- function(dy.d,data,ndata=nrow(data),padd = 0.5,pdelete = 0.5){
  pchange = c(padd,pdelete)
  pchange = pchange/sum(pchange)
  
  type.change = sample(c("add","delete"),1,prob=pchange)
  if(type.change=="add"){
    dy.d = add.object(dy.d,data)
  }else{
    if(type.change=="delete"){
      dy.d = delete.object(dy.d)
    }
  }
  return(dy.d)
}





