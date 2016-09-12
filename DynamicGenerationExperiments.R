rm(list = ls())
dirname = getSrcDirectory(function(x) {x})
setwd(dirname)
require('testthat')
source('Datasets.R')
source('SimilarityMeasures.R')
source('DynamicSimulation.R')
source('DynamicGeneration.R')
error.file = paste(getwd(),"/Errors.dat",sep="")

msgDebug = T
pdatainitial = 0.75

nseeds = 100
seedspool = sample(1:1e+05,nseeds,replace=F)

seed = seedspool[1]
set.seed(seed)
aux.names.datasets = names.datasets[1:3]

for(name in aux.names.datasets){
  data = prepare.data(name)
  classes = as.matrix(as.numeric(as.factor(data[[2]])))
  data = as.matrix(data[[1]])
  
  nclasses = length(unique(classes))
  ndata = nrow(data)
  if(msgDebug){
    cat("\nSeed:",seed)
    cat("\nDataset:",name)
    cat("\nndata:",ndata)
    cat("\nnclasses:",nclasses)
  }
  
  dy.data = prepare.dy.data(data,pdataincial)
  
  ndy.data = nrow(dy.data)
  ndy.classes = length(unique(classes[dy.data[,1],]))
  
  if(msgDebug){
    cat("\nndydata:",ndy.data)
    cat("\nndyclasses:",ndy.classes)
  }
  
  net = generate.network(dy.data)
  plot.net(net,classes,"before")
  aux = c(1:ndata)
  new.indexes = aux[!aux %in% dy.data[,1]]
  if(msgDebug){
    cat("\nNew Data:")
  }
  for(i in sample(new.indexes)){
    # if(msgDebug){
    #   cat("\n",i)
    # }
    dy.data = add.object(i,dy.data,data)
    time1 = system.time((net = add.vertex.network(net,i,dy.data)))
    time2 = system.time((net2 = generate.network(dy.data)))
    aux = infomap.community(net)
    aux2 = infomap.community(net2)
    result = compare(classes[dy.data[,1]],aux$membership,"nmi")
    result2 = compare(classes[dy.data[,1]],aux2$membership,"nmi")
    if(msgDebug){
      cat("","nmi1:",result)
      cat("","nmi2:",result2)
      cat("","tempo1:",as.numeric(time1[3]))
      cat("","tempo2:",as.numeric(time2[3]))    
    }
  }
  plot.net(net,classes,"after")
}
