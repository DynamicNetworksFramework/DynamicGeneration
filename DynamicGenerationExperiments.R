rm(list = ls())
dirname = getSrcDirectory(function(x) {x})
setwd(dirname)
require('testthat')
source('Datasets.R')
source('DynamicSimulation.R')
source('DynamicGeneration.R')
error.file = paste(getwd(),"/Errors.dat",sep="")

msgDebug = T
pdatainitial = 0.5

nseeds = 100
seedspool = sample(1:1e+05,nseeds,replace=F)

seed = seedspool[1]
set.seed(seed)
name = sample(names.datasets,1)

data = prepare.data(name)
classes = as.matrix(data[[2]])
data = as.matrix(data[[1]])

nclasses = length(unique(classes))
ndata = nrow(data)
if(msgDebug){
  cat("\nSeed:",seed)
  cat("\nDataset:",name)
  cat("\nndata:",ndata)
  cat("\nnclasses:",nclasses)
}

dy.data = prepare.dy.data(ndata,pdataincial)

ndydata = nrow(dy.data)
ndyclasses = length(unique(classes[dy.data,]))

if(msgDebug){
  cat("\nndydata:",ndydata)
  cat("\nndyclasses:",ndyclasses)
}