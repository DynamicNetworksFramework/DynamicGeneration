
names.datasets = c("glass","iris","wine","yeast","ecoli","breast","leaf","seeds","parkinsons","ionosphere")

prepare.data <- function(name){
  stopifnot(dir.exists("Datasets"))
  data = switch(EXPR = name,
         "glass" = glass(),
         "iris" = iris(),
         "wine" = wine(),
         "yeast" = yeast(),
         "ecoli" = ecoli(),
         "breast" = breast(),
         "leaf" = leaf(),
         "seeds" = seeds(),
         "parkinsons" = parkinsons(),
         "ionosphere" = ionosphere())
  return(data)
}

glass <- function(){
  data = read.table("Datasets/glass.data.txt",sep=",")
  classes = data[,11]
  data = data[,2:10]
  return(list(data,classes))
}

iris <- function(){
  data = read.table("Datasets/iris.data.txt",sep=",")
  classes = data[,5]
  data = data[,1:4]
  return(list(data,classes))
}

wine <- function(){
  data = read.table("Datasets/wine.data.txt",sep=",")
  classes = data[,1]
  data = data[,2:13]
  return(list(data,classes))
}

yeast <- function(){
  data = read.table("Datasets/yeast.data.txt")
  classes = data[,10]
  data = data[,2:9]
  return(list(data,classes))
}

ecoli <- function(){
  data = read.table("Datasets/ecoli.data.txt")
  classes = data[,9]
  data = data[,2:8]
  return(list(data,classes))
}

breast <- function(){
  data = read.table("Datasets/wdbc.data.txt",sep=",")
  classes = data[,2]
  data = data[,3:30]
  return(list(data,classes))
}

leaf <- function(){
  data = read.csv("Datasets/leaf.csv")
  classes = data[,1]
  data = data[,3:16]
  return(list(data,classes))
}

seeds <- function(){
  data = read.table("Datasets/seeds_dataset.txt")
  classes = data[,8]
  data = data[,1:7]
  return(list(data,classes))
}

parkinsons <- function(){
  data = read.table("Datasets/parkinsons.data.txt",sep=",")
  classes = data[,18]
  data = data[,c(2:17,19:24)]
  return(list(data,classes))
}

ionosphere <- function(){
  data = read.table("Datasets/ionosphere.data.txt",sep=",")
  classes = data[,35]
  data = data[,c(1,3:34)]
  return(list(data,classes))
}