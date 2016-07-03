require('igraph')

############################
# Main Functions           #
############################

generate.network <- function(dy.data, ndy.data = nrow(dy.data), similarity.function = inverse.euclidean){
  sim.m = similarity.matrix(dy.data[,-1],similarity.function)
  ep = calculate.epsilon(sim.m)
  adj.m = knn.epsilon.cut(sim.m,ep=ep)
  net = graph.adjacency(adj.m,mode="max",diag=F)
  V(net)$index = dy.data[,1]
  return(net)
}

################################
# Auxiliar Functions           #
################################

similarity.matrix <- function(data,similarity.function){
  nlines = nrow(data)
  sim.m = matrix(0,nrow=nlines,ncol=nlines)
  for(i in 1:(nlines-1)){
    for(j in (i+1):nlines){
      sim.m[i,j] = sim.m[j,i] = similarity.function(data[i,],data[j,])
    }
  }
  return(sim.m)
}

calculate.epsilon <- function(sim.m,p.ep=0.7){
  return(mean(sim.m)/p.ep)
}

knn.epsilon.cut <- function(sim.m,k=1,ep){
  nlines = nrow(sim.m)
  adj = sim.m - ep
  adj[adj>0] = 1
  adj[adj<=0] = 0
  for(i in 1:nlines){
    for(j in 1:k){
      neighbors.candidates = which(adj[i,]==0)
      next.neighbor = which.min(sim.m[i,neighbors.candidates])
      adj[i,next.neighbor] = adj[next.neighbor,i] = 1
    }
  }
  return(adj)
}


plot.net <- function(net, classes, colored=T){
  nc = unique(classes[V(net)$index])
  colors = rainbow(length(nc))
  auxclasses = classes[V(net)$index]
  if(colored){
    for(i in nc){
      aux = which(auxclasses == i)
      V(net)[aux]$color = colors[which(nc==i)]
    }
  }
  filename = paste(name,"net.png",sep="")
  png(filename=filename)
  plot.igraph(net)
  dev.off()
}