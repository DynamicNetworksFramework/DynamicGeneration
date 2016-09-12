require('igraph')

############################
# Main Functions           #
############################

generate.network <- function(dy.data, ndy.data = nrow(dy.data), 
                             similarity.function = inverse.euclidean){
  sim.m = similarity.matrix(dy.data[,-1],similarity.function)
  ep = calculate.epsilon(sim.m)
  stopifnot(ep!=Inf)
  adj.m = knn.epsilon.cut(sim.m,ep=ep)
  net = graph.adjacency(adj.m,mode="max",diag=F)
  
  V(net)$pk = dy.data[,1]
  return(net)
}

add.vertex.network <- function(net,pk.newdata,dy.data,
                               pnc = 0.1,
                               similarity.function = inverse.euclidean){
  
  net = add.vertices(net,1)
  V(net)[vcount(net)]$pk = pk.newdata
  
  neighbors.newdata = pick.new.neighbors3(net=net, pk.newdata = pk.newdata, 
                                         dy.data = dy.data, pnc=pnc,
                                         similarity.function = similarity.function)
  
  if(msgDebug){
    cat("","New neighbors:",length(neighbors.newdata))
  }
  for(v in neighbors.newdata){
    net = add.edges(net, c(vcount(net),v))
    net = simplify(net)
  }
  return(net)
}

pick.new.neighbors <- function(net,pk.newdata,dy.data,
                               pnc = 0.1,
                               similarity.function = inverse.euclidean){
  
  #Firsts neighbors candidates, by degree
  n.neighbors.candidates = ceiling(vcount(net)*pnc)
  prob.degree = degree(net)/sum(degree(net))
  neighbors.candidates = sample(1:vcount(net),
                                size = n.neighbors.candidates,
                                replace = F, prob = prob.degree)
  
  neighbors.candidates.pks = V(net)[neighbors.candidates]$pk
  sim.neighbors = rep(Inf,n.neighbors.candidates)
  
  x = which(dy.data[,1]==index.newdata)
  for(i in 1:n.neighbors.candidates){
    y = which(dy.data[,1]==neighbors.candidates.pks[i])
    sim.neighbors[i] = similarity.function(dy.data[x,-1],
                                           dy.data[y,-1])
    
  }
  
  sim.neighbors[sim.neighbors==Inf] = max(sim.neighbors[sim.neighbors!=Inf])
  prob.sim = sim.neighbors/sum(sim.neighbors[sim.neighbors!=Inf])
  
  neighbors.newdata = c()
  
  #base.sim = sim.neighbors[which(neighbors.candidates==neighbors.newdata)]
  base.sim = mean(sim.neighbors)
  visited = c()
  while(length(neighbors.newdata)<n.neighbors.candidates){
    neighbors.newdata = c(neighbors.newdata,
                          sample(neighbors.candidates,1,
                                 replace=F,prob=prob.sim))
    for(v in neighbors.newdata[!neighbors.newdata %in% visited]){
      visited = c(visited,v)
      for(v2 in neighbors(net,v)){
        aux.index = V(net)[v2]$pk
        y = which(dy.data[,1]==aux.index)
        aux.sim = similarity.function(dy.data[x,-1],
                                      dy.data[y,-1])
        if(aux.sim >= base.sim){
          neighbors.newdata = c(neighbors.newdata,v2)
        }
      }
      neighbors.newdata = unique(neighbors.newdata)
    }
  }
  
  return(neighbors.newdata)
}

pick.new.neighbors2 <- function(net,pk.newdata,dy.data,
                               pnc = 0.1,
                               similarity.function = inverse.euclidean,
                               pdegree = 0.1){
  
  #Firsts neighbors candidates, by degree
  n.neighbors.candidates = ceiling(vcount(net)*pnc)
  prob.degree = degree(net)/sum(degree(net))
  neighbors.candidates = sample(1:vcount(net),
                                size = n.neighbors.candidates,
                                replace = F, prob = prob.degree)
  
  neighbors.candidates.pks = V(net)[neighbors.candidates]$pk
  sim.neighbors = rep(Inf,n.neighbors.candidates)
  
  x = which(dy.data[,1]==index.newdata)
  for(i in 1:n.neighbors.candidates){
    y = which(dy.data[,1]==neighbors.candidates.pks[i])
    sim.neighbors[i] = similarity.function(dy.data[x,-1],
                                           dy.data[y,-1])
    
  }
  
  sim.neighbors[sim.neighbors==Inf] = max(sim.neighbors[sim.neighbors!=Inf])
  prob.sim = sim.neighbors/sum(sim.neighbors[sim.neighbors!=Inf])
  
  neighbors.newdata = c()
  visited = c()
  
  min.degree = round(mean(degree(net))*(1-pdegree))
  max.degree = round(mean(degree(net))*(1+pdegree))
  newdata.degree = sample(min.degree:max.degree,1)
  
  
  current.vertex = sample(neighbors.candidates,1,replace=F,prob=prob.sim)
  current.vertex.neighbors = neighbors(net,current.vertex)
  
  while(length(current.vertex.neighbors)>0){
    visited = c(visited,current.vertex)
    aux.pk = V(net)[current.vertex]$pk
    y = which(dy.data[,1]==aux.pk)
    aux.sim = similarity.function(dy.data[x,-1],
                                  dy.data[y,-1])
    aux.mean.sim = c()
    for(i in 1:length(current.vertex.neighbors)){
      aux.pk = current.vertex.neighbors[i]
      aux.pk = V(net)[aux.pk]$pk
      y = which(dy.data[,1]==aux.pk)
      aux.mean.sim = c(aux.mean.sim,similarity.function(dy.data[x,-1],dy.data[y,-1]))
    }
    if(aux.sim>mean(aux.mean.sim)){
      neighbors.newdata = c(neighbors.newdata,current.vertex)
    }
    
    neighbors.candidates = c(current.vertex,current.vertex.neighbors)
    prob.sim = c(aux.sim,aux.mean.sim)
    prob.sim = prob.sim/sum(prob.sim)
    
    current.vertex = sample(neighbors.candidates,1,prob=prob.sim)
    current.vertex.neighbors = neighbors(net,current.vertex)
    current.vertex.neighbors = current.vertex.neighbors[!current.vertex.neighbors %in% visited]
    
    if(length(neighbors.newdata)>=newdata.degree){
      current.vertex.neighbors = c()
      if(msgDebug){
        cat("","max degree!")
      }
    }
  }
  
  
  return(neighbors.newdata)
}

pick.new.neighbors3 <- function(net,pk.newdata,dy.data,
                                pnc = 0.1,
                                similarity.function = inverse.euclidean,
                                pdegree = 0.1){
  
  #Firsts neighbors candidates, by degree
  n.neighbors.candidates = ceiling(vcount(net)*pnc)
  prob.degree = degree(net)/sum(degree(net))
  neighbors.candidates = sample(1:vcount(net),
                                size = n.neighbors.candidates,
                                replace = F, prob = prob.degree)
  
  neighbors.candidates.pks = V(net)[neighbors.candidates]$pk
  sim.neighbors = rep(Inf,n.neighbors.candidates)
  
  x = which(dy.data[,1]==pk.newdata)
  for(i in 1:n.neighbors.candidates){
    y = which(dy.data[,1]==neighbors.candidates.pks[i])
    sim.neighbors[i] = similarity.function(dy.data[x,-1],
                                           dy.data[y,-1])
    
  }
  
  sim.neighbors[sim.neighbors==Inf] = max(sim.neighbors[sim.neighbors!=Inf])
  prob.sim = sim.neighbors/sum(sim.neighbors[sim.neighbors!=Inf])
  
  neighbors.newdata = c()
  visited = c()
  base.sim = mean(sim.neighbors)
  min.degree = round(mean(degree(net)))
  max.degree = round(mean(degree(net))*(1+pdegree))
  newdata.degree = sample(min.degree:max.degree,1)
  
  
  current.vertex = sample(neighbors.candidates,1,replace=F,prob=prob.sim)
  current.vertex.neighbors = neighbors(net,current.vertex)
  
  while(length(current.vertex.neighbors)>0){
    visited = c(visited,current.vertex)
    aux.pk = V(net)[current.vertex]$pk
    y = which(dy.data[,1]==aux.pk)
    aux.sim = similarity.function(dy.data[x,-1],
                                  dy.data[y,-1])
    
    if(aux.sim>=base.sim){
      neighbors.newdata = c(neighbors.newdata,current.vertex)
    }
    
    aux.neighbors.sim = c()
    for(i in 1:length(current.vertex.neighbors)){
      aux.pk = current.vertex.neighbors[i]
      aux.pk = V(net)[aux.pk]$pk
      y = which(dy.data[,1]==aux.pk)
      aux.neighbors.sim = c(aux.neighbors.sim,similarity.function(dy.data[x,-1],dy.data[y,-1]))
    }
    
    aux.neighbors.sim[aux.neighbors.sim==Inf] = max(aux.neighbors.sim[aux.neighbors.sim!=Inf])
    prob.sim = aux.neighbors.sim/sum(aux.neighbors.sim)
    # if(msgDebug){
    #   cat("\nvizi:",length(current.vertex.neighbors))
    #   cat("\nprob:",length(prob.sim))
    # }
    
    if(length(current.vertex.neighbors)>1){
      current.vertex = sample(current.vertex.neighbors,1,prob=prob.sim)
    }else{
      current.vertex = current.vertex.neighbors
    }
    current.vertex.neighbors = neighbors(net,current.vertex)
    current.vertex.neighbors = current.vertex.neighbors[!current.vertex.neighbors %in% visited]
    
    if(length(neighbors.newdata)>=newdata.degree){
      current.vertex.neighbors = c()
    }
  }
  
  
  return(neighbors.newdata)
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
  return(mean(sim.m[sim.m != Inf])/p.ep)
}

knn.epsilon.cut <- function(sim.m,k=1,ep){
  nlines = nrow(sim.m)
  adj = sim.m - ep
  adj[adj>0] = 1
  adj[adj<=0] = 0
  for(i in 1:nlines){
    for(j in 1:k){
      neighbors.candidates = which(adj[i,]==0)
      neighbors.candidates = neighbors.candidates[-which(neighbors.candidates==i)]
      next.neighbor = which.max(sim.m[i,neighbors.candidates])
      adj[i,next.neighbor] = adj[next.neighbor,i] = 1
    }
  }
  return(adj)
}

plot.net <- function(net, classes, aux.name, colored=T){
  nc = unique(classes[V(net)$pk])
  colors = rainbow(length(nc))
  auxclasses = classes[V(net)$pk]
  if(colored){
    for(i in nc){
      aux = which(auxclasses == i)
      V(net)[aux]$color = colors[which(nc==i)]
    }
  }
  filename = paste(name,aux.name,"net.png",sep="")
  png(filename=filename)
  plot.igraph(net)
  dev.off()
}