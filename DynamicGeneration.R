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
                               pnc = 0.05,
                               similarity.function = inverse.euclidean){
  
  net = add.vertices(net,1)
  V(net)[vcount(net)]$pk = pk.newdata
  
  neighbors.newdata = pick.new.neighbors(net=net, pk.newdata = pk.newdata, 
                                         dy.data = dy.data, pnc=pnc,
                                         similarity.function = similarity.function)
  
  # if(msgDebug){
  #   cat("","New neighbors:",length(neighbors.newdata))
  # }
  for(v in neighbors.newdata){
    net = add.edges(net, c(vcount(net),v))
    net = simplify(net)
  }
  return(net)
}


pick.new.neighbors <- function(net,pk.newdata, dy.data,
                                similarity.function = inverse.euclidean,
                                pnc = 0.1, pdegree = 0.1){
  
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
  visited = neighbors.candidates
  base.sim = mean(sim.neighbors)
  min.degree = round(mean(degree(net)))
  max.degree = round(mean(degree(net))*(1+pdegree))
  newdata.degree = sample(min.degree:max.degree,1)
  
  current.vertex = sample(neighbors.candidates,1,replace=F,prob=prob.sim)
  current.vertex.neighbors = neighbors(net,current.vertex)
  # neighbors.candidates = c()
  # sim.neighbors = c()
  
  while(length(neighbors.candidates)>0){
    visited = c(visited,current.vertex)
    neighbors.newdata = c(neighbors.newdata,current.vertex)
    
    # if(msgDebug){
    #   #cat("\nCurrent:",current.vertex)
    #   #cat("\nCurrent neigh:",current.vertex.neighbors)
    #   #cat("\nVisited:",length(visited))
    #   #cat("\nCandidates:",neighbors.candidates)
    #   #cat("\nNew neighbors:",neighbors.newdata)
    #   #cat("\nSim min:",base.sim)
    # }
    if(length(current.vertex.neighbors)>0){
      for(i in 1:length(current.vertex.neighbors)){
        visited = c(visited,current.vertex.neighbors[i])
        aux.pk = current.vertex.neighbors[i]
        aux.pk = V(net)[aux.pk]$pk
        y = which(dy.data[,1]==aux.pk)
        aux.sim = similarity.function(dy.data[x,-1],dy.data[y,-1])
        if (aux.sim >= base.sim){
          neighbors.newdata = c(neighbors.newdata,current.vertex.neighbors[i])
        }else{
          neighbors.candidates = c(neighbors.candidates, current.vertex.neighbors[i])
          sim.neighbors = c(sim.neighbors,aux.sim)
        }
      }
    }
    
    sim.neighbors[sim.neighbors==Inf] = max(sim.neighbors[sim.neighbors!=Inf])
    
    prob.sim = sim.neighbors/(sum(sim.neighbors))
    
    if(length(neighbors.candidates)>1){
      current.vertex = sample(neighbors.candidates,1,prob=prob.sim)
      current.vertex.id = which(neighbors.candidates==current.vertex)
      neighbors.candidates = neighbors.candidates[-current.vertex.id]
      sim.neighbors = sim.neighbors[-current.vertex.id]
    }else{
      current.vertex = neighbors.candidates
      neighbors.candidates = c()
    }
    
    base.sim = mean(sim.neighbors)
    current.vertex.neighbors = neighbors(net,current.vertex)
    current.vertex.neighbors = current.vertex.neighbors[!current.vertex.neighbors %in% visited]
    visited = unique(visited)
    
    if(length(neighbors.newdata)>=newdata.degree){
      neighbors.candidates = c()
    }
  }
  
  return(unique(neighbors.newdata))
  
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