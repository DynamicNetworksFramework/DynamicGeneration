require('igraph')

############################
# Main Functions           #
############################

generate.network <- function(dy.data, ndy.data = nrow(dy.data), similarity.function = inverse.euclidean){
  sim.m = similarity.matrix(dy.data[,-1],similarity.function)
  ep = calculate.epsilon(sim.m)
  stopifnot(ep!=Inf)
  adj.m = knn.epsilon.cut(sim.m,ep=ep)
  net = graph.adjacency(adj.m,mode="max",diag=F)
  V(net)$index = dy.data[,1]
  return(net)
}

add.vertice.network <- function(net,index.newdata,dy.data,
                               pnc = 0.1,
                               similarity.function = inverse.euclidean){
  
  n.neighbors.candidates = ceiling(vcount(net)*pnc)
  prob.degree = degree(net)/sum(degree(net))
  neighbors.candidates = sample(1:vcount(net),
                                size = n.neighbors.candidates,
                                replace = F, prob = prob.degree)
  
  net = add.vertices(net,1)
  V(net)[vcount(net)]$index = index.newdata
  
  neighbors.candidates.indexes = V(net)[neighbors.candidates]$index
  sim.neighbors = rep(Inf,n.neighbors.candidates)
  for(i in 1:n.neighbors.candidates){
    x = which(dy.data[,1]==index.newdata)
    y = which(dy.data[,1]==neighbors.candidates.indexes[i])
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
        aux.index = V(net)[v2]$index
        x = which(dy.data[,1]==index.newdata)
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
  
  if(msgDebug){
    cat("","New neighbors:",length(neighbors.newdata))
  }
  for(v in neighbors.newdata){
    net = add.edges(net, c(vcount(net),v))
    net = simplify(net)
  }
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
      next.neighbor = which.min(sim.m[i,neighbors.candidates])
      adj[i,next.neighbor] = adj[next.neighbor,i] = 1
    }
  }
  return(adj)
}

plot.net <- function(net, classes, aux.name, colored=T){
  nc = unique(classes[V(net)$index])
  colors = rainbow(length(nc))
  auxclasses = classes[V(net)$index]
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