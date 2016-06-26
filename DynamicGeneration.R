require('igraph')

############################
# Main Functions           #
############################

generate.network <- function(data, ndata = nrow(data), data.index = c(1:ndata),similarity.function = inverse.euclidean){
  g = make_empty_graph(0,F)
  for(i in data.index){
    g = add.vertices(g,1)
    V(g)[vcount(g)]$index = i
  }
  
}

################################
# Auxiliar Functions           #
################################
