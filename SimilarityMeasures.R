

euclidean <- function(x,y,...){
  stopifnot(length(x)==length(y))
  d = sqrt(sum((x-y)^2))
  return(d)
}

inverse.euclidean <- function(x,y,...){
  stopifnot(length(x)==length(y))
  d = 1/euclidean(x,y)
  return(d)
}


manhattan <- function(x,y,...){
  stopifnot(length(x)==length(y))
  d = sum(abs(x-y))
  return(d)
}

inverse.manhattan <- function(x,y,...){
  stopifnot(length(x)==length(y))
  d = 1/manhattan(x,y)
  return(d)
}