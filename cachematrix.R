## Thisfunction caches the Inverse of a matrix which helps to   
## avoid recalculation when the inverse is requested for the same 
##matrix again or calculate a new inverse if the matrix changes and caches the result in R's memory

## This function creates a  matrix and provides a function which set values,
## get values,set inverse & get inverse

makeCacheMatrix <- function(x = matrix()){
  inv <- NULL
  set <- function(y){
    x <<- y
    inv<<-NULL
  }
  get <- function(){x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## The below function will will look into cache to see if inverse exist for matrix 
## and then retrieve the value if its the same matrix however will calculate 
##if no inverse exist or matrix is not same

cacheSolve <- function(x,....){
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}