## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  setMatrix <- function(y){
    x <<- y
    m <<- NULL
  }
  
  getMatrix <- function(){
    x
  }
  
  cacheInverse <- function(solve){
    m <<- solve
  }
  
  getInverse <- function(){
    m
  }
  
  list(setMatrix = setMatrix, getMatrix = getMatrix, cacheInverse = cacheInverse, getInverse = getInverse)
  
}


## This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        
  inverse <- x$getInverse()
  
  if(!is.null(inverse)){
    message("cached data is returned")
    return inverse
  }
  
  data <- x$getMatrix()
  inverse <- solve(data)
  x$cacheInverse(inverse)
  
  inverse
  
}
