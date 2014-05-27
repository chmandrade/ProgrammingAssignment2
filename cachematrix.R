## Overview:
##
## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than compute it repeatedly.
##
## These functions enable caching of the inverse of a square invertible matrix.
## Function:  makeCacheMatrix(x), where x is a matrix
##
## This function creates a special "matrix" object that can cache
## its inverse.
## 

makeCacheMatrix <- function(x = matrix()) {
  h <- NULL
  set <- function(y) {
    x <<- y
    h <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) h <<- solve
  getsolve <- function() h
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## Function:  solveCache(x), where x is a matrix
##
## This function calculates the inverse of the special "matrix"
## returned by `makeCacheMatrix` above. If the inverse has
## already been calculated (and the matrix has not changed), then
## `cacheSolve` will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  h <- x$getsolve()
  
  if(!is.null(h)) {
    message("getting cached data")
    return(h)
  }
  
  data <- x$get()
  h <- solve(data, ...)
  x$setsolve(h)
  h
}
