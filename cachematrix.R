## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
