## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    inv <<- NULL
    x <<- y
  }
  get <- function() x
  setInverse <- function(invs) inv <<- invs
  getInverse <- function() inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached inverse")
    return(inv)
  }
  
  mtrx <- x$get()
  inv <- solve(mtrx)
  x$setInverse(inv)
  inv
}
