## Convenience functions to manipulate a matrix which caches it's inverse

## build a wrapper around the matrix, which allows its inverse to be cached

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  get <- function() x
  setInv <- function(inverted) inv <<- inverted
  getInv <- function() inv
  
  list(set=set, get=get, setInv=setInv, getInv=getInv)
}


## Produces the inverse of 'x', using cached data if available

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  mtx <- x$get()
  inv <- solve(mtx)
  x$setInv(inv)
  
  inv
}
