## Put comments here that give an overall description of what your
## functions do

## Very simple cache manager.  Invalidates cache on setting new matrix.

makeCacheMatrix <- function(x = matrix()) {
  cachedInverse <- NULL
  
  set <- function (xprime) {
    x <<- xprime
    cachedInverse <<- NULL
  }
  
  get <- function() x
  
  setinverse <- function (inverse) cachedInverse <<- inverse
  getinverse <- function () cachedInverse
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Simple caching/memoizing function to calculate inverse of matrices.
## Usage:
## mtrx <- matrix(c(4, 3, 3, 2), nrow = 2, ncol = 2)
## cmtrx <- makeCacheMatrix(mtrx)
## imtrx <- cacheSolve(cmtrx)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    return (inv) 
  }
  
  inv <- solve(x$get())
  x$setinverse(inv)
  return (inv)
}
