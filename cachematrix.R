## These functions allows a time consuming task to be avoided 
## when calculating an inverse of a matrix. It caches the inverse
## inside a makeCacheMatrix object.

## The makeCacheMatrix creates an object that carries out 
## a matrix and it's inverse. 
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inv) m <<- inv
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function verifies if the makeCacheMatrix object 
## already has the inverse cached. If not, it calculates
## the inverse matrix, stores and returns it.

cacheSolve <- function(x, ...) {
  m <- x$getInverse()
  if(!is.null(m)) {
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setInverse(m)
  m
}


