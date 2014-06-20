
## Function makeCacheMatrix 
##
## Purpose: 
##  Create a special "matrix" object that can cache its inverse.
##
## On Entry:
##  x - matrix
##
## One Exit:
##  returns an object that sotres a numeric vector and caches it inverse
##
##
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}

## Function cacheSolve
##
## Purpose:
##  Computes the inverse of the special "matrix" returned by makeCacheMatrix.
##  If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
##  should retrieve the inverse from the cache.
## 
## On Entry:
##  Params: x matrix
##          '...' to pass through to solve
## On Exit:
##  Returns a matrix that is the inverse of 'x'
##
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- inverse(data, ...)
  x$setinverse(m)
  m
}
