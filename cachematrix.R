## Put comments here that give an overall description of what your
## functions do

## Function 'makeCacheMatrix' - This function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function 'cacheSolve' - This function computes the inverse of the
## matrix return by 'makeCacheMatrix'. If the inverse has already
## been calculated, and not changed, then it retrieves the inverse
## from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if (!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat, ...)
  x$setInverse(m)
  m
}
