## jorgevillamizarco assignment 2

## "makeCacheMatrix" and "cachesolve" are functions that create a matrix with cache and calculates matrix inverse 
## (must be a matrix created with makeCacheMatrix function) respectively.

## makeCacheMatrix function creates a special matrix with cached values and allows to get and set the matrix and its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## cachesolve function returns the inverse of a matrix of type "makeCacheMatrix". 
## If the inverse has already been calculated then the value is taken from cache.

cachesolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
    
}
