## Cache the result of a inversed matrix, so it can be used again for an equal
## matrix without any calculations.

## makeCacheMatrix has a set of functions to store an matrix and inversed matrix 
## objects on an environment different (not current), in order to cache it.

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


## cacheSolve function verifies if the matrix have already been inversed, if so 
## it will use the cached object, if not it will inverse, cache and return 
## the inversed matrix.

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}