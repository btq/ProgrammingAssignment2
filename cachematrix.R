## the following is a pair of functions that cache the inverse of a matrix

## Create a matrix and cache its inverse
# returns list of functions
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) s <<- solve
  getInverse <- function() s
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
# If the inverse has been calculated (and the matrix has not changed), then
# it should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # check if inverse is cached and load it if yes
  s <- x$getInverse()
  if(!is.null(s)) {
    message("getting cached inverse")
    return(s)
  }
  # get data, use solve to compute inverse, put in cache
  data <- x$get()
  s <- solve(data, ...)
  x$setInverse(s)
  s
}

