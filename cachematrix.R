## The following functions cache the inverse of a matrix.


## This function creates a matrix object that caches the matrix's inverse.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL

  # set the matrix for which the inverse will be computed
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }

  # get the matrix for which the inverse will be computed
  get <- function() {
    x
  }

  # set the inverse of the matrix once it's computed
  setinverse <- function(inverse) {
    inv <<- inverse
  }

  # get the cached inverse of the matrix
  getinverse <- function() {
    inv
  }

  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the matrix object created by
## makeCacheMatrix unless the result has been cached, in which case
## the result is retrieved from the cache.
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()

  # the cache contains the inverse - retrieve and return it
  if (!is.null(inv)) {
    message("retrieving cached inverse")
    return(inv)
  }

  # the cache does not contain the inverse - compute, cache, and return it
  matrix <- x$get()
  inv <- solve(matrix)
  x$setinverse(inv)
  inv
}