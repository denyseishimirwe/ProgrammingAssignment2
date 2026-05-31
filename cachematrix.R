## These two functions cache the inverse of a matrix so that the
## potentially costly computation of matrix inversion is only done
## once and reused on subsequent calls.


## makeCacheMatrix creates a special "matrix" object that can cache its
## inverse. It returns a list of functions to set/get the matrix and
## set/get its cached inverse.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of the special "matrix" created by
## makeCacheMatrix. If the inverse is already cached it returns the
## cached value; otherwise it computes the inverse, caches it, and
## returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}