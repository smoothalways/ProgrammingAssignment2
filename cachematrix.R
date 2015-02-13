## Functions for computing the inverse of a matrix and caching it

## First function creates a special matrix object and return

makeCacheMatrix <- function(x = matrix()) {
  ## initialize variable to hold inverse
  inv <- NULL
  ## set the value of the matrix
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  ## get the value of the matrix
  get <- function() x
  ## calculate the inverse of the matrix and cache it
  setinv <- function(inverse) inv <<- inverse
  ## retrieve the inverse of the matrix
  getinv <- function() inv
  ## build list of above 4 functions for use externally
  list (set = set, get = get,
        setinv = setinv,
        getinv = getinv)
}

## Second function returns the inverse of the matrix
## If the cached inverse is available it returns it
## otherwise it computes the inverse, caches it and returns it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  ## check to see if inverse is already cached
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  ## calculate inverse of matrix
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
