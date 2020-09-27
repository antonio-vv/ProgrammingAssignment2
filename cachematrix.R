## This function construct a list of functions in order to get outputed the input matrix and to
## get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(get = get, setinv = setinv, getinv = getinv)
}


## This function verifies if the last function got the inverse of the matrix inputed and print it, otherwise
## the input matrix is returned to the 'makeCacheMatrix' function to solve the inverse.

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}