## This function construct a list of functions in order to get outputed the input matrix and to
## get the inverse of this matrix.

makeCacheMatrix <- function(x = matrix()) {
  #Initialize the inverse object
  inv <- NULL
  #Get the input matrix as an object
  get <- function() x
  #Solve the inverse 0of the matrix
  setinv <- function(solve) inv <<- solve
  #Get the inverse of the matrix as an object
  getinv <- function() inv
  #Build the list with all previous functions
  list(get = get, setinv = setinv, getinv = getinv)
}


## This function verifies if the last function got the inverse of the matrix inputed and print it, otherwise
## the input matrix is returned to the 'makeCacheMatrix' function to solve the inverse.

cacheSolve <- function(x, ...) {
  #Initialize the inverse as an object with the value obtained with the previous function
  inv <- x$getinv()
  #Check if the inverse has benn already calculated
  if(!is.null(inv)) {
    #Print out the result obtained before
    message("getting cached inverse")
    return(inv)
  }
  #Get the original matrix
  data <- x$get()
  #Get the inverse of that matrix
  inv <- solve(data, ...)
  #Set the value forthe inverse of the matrix
  x$setinv(inv)
  #Print out the result
  inv
}