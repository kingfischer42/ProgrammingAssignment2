## These functions will first, create an empty matrix that gets filled with 
##values according to the matrix you pass in
## the next function then inverses the matrix created by the 1st one

## Creates the special matrix to be inverted

makeCacheMatrix <- function(x = matrix()) {
  
  var <- NULL
  
  ##sets the matrix
  setmat <- function(matrix) {
    x <<- matrix
    var <<- NULL
  }
  
  ##gets and returns the matrix
  getmat <- function() {
    x
  }
  
  ##sets the inverse
  setinv <- function(inv) {
    var <<- inv
  }
  
  ##gets the inverse 
  getinv <- function() {
    var
  }
  
  ##method list
  list(setmat = setmat, getmat = getmat, setinv = setinv, getinv = getinv)
}


## Computes the inverse of the matrix created above

cacheSolve <- function(x, ...) {
  var <- x$getinv()
  
  ##returns inverse if already set
  if (!is.null(var)) {
    return(var)
  }
  
  ##extracts matrix 
  dat <- x$getmat()
  
  ##assumes that the matrix is invertible and solves
  var <- solve(dat, ...)
  x$setinv(var)
  var
}
