## makeCacheMatrix creates a list of functions for cacheSolve to take in as an
## argument. makeCacheMatrix takes in a square inversible matrix. 
## cacheSolve checks if there is a cached matrix inversion in makeCacheMatrix,
## if not, it does it itself.

## takes in a square inversible matrix, outputs a list that stores a free variable. 

makeCacheMatrix <- function(x = diag()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Takes the list from makeCacheMatrix, and solves (inverses) the matrix unless the
## matrix has already been inversed and stored in the free variable (inv in this case).

cacheSolve <- function(x, ...) {
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  else
    data <- x$get()
  inv <- solve(data, ...)
  
  x$setinv(inv)
  return(inv)
  ## Return a matrix that is the inverse of 'x'
}
