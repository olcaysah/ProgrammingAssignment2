## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## A pair of functions that cache the inverse of a matrix
## Creates a matrix object that can cache its inverse
makeCacheMatrix <- function( m = matrix() ) {
  ## Initialize the inverse property
  i <- NULL
  ## Set the matrix
  setmatrix <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  ## Get the matrix
  getmatrix <- function() {
    ## Return the matrix
    m
  }
  ## Set the inverse of the matrix
  setInversematrix <- function(inverse) {
    i <<- inverse
  }
  ## Get the inverse of the matrix
  getInversematrix <- function() {
    ## Return the inverse property
    i
  }
  ## Return a list of the methods
  list(setmatrix = setmatrix,
       getmatrix = getmatrix,
       setInversematrix = setInversematrix,
       getInversematrix = getInversematrix)
}
## Write a short comment describing this function
## Compute the inverse of the special matrix returned by "makeCacheMatrix"
## above. If the inverse has already been calculated (and the matrix has not
## changed), then the "cachesolve" should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInversematrix()
  ## Return the inverse if its already set
  if( !is.null(m) ) {
    message("getting cached data")
    return(m)
  }
  ## Get the matrix from the object
  data <- x$getmatrix()
  ## Calculate the inverse using matrix multiplication
  m <- solve(data) %*% data
  ## Set the inverse to the object
  x$setInversematrix(m)
  ## Return the matrix
  m
}
