## Below are two functions that create and cache the inverse of a matrix.  

## This first function "makeCacheMatrix, creates a matrix object for later inverse.

makeCacheMatrix <- function( m = matrix() ) {
     
     ## Initialize the inverse value, setting to NULL to begin
     i <- NULL
     
     ## Set the matrix, using <<- operator to assign a values outside the current environment
     #  Set i to NULL again to begin with an empty cache
     setMatrix <- function( matrix ) {
          m <<- matrix
          i <<- NULL
     }
     
     ## Get the matrix created from m object above
     getMatrix <- function() {
     # Return the matrix
          m
     }
     
     ## Method to set the inverse of the matrix
     setInverse <- function(solve) {
          i <<- solve
     }
     
     ## Method to get the inverse of the matrix
     getInverse <- function() {
          ## Return the inverse property
          i
     }
     
     ## Return a list of the methods
     list(setMatrix = setMatrix, getMatrix = getMatrix,
          setInverse = setInverse,
          getInverse = getInverse)
}


## Evaluate the inverse of the matrix created by "makeCacheMatrix" above.
## If the inverse has been determined, and there is not a new matrix, then the "cachesolve" 
## function should retrieve the chached inverse.

cacheSolve <- function(x, ...) {
     
     ## Return a matrix that is the inverse of 'x'
     m <- x$getInverse()
     
     ## If the cache contains the inverse values, return the cached values.
     if( !is.null(m) ) {
          message("getting cached data")
          return(m)
     }
     
     ## If the cache does not contain the inverse values, get the matrix, calculate inverse
     ## and store in the cache.
     
     # Get Matrix
     data <- x$getMatrix()
     
     ## Use Solve function for the inverse of the matrix.
     m <- solve(data)
     
     ## Set the inverse.
     x$setInverse(m)
     
     ## Return the inverse matrix
     m
}