# This script is assigned to create and cache a matrix (and its inverse),
# which is done by two functions:
# 1: MakeCacheMatrix:   this function creates a special "matrix" object that can cache its inverse.
# 2: CacheSolve:        This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above. 
#                       If the inverse has already been calculated (and the matrix has not changed), then
#                       `cacheSolve` should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {

      # Initialize the inverse property
      inv <- NULL
      
      # Set the matrix
      set <- function( matrix ) {
            x <<- matrix
            inv <<- NULL
      }
      
      # Get the matrix
      get <- function() {
            ## Return the matrix
            x
      }
      
      # Set the inverse of the matrix
      setInverse <- function(inverse) {
            inv <<- solve(x) # This step calculates the inverse of the matrix
      }
      
      # Get the inverse of the matrix
      getInverse <- function() {
            # Return the inverse property
            inv
      }
      
      ## Return a list of the steps
      list(set = set, 
           get = get,
           setInverse = setInverse,
           getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
      # Return a matrix that is the inverse of 'x'
      
      # Return the inverse matrix of 'x'
      invM <- x$getInverse()
      
      # Return the inverse if it already exists 
      if( !is.null(invM) ) {
            message("getting cached data")
            return(invM)
      }
      
      # Get the matrix from the object
      data <- x$get()
      
      # Calculate the inverse using matrix multiplication
      invM <- solve(data)
      
      # Set the inverse to the object
      x$setInverse(invM)
      
      # Return the inverse matrix
      invM
}

