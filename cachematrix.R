## Functions that cache the inverse of a matrix

### Matrix inversion is usually a costly computation and there may be some 
### benefit to caching the inverse of a matrix rather than computing it repeatedly

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      
      # Variable for matrix inverse
      minv <- NULL
      
      # Set matrix value
      setMatrix <- function(m){
            x <<- m
            minv <<- NULL
      }
      
      # Get matrix value
      getMatrix <- function() {
            x
      }
      
      # Set inverse of the matrix
      setMatrixInverse <- function(inv) {
            minv <<- inv
      }
      
      # Get inverse of the matrix
      getMatrixInverse <- function() {
            minv
      }
      
      list(set = setMatrix, get = getMatrix,
           setInverse = setMatrixInverse, getInverse = getMatrixInverse)
      
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above, retrieving the result from cache when available.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      
      ## If matrix inverse is already computed, get it from cache
      inversedMatrix <- x$getInverse()
      
      if(!is.null(inversedMatrix)) {
            message("getting cache data")
            inversedMatrix
      } else {
            ## Otherwise, compute inverse matrix and set it in matrix created
            ## with makeCacheMatrix
            inv <- solve(x$get())
            x$setInverse(inv)
            inv
      }
      
}
