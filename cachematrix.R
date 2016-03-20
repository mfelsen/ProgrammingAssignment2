## Caching the Inverse of a Matrix
## Contains 4 member functions: set, get, setInv and getInv

## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
         inv <- NULL
         set <- function(y) {
                 x <<- y
                inv <<- NULL
         			    }
         get <- function() x
         setInverse <- function(inverse) inv <<- inverse
         getInverse <- function() inv
         list(set = set,
              get = get,
              setInverse = setInverse,
              getInverse = getInverse)
 }


## Computes the inverse of the special "matrix" created by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 
         inv <- x$getInverse()
         if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        			    }
         mat <- x$get()
         inv <- solve(mat, ...)
         x$setInverse(inv)
         inv
 }



