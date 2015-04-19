##
## This file contains a pair of functions able to create, solve, and cache the
## inverse of a matrix.
##

##
## This function creates a "special" matrix (wrapping the passed matrix), that
## can be used by the function below to calculate and cache its inverse.  This
## "special" matrix is really a list containing functions to:
## 
## (1) set the value of a matrix
## (2) get the value of a matrix
## (3) set the value of the inverse of the matrix
## (4) get the value of the inverse of the matrix
##
makeCacheMatrix <- function(x = matrix())
{
     ## Create a place-holder for the inverse of the passed matrix.
     INV <- NULL

     ## A function to set the value of the matix.
     set <- function(y) {
         MTX <<- y
         INV <<- NULL
     }
     set(x)
     ## A function to get the value of the matrix.
     get <- function() {
         return(MTX)
     }
     ## A function to set the inverse of the matrix.
     setInverse <- function(y) {
         INV <<- y
     }
     ## A function to get the inverse of the matrix.
     getInverse <- function() {
         return(INV)
     }
     ## A list containing methods to get & set the value of the matrix,
     ## and get & set the value of its inverse.
     list(set = set,
          get = get,
          setInverse = setInverse,
          getInverse = getInverse)
}

##
## This function calculates and returns the inverse of a "special" matrix 
## created from the above function.  It first checks to see if the inverse has
## already been calculated, an if so, fetches the inverse from the cache and
## skips the computation.
##
cacheSolve <- function(x, ...)
{
    ## Fetch the inverse of the passed matrix from its cache.
    INV <- x$getInverse()
    
    ## If the caches inverse exists, then return it.
    if(!is.null(INV))
    {
        message("fetching cached data")
        return(INV)
    }
    ## Otherwise, calculate the inverse of the passed matrix.
    data <- x$get()
    INV <- solve(data, ...)
    ## And cache the result for future use.
    x$setInverse(INV)
    return(INV)
}
