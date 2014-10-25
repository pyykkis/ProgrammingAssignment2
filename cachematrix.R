## Matrix inversion is a potentially costly computation.
## makeCacheMatrix creates a matrix with cached inverse attribute
## and cacheSolve efficiently solves matrixes by utilizing cache if inverse
## matrix is already computed.
##
## Functions assume that matrix is invertible.

## makeCacheMatrix
##
## Description
## Wraps matrix object to support cache for inverse computation
##
## Parameters
## x Matrix object to be wrapped. Default value: Empty matrix
makeCacheMatrix <- function(x = matrix()) {
    i   <- NULL
    get <- function() x
    set <- function(y) {
        # <<- searches x and y from parent environments instead of defining new variables
        x <<- y
        i <<- NULL
    }
    
    getinverse <- function() i
    setinverse <- function(inverse) i <<- inverse
    list(set = set, get = get,
         setinverse = setinverse, getinverse= getinverse)
}

## cacheSolve
##
## Description
## Solve matrix using cached result, if available. Otherwise, compute
## inverse using 'solve' function and cache the result.
##
## Parameters
## x    cacheMatrix to solve
## ...  Extra parameters passed to 'solve' function
cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    data <- x$get()
    i    <- solve(data, ...)
    
    x$setinverse(i)
    i
}
