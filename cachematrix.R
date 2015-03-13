## This file provides two main functions:
## - makeCacheMatrix: creates an object that takes a matrix, and can cache the
##   inverse matrix
## - cacheSolve: uses the object given by makeCacheMatrix to compute the
##   inverse matrix
## Using these functions in loops is faster than calculating the inverse matrix
## in every loop.
## Usage example:
## m <- matrix(c(2, 1, 5, 3), 2, 2)
## cache_m <- makeCacheMatrix(m)
## cacheSolve(cache_m)
## cacheSolve(cache_m)
## The second "cacheSolve" will compute, the second will use the cached matrix.



## This function creates a "cached matrix" object that can be used to compute
## the inverse matrix faster by caching it. When the inverse matrix is needed
## several times (like in loops), it must not be computed in each loop, but
## the cached matrix can be used.
##
## Usage: makeCacheMatrix(some_matrix)

makeCacheMatrix <- function(x = matrix()) {
    ## define empty caching object for inverse matrix
    cachedInvMatrix <- NULL

    ## setter function: set the matrix, reset cached inverse matrix
    set <- function(newMatrix) {
        x <<- newMatrix
        cachedInvMatrix <<- NULL
    }

    ## getter function: returns the matrix
    get <- function() {
        return(x)
    }

    ## setter function: set the cached inverse matrix
    setInv <- function(invMatrix) {
        cachedInvMatrix <<- invMatrix
    }

    ## getter function: returns the cached inverse matrix
    getInv <- function() {
        return(cachedInvMatrix)
    }
    
    ## now create the "makeCacheMatrix" which is a list of functions
    return(list(set = set, get = get, setInv = setInv, getInv = getInv))
}


## This function solves an inverse matrix using the "caching" matrix object
## above. This means, it will use the cached inverse matrix if available.
## If no inverse matrix is available, it will compute the inverse matrix
## and cache it for future use.
##
## Usage: cacheSolve(matrix_to_solve, [maybe other parameters])

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    result <- x$getInv()
    ## return the cached matrix, if we have one
    if(!is.null(result)) {
        ## if we have a cached matrix, return it to the user
        message("getting cached data")
        return(result)
    } else {
        ## if we do not have a cached inverse matrix, then solve it
        tmpMatrix <- x$get()
        result <- solve(tmpMatrix, ...)
        ## cache result
        x$setInv(result)
        ## return result to user
        return(result)
    }
}
