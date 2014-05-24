## This file consists of a pair of functions that allow for the efficient
## retrieval of inverted matrices. makeCacheMatrix takes a matrix as an 
## argument, and creates from it a special "matrix" object that is able to
## cache its inverse. cacheSolve takes one of these special objects as an
## argument and returns its inverse. If the inverse has already been
## calculated, the cached inverse is returned; otherwise, the inverse is
## calculated, cached, and returned.
##

## DESCRIPTION
## Creates a special "matrix" object that is able to cache its inverse.
## The object returned is a list containing functions to
## - set the value of the matrix ("set")
## - get the value of the matrix ("get")
## - set the value of the inverse ("setinverse")
## - get the value of the inverse ("getinverse")
## 
## EXAMPLE
## makeCacheMatrix(x)   # Creates an object from the invertible matrix x
##                      # that is capable of caching its own inverse
##
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## DESCRIPTION
## cacheSolve takes an object constructed by makeCacheMatrix as an
## argument and returns its inverse. If the inverse has already been
## calculated, the cached inverse is returned; otherwise, the inverse is
## calculated, cached, and returned.
##
## EXAMPLE
## cacheSolve(x)    # x should be an object that was created by makeCacheMatrix
##
cacheSolve <- function(x, ...) {
    
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("Getting cached data.")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
