## The two functions below craete, calculate, and cache the inverse of a matrix.


## This function creates a matrix and caches its inverse.

makeCacheMatrix <- function(myMatrix = matrix()) {
        cachedMatrixInverse <- NULL
        set <- function(y) {
                myMatrix <<- y                                      # superassign 1) the new matrix's value and 2) cached value to NULL
                cachedMatrixInverse <<- NULL
        }
        get <- function() myMatrix                                  # return the matrix supplied to the makeCacheMatrix function
        setInverse <- function(solve) cachedMatrixInverse <<- solve # compute the inverse and superassign it
        getInverse <- function() cachedMatrixInverse                # return the cached inverse
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)       
}


## This function computes the inverse of the matrix returned by 
## makeCacheMatrix. If it the inverse value has already been cached,
## cacheSolve returns the cached value instead of performing the calculation.

cacheSolve <- function(myMatrix) {
        
        cachedMatrixInverse <- myMatrix$getInverse() 
        if(!is.null(cachedMatrixInverse)) {          # check to see if the inverse is cached
                message("getting cached data")       # if there's a cached value, return it
                return(cachedMatrixInverse)
        }
        data <- myMatrix$get()                       
        cachedMatrixInverse <- solve(data)           # if there's not a cached value, compute inverse
        myMatrix$setInverse(cachedMatrixInverse)     # store that cached inverse
        cachedMatrixInverse
}
