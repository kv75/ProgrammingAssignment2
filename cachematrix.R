## These two functions are used to create a special object
## that stores an invertible matrix and caches its inverse

## Function 'makeCacheMatrix' creates a special "matrix" object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    ## 'x' is an invertible matrix
    
    ## Return a "matrix" object
    
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() {
        x
    }
    setInverse <- function(solved) {
        m <<- solved
    }
    getInverse <- function() {
        m
    }
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Function 'cacheSolve' computes the inverse of the special "matrix"
## returned by 'makeCacheMatrix'. If the inverse has already been calculated
## (and the matrix has not changed), then 'cacheSolve' retrieves
## the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## 'x' is a "matrix" object returned by 'makeCacheMatrix'

    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getInverse()
    if(!is.null(m)) {
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
