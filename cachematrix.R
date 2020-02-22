## Functions to support caching of the inverse of a matrix

## Creates an object representing a matrix that supports the caching of its inverse

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    get <- function() x
    set <- function(y) {
        x <<- y
        inverse <<- NULL
    }
    getinverse <- function() inverse
    setinverse <- function(solved_inverse) inverse <<- solved_inverse
    list(get = get, set = set, getinverse = getinverse, setinverse = setinverse)
}


## Produces and caches the inverse of a matrix

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if (!is.null(inv)) {
        message("returning cached inverse")
        return(inv)
    }
    m <- x$get()
    inv <- solve(m)
    x$setinverse(inv)
    inv
}
