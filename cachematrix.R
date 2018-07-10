# Per assignment 2 for week 3, this file contains functions that allow caching the inverse of a matrix and accessing it later. This saves processing time.
# Note that only square matrices have an inverse. The solve(x) function used below will retun an error if x is not square (or if its determinant is 0).

# makeCacheMatrix contains a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) minv <<- solve
    getinverse <- function() minv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix" object from makeCacheMatrix. If the inverse already exists, it is accessed instead of computed.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinverse()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)
    x$setinverse(minv)
    minv
    
}
