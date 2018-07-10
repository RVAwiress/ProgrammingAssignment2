# Per assignment 2 for week 3, this file contains functions that allow caching the inverse of a matrix and accessing it later. This saves processing time.
# Note that only square matrices have an inverse. The solve(x) function used below will retun an error if x is not square (or if its determinant is 0).

# makeCacheMatrix contains a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # this function contains the get & set functions for the matrix data and the matrix inverse
    # minv will be the matrix inverse, initially set to null since it's not cached yet
    minv <- NULL
    
    # the set function is called when new data is put into the matrix
    # the new data is passed through to the variable x, and the matrix inverse is set to null since it hasn't been calculated yet for this new data
    # these changes are made in the parent environment
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    
    # the get function is called to retrieve the data in the matrix, so it just returns that data directly
    get <- function() x
    
    # set inverse has to solve for the inverse in order to set it, done in the parent environment
    setinverse <- function(solve) minv <<- solve
    
    # get inverse returns the inverse, which may have been set or it may be null
    getinverse <- function() minv
    
    # the function returns a list of the 4 nested functions, with their names for external access (e.g. makeCacheMatrix$set) equal to their names inside this function
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


# cacheSolve computes the inverse of the special "matrix" object from makeCacheMatrix. If the inverse already exists, it is accessed instead of computed.

cacheSolve <- function(x, ...) {
    # first get minv from the parent (or above) environment to see if it's been set
    minv <- x$getinverse()

    # if it has been set, print that the data is cached and then return minv
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv) # rest of function is not processed
    }
    
    # otherwise minv is not set, so need to set it
    # get the matrix data
    data <- x$get()
    # calculate the inverse
    minv <- solve(data, ...)
    # set the inverse in the parent (or above) environment
    x$setinverse(minv)
    
    # return the matrix inverse itself as the return for cacheSolve (in addition to setting it in its environment)
    minv
    
}
