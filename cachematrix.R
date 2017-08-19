## Put comments here that give an overall description of what your
## functions do
## The overall purpose of the script is to cache results of computationally 
## expensive matrix inversion operation.

## Write a short comment describing this function
## The first function 'makeCacheMatrix' is used to 'set' (assign) and 'get'  
## (retrieve) an invertible matrix (data) and its inverse matrix.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inv) m <<- inv
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## Write a short comment describing this function
## The second function 'cacheSolve' returns the cached value of inverse matrix 
## if the data is not updated; it returns the re-calculated inverse matrix if 
## the data is updated.
cacheSolve <- function(x, ...) {
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    ## Return a matrix that is the inverse of 'x'
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}
