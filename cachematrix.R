
## Matrix inversion is usually a costly computation and there may be some benefit to 
## caching the inverse of a matrix rather than compute it repeatedly 
## The functions below cache the inverse of a matrix.

## The two functions are
        
        ## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
        ## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
        ## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
        ## retrieve the inverse from the cache.

## For these functions the assumption is that the matrix supplied is always invertible


## First, the makeCacheMatrix:
## Creates a special "matrix", that contains a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the Inverse
## get the value of the Inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
        x <<- y 
        m <<- NULL 
}
get <- function() x 
setinverse <- function(inverse) m <<- inverse 
getinverse <- function()
list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}

## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
