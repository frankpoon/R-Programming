## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a special "matrix" object 
## that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    cachedInverse <- NULL
    setMatrix <- function(y) {
        x <<- y
        cachedInverse <<- NULL
    }
    getMatrix <- function() x
    setInverse <- function(inverse) cachedInverse <<- inverse
    getInverse <- function() cachedInverse
    list(setMatrix = setMatrix, 
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
}

## cacheSolve computes the inverse of the special "matrix" returned 
## by makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.
cacheSolve <- function(x) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getInverse()
    if(!is.null(inverse)) {
        message("getting cached matrix")
        return(inverse)
    }
    inverse <- solve(x$getMatrix())
    x$setInverse(inverse)
    inverse
}
