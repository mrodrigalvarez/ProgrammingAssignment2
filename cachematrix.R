################################################################################
## FILE:    cachematrix.R
## PURPOSE: Caching the Inverse of a Matrix
## PRE:     Assumes that the matrix supplied is always invertible.
################################################################################

################################################################################
## makeCacheMatrix
##   Function that creates a special "matrix" object that can cache its inverse
##   so it avoids compute it repeatedly.
##   parameters
##     'x': matrix to stored in the cache
##   Return a list with a matrix that can cache its inverse
################################################################################
makeCacheMatrix <- function(x = matrix()) {
    # gets & set args
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    # get & set inverse
    setinverse <- function(solve) inv <<- solve
    getinverse <- function() inv
    # return
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

################################################################################
## cacheSolve
##   Function that computes the inverse of the special "matrix" returned by 
##   'makeCacheMatrix' above. If the inverse has already been calculated (and 
##   the matrix has not changed), then retrieve the inverse from the cache.
##   parameters
##     'x': result from function 'makeCacheMatrix' (special "matrix")
##   Return a matrix that is the inverse of 'x'
################################################################################
cacheSolve <- function(x, ...) {
    # 
    inv <- x$getinverse()
    # check if the inverse matrix is stored in cache
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv) 
    }
    # return the inverse matrix of 'x' after computing and saving it in the cache
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
