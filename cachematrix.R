## The following two functions makeCacheMatrix() and cacheSolve() implement 
## caching for matrix inverse. Inverse of a matrix is not recalcuated if
## already calaucted once.


## The function makeCacheMatrix(matrix) Creates a special "matrix" object that can cache its
## inverse.Retruned matrix provides following functions:
## set(matrix) : Set the underlying matrix
## get() : Get the underlying matrix
## setinverse(matrix) : Set inverse of the matrix
## getinverse() : Fetch inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        if(class(y)!="matrix"){
            message("Type not a matrix! Please supply valid matrix.")
        }else{
            x <<- y
            inv <<- NULL   
        }
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## The function cacheSolve(matrix, ...) computes the inverse of the special 
##"matrix" returned by makeCacheMatrix. If the inverse has already been
## calculated (and the matrix has not changed),then the cachesolve should
## retrieve the inverse from the cache.
## This function assumes that the matrix is invertible.

cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        # Inverse is avaliable in cache
        message("Getting cached inverse")
        return(inv)
    }
    # Calculating inverse as it is not cached yet
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv  # Return inverse
}
