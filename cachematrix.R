##  R Programming - Program Assignment #2
##  Matrix inversion is usually a costly computation and there may be some
##  benefit to caching the inverse of a matrix rather than computing it
##  repeatedly.  The following pair of functions are used to 
##  cache the inverse of a matrix.

## The first function, `makeCacheMatrix` creates a special "matrix" object
## that can cache its inverse. The return list contains a function to
##    1.  set the value of the matrix 
##         Use this when you want to set of change the value of the matrix
##    2.  get the value of the matrix
##    3.  set the matrix inverse
##    4.  get the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse of
## the matrix and stores the inverse of the matrix in the cache via the `setinverse`
## function.
## The matrix is changed using the 'set' function defined in makeCacheMatrix.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
    ## If there is a stored inverse, get the cached inverted matrix
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
    ## If there is not a cached inverted matrix, create one using
    ##   the "solve" function and set it in cache using "setinverse"
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}
