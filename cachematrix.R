## Here the inverse of a matrix is calculated through caching the calculation and skipping the 
# calculation if the matrix is not changed.

## The "makeCacheMatrix" function caches the input matrix through giving a list of the values of 
# the matrix and its inverse by running some functions on them 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(   set = set,
                get = get,
                setInverse = setInverse,
                getInverse = getInverse)
}




## cacheSolve function calculates the inverse of the matrix provided by makeCacheMatrix and saves the inverse in the list
# returned by the makeCacheMatrix and if the inverse exists in the list i.e. the input matrix is not changed it skips the
# calculation and returns the value from the cached data

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
