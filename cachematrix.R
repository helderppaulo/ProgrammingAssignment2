## The makeCacheMatrix creates an object that holds a matrix and a cache of its
## inverted matrix. When the function set of this object is called the cache is
## invalidated, being set to NULL. 
## The cacheSolve function, inverts a cacheable matrix object. If this object
## has a cached inverted matrix set, it only returns the cache, otherwise, it
## calculates the inversion, sets the cache and returns the inverted matrix.

## This function receives a matrix and returns an object that wraps the matrix
## and defines a internal cache for its inverted matrix.
## Whenever the set element is invoked the internal matrix is changed to the
## new matrix passed to the set function, and the cache is set to NULL.
makeCacheMatrix <- function(x = matrix()) {
	cache <- NULL
        set <- function(y) {
                x <<- y
                cache <<- NULL
        }
        get <- function() x
        setinverted <- function(inverted) cache <<- inverted
        getinverted <- function() cache
        list(set = set, get = get,
             setinverted = setinverted,
             getinverted = getinverted)
}


## This function receives a cacheable matrix (technically a list of four 
## functions defined by the makeCacheMatrix function) and returns the wrapped
## matrix inverted. The function checks if the object has the inverted matrix
## cached, if it has return it, else it calculates its inversion sets the cache
## then returns the result.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverted()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverted(m)
        m
}
