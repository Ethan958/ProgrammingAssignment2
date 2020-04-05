## The function is used to calculate matrix inversion quickly
## by cathing the inverse of a matrix rather than compute it repeatedly.

## The first function caches the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve <- function(solve) 
                m <<- solve
        getsolve <- function() m
        list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## The second function create the matrix inversion. 
## If the value has existed, it will be taken from cache.
## If the value doesn't exist, it will be calculated by this function.

cacheSolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
        ## Return a matrix that is the inverse of 'x'
}


