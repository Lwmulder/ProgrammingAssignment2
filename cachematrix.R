## Put comments here that give an overall description of what your
## functions do

## Create the inverse of the maxtrix.
 makeCacheMatrix <- function(x = matrix()) {
    z <- NULL
    set <- function(y) {
        x <<- y
        z <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) z <<- inverse
    getInverse <- function() z
    list(set = set,
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Looks in the cache if the value is already there

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    z <- x$getInverse()
    if (!is.null(z)) {
        message("getting cached data")
        return(z)
    }
    data <- x$get()
    z <- solve(data, ...)
    x$setInverse(z)
    z
}


