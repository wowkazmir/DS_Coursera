
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 }

## This function computes the inverse of the special "matrix"

## It first checks to see if the inverse has already been calculated
cacheSolve <- function(x, ...) {
    inv <- x$getInverse
    if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
    }
## If the inverset has not been calculated, it does so here    
    data <- x$get()
    inv <- inverse(data, ...)
    x$setInverse(inv)
    inv
}
