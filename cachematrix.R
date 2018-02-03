## The following functions enable you to set and get matrices
## as well solve for the inverses of the matrices; unless the
## inverse has already been calculated, at which point the value
## will be obtained from the stored cache.

## This function creates a special "matrix" that is a list containing
## functions for: 1. setting the value of the matrix, 2. getting the
## value of the matrix, 3. setting the value of it's inverse, and
## 4. setting the value of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## This function calculates the inverse of the special "matrix" from
## the previous function. However, it first checks whether the inverse
## has already been calculated. If so, it gets the inverse from the cache
## rather that wasting time recalculating.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
	if(!is.null(inv)) {
	    message("getting cached data")
	    return(inv)
	}
	data <- x$get()
	inv <- solve(data)
	x$setInverse(inv)
}
