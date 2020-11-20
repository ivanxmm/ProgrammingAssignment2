## makeCacheMatrix and cacheSolve create a special matrix and chaches its inverse.

## makeCacheMatrix creates a special 'matrix' that is a list with a function that
## creates a matrix and stores in cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
        x <<- y
        inv <<- NULL
        }
        get <- function() x
        setInverse <- function(solveMatrix) inv <<- solveMatrix
        getInverse <- function() inv
        list (set = set, get = get, setInverse = setInverse, 
        getInverse = getInverse)
}


## cacheSolve calculates the inverse of the 'matrix' created by makeCacheMatrix but
## checks first to see if the inverse was already calculated, if yes, it gets the 
## inverse from the cache, if no it calculates the inverse and prints it.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse (inv)
        inv        ## Return a matrix that is the inverse of 'x'
}
