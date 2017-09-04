## These functions are for creating objects that stores a matrix and its inverse

## makeCacheMatrix function accepts matrix parameter and returns an object that
## contains a list of functions that will set and get a matrix,


makeCacheMatrix <- function(x = matrix()) {
        theinv <- NULL
        set <- function(y) {
                x <<- y
                theinv <<- NULL
        }
        get <- function() x
        setTheInverse <- function(inverse) theinv <<- inverse
        getTheInverse <- function() theinv
        list(set = set,
             get = get,
             setTheInverse = setTheInverse,
             getTheInverse = getTheInverse)
}


## cacheSolve takes an object parameter returned by makeCacheMatrix and returns
## the inverse of the matrix for the makeCacheMatrix above.  If the inverse
## has already been calculated and the matrix has not changed, then retrieve
## the inverse from the cache. If it hasn't been calculated, it will calculate
## the inverse and cache it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        theinv <- x$getTheInverse()
        if (!is.null(theinv)) {
                message("getting cached data")
                return(theinv)
        }
        mat <- x$get()
        theinv <- solve(mat, ...)
        x$setTheInverse(theinv)
        theinv
}
