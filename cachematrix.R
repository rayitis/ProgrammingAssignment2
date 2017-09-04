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
