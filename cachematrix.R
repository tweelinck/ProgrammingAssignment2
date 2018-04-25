## These two functions allow you to invert a matrix, but caches the inverse. Therefore, when you compute the inverse of the matrix whose inverse is cached the function doesn't computed the inverse again, but just returns the cached value.

## This function creates a `matrix' (actually consisting of four functions) that allows you to cache the inverse of the matrix in this `matrix' itself. To specify a matrix m under the name x run the commands: `x <- makeCachematrix' and 'x$set(m)'.

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y) {
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setinv <- function(inv) xinv <<- inv
        getinv <- function() xinv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function computes the inverse of a matrix, (or rather of a `matrix' as constructed above). In the case the matrix inverse is cached, it just returns the cached value  (without re-doing the computation). 

cacheSolve <- function(x) {
        xinv <- x$getinv()
        if(!is.null(xinv)) {
                message("getting cached data")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data)
        x$setinv(xinv)
        xinv
}
