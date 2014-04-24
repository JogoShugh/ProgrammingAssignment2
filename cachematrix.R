## These functions help you calculate, cache, and quickly look up the inverse of matrix. If you
## need to calculate the inverse of a large matrix multiple times, using these functions can help
## you reduce the time required.
##
## 
# x <- makeCacheMatrix(matrix(c(1,3,2,4), nrow=2, ncol=2))

## Returns an object that wraps a numeric matrix and can store a cached inverse of that matrix.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i
        list(
                set = set, 
                get = get,
                setInverse = setInverse,
                getInverse = getInverse
        )
}

## Given a CacheMatrix x, cacheSolve will attempt to retrieve the pre-cached inverse of x's wrapped matrix. 
## If no cached inverse is found, it calculates the inverse, stores it in x, and returns the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getInverse()
        if(!is.null(i)) {
                message("getting cached inverse")
                return(i)
        }
        wrappedMatrix <- x$get()
        i <- solve(wrappedMatrix, ...)
        x$setInverse(i)
        i
}

