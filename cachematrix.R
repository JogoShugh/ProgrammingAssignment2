## These functions help you calculate, cache, and quickly look up the inverse of a numeric matrix. If you
## need to calculate the inverse of a large matrix multiple times, using these functions can help
## you reduce the time required.
##
## Basic Usage: 
## 
## > x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T))
## 
## The first time you try to solve for the matrix inverse, you'll get the result like so:
##
## > cacheSolve(x)
##     [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## If you call cacheSolve again with the same CacheMatrix instance, it will be pulled from the cached copy:
##
## > cacheSolve(x)
## getting cached inverse
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5

## Returns an object that wraps a numeric matrix and can store and retrieve a cached inverse of that matrix.
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

## Advanced Usage:
## 
## As stated in the Basic Usage, to create a CacheMatrix, pass in a matrix to the constructor function:
##
## > x <- makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=T))
## 
## To get the wrapped numeric matrix, call the get function on the returned object:
## > x$get()
##     [,1] [,2]
## [1,]    1    2
## [2,]    3    4
## 
## Initially, the CachMatrix will have no computed inverse:
## > x$getInverse()
## NULL
##
## The first time you try to solve for the matrix inverse, you'll get the result like so:
##
## > cacheSolve(x)
##     [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## If you call cacheSolve again with the same CacheMatrix instance, it will be pulled from the cached copy:
## > cacheSolve(x)
##
## getting cached inverse
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
## 
## You can also now query the CacheMatrix directly for its cached inverse:
##
## > x$getInverse()
##      [,1] [,2]
## [1,] -2.0  1.0
## [2,]  1.5 -0.5
##
## If you want to modify the CacheMatrix's wrapped matrix, you can set the matrix directly as below. Notice that
## once you set a new matrix, its cached inverse is cleared:
##
## > newMatrix <- matrix(c(1,2,3,4), nrow=2, ncol=2, byrow=F)
## > x$set(newMatrix)
## > x$getInverse()
## NULL
## 
## Finally, if you call cacheSolve for the CacheMatrix that you just modified, it will again compuete the new
## inverse matrix on the first call, then look it up from the cache on subsequent calls
##
## > cacheSolve(x)
##      [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > cacheSolve(x)
## getting cached inverse
##     [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
