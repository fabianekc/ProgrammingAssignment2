## makeCacheMatrix creates a special "matrix", which is really a list 
## containing functions to 
## - set and get the value of the matrix, and 
## - set and get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix()) {
        minv <- NULL          ## Matrix INVerse
        set <- function(y) {
                x <<- y
                minv <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) minv <<- solve
        getInverse <- function() minv
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

## The following function calculates the inverse of the special "matrix"
## created with the above function. However, it first checks to see if 
## inverse has already been calculated. If so, it gets the inverse from 
## the cache and skips the computation. Otherwise, it calculates the 
## inverse of the matrix and sets the value of the inverse in the cache 
## via the setInverse function.

cacheSolve <- function(x, ...) {
        minv <- x$getInverse()
        if(!is.null(minv)) {
                message("getting cached data")
                return(minv)
        }
        data <- x$get()
        minv <- solve(data, ...)
        x$setInverse(minv)
        minv
}

## example usage
##   mat<-makeCacheMatrix(matrix(rnorm(9),3,3))
##   cacheSolve(mat)
##     => calculates inverse
##   cacheSolve(mat)
##     => returns inverse from cache