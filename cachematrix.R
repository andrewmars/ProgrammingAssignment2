## A set of functions to speed up repetetive computations of the inverse matrix.
## They internally cache the result of calculating the inverse matrix.
## If the data don't change, no computation is needed.

##
## Creates a special matrix with 4 additional helper functions to: 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix
##
makeCacheMatrix <- function(x = matrix()) {
    xi <- NULL

    set <- function(y) {
        x <<- y
        xi <<- NULL
    }

    get <- function(y) x
    
    setinverse <- function(xinv) xi <<- xinv
    
    getinverse <- function() xi
    
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

##
## Calculates the inverse of the special matrix created with the makeCacheMatrix() function.
## If the inverse matrix has already been calculated, it gets the value from the cache and skips the computation.
## Otherwise, it calculates the inverse matrix and sets its value in the cache via the setinverse() function.
##
cacheSolve <- function(x, ...) {
    xi <- x$getinverse()
    if (!is.null(xi)) {
        message("getting cached data")
        return(xi)
    }
    data <- x$get()
    xi <- solve(data)
    x$setinverse(xi)
    xi
}
