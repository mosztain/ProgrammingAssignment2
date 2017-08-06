## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will define the get and set operators
## for matrix 'x' and its inverse 'v'

makeCacheMatrix <- function(x = matrix()) {
    v <- NULL
    set <- function(y) {
        x <<- y
        v <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) v <<- inverse
    getInverse <- function() v
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function
## This function will check for the existance of the
## inverse matrix 'v' as a cached variable and return
## it.  If 'v' does not exist (is NULL) it will calculate
## the inverse of 'x' using the solve function and then 
## assign its result to 'v'

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    v <- x$getInverse()
    if(!isnull(v)) {
        message("getting cache data")
        return(v)
    }
    data <- x$get()
    v <- solve(x)
    x$setInverse(v)
    v
}
