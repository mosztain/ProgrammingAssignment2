## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
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
