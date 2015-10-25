## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverseMatrix <- NULL
    set <- function(y){
        x <<- y
        inverseMatrix <<- NULL
    }
    get <- function() x
    setInverse <- function(i) inverseMatrix <<- i
    getInverse <- function() inverseMatrix
    list(set = set, get = get, setInverse = setInverse, getInverse = get)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    if(!is.null(inv)){
        message("getting cache data")
        return(inv)
    }
    dat <- x$get()
    inv <- solve(dat, ...)
    x$setInverse(inv)
    inv
}
