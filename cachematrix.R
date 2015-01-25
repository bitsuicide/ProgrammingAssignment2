## Write a pair of functions that cache the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    i_mtx <- NULL
    set <- function(y) {
        x <<- y 
        i_mtx <<- NULL
    }
    get <- function() x
    setinverse <- function(matrix) i_mtx <<- matrix
    getinverse <- function() i_mtx
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
    i_mtx = x$getinverse()
    if(!is.null(i_mtx)) {
        message("getting cache data")
        return(i_mtx)
    }
    data <- x$get()
    i_mtx <- solve(data)
    x$setinverse(i_mtx)
    i_mtx    ## Return a matrix that is the inverse of 'x'
}
