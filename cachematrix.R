## The following functions are used to compute and cache the inverse of a matrix.
## The inversion operation can be time consuing. Therefore, caching its results can save some time.

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    
    im <- NULL
    set <- function(y) {
        x <<- y
        im <<- NULL
    }
    get <- function() x
    setinverse <- function(inverseMatrix) im <<- inverseMatrix
    getinverse <- function() im
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## THis functions is used to retreive the inverse of a matrix.
## If there is a cached result, the cached value is returned.
## Otherwise, this function will compute the invese of the matrix, cache it, and than return its value.

cacheSolve <- function(x, ...) {
    im <- x$getinverse()
    if(!is.null(im)) {
        message("getting cached data")
        return(im)
    }
    data <- x$get()
    im <- solve(data, ...)
    x$setinverse(im)
    im
}
