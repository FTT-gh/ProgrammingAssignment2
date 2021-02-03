## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix creates a special matrix object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    minv <- NULL
    set <- function(y) {
        x <<- y
        minv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) minv <<- inv
    #setinv <- function() inv <<- solve(x)
    getinv <- function() minv;
    
    list(set    = set, 
         get    = get,
         setinv = setinv,
         getinv = getinv)
}

## The function cacheSolve computes the inverse of the special matrix returned by 
#  makeCacheMatrix above. If the inverse has already been calculated 
#  (and the matrix has not changed), then the cachesolve should retrieve the
#  inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    minv <- x$getinv()
    if(!is.null(minv)) {
        message("getting cached data")
        return(minv)
    }
    data <- x$get()
    minv <- solve(data, ...)    # !hint solve
    x$setinv(minv)
    minv
}


