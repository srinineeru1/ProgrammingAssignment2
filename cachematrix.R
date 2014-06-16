## makeCacheMatrix function caches a matrix and its inverse to enable 
## easy retrieval of inverse of the matrix without recomputing
## cacheSolve function computes inverse of a matrix if not found in the cache

makeCacheMatrix <- function(x = matrix()) {
    ## Caches a matrix and its inverse
    ## Initialize inverse to a matrix with all elements as NA
    i = matrix(NA, nrow(x), ncol(x))
    ## Place the input matrix into cache
    set <- function(y) {
        x <<- y
        i <<- matrix(NA, nrow(y), ncol(y))
    }
    get <- function() x
    setinverse <- function(invert) i <<- invert
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if inverse of the matrix is in cache
    ## if inverse is a matrix with all elements as NA, it is not in cache
    ## then compute the inverse and place it in the cache for future calls
    i = makeCacheMatrix(x)$getinverse()
    if(!is.na(all(i))) {
        message("getting cached data")
        return(i)
    }
    data <- makeCacheMatrix(x)$get()
    i <- solve(data, ...)
    makeCacheMatrix(x)$setinverse(i)
    i
}