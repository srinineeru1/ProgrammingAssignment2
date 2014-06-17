## makeCacheMatrix function caches a matrix and its inverse to enable
## easy retrieval of inverse of the matrix without recomputing
## cacheSolve function computes inverse of a matrix if not found in the cache

makeCacheMatrix <- function(x = matrix()) {
    ## Caches a matrix and its inverse
    ## Place the input matrix into cache
#    if (!exists("i")) i <- NULL
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Return the matrix from the cache
    get <- function() x
    # Place the inverse of matrix into cache
    setinverse <- function(invert) i <<- invert
    # Return inverse of matrix from cache
    getinverse <- function() i
    clearCache <- function() {
        x <<- NULL
        i <<- NULL
    }
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse,
         clearCache = clearCache)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if inverse of the matrix is in cache
    ## if inverse is a matrix with all elements as NA, it is not in cache
    ## then compute the inverse and place it in the cache for future calls
    inv = x$getinverse()
    if(!is.null(inv)) {
        print("getting inverse from cache")
        return(inv)
    }
    else {
        inv <- solve(x$get())
        x$setinverse(inv)
        inv
    }
}