## makeCacheMatrix function caches a matrix and its inverse to enable 
## easy retrieval of inverse of the matrix without recomputing
## cacheSolve function computes inverse of a matrix if not found in the cache

makeCacheMatrix <- function(x = matrix()) {
    ## Caches a matrix and its inverse
    ## Place the input matrix into cache
    setMat <- function(y) {
        x <<- y
        i <<- NULL
    }
    # Return the matrix from the cache
    getMat <- function() {
        x
    }
    # Place the inverse of matrix into cache
    setInv <- function(invert) {
        i <<- invert
    }
    # Return inverse of matrix from cache
    getInv <- function() {
        i
    }
    clearMat <- function() {
        x <<- NULL
        i <<- NULL
    }

    list(setMat = setMat, getMat = getMat,
         setInv = setInv,
         getInv = getInv,
         clearMat = clearMat)
}


cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    ## Check if inverse of the matrix is in cache
    ## if inverse is a matrix with all elements as NA, it is not in cache
    ## then compute the inverse and place it in the cache for future calls
    inv = makeCacheMatrix(x)$getInv()
    if(!is.null(inv)) {
        print("getting cached data")
        return(inv)
    }
    else {
#        inv <- solve(x, ...)
        makeCacheMatrix(x)$setInv(solve(x, ...))
#        m <- makeCacheMatrix(x)$getMat()
#        print(m)
        makeCacheMatrix(x)$getInv()
    }
}