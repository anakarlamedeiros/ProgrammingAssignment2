## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(matrixToCache = matrix()) {
    cachedMatrix <- matrixToCache
    inverse_cachedMatrix <- NULL
    set <- function(newMatrixToCache) {
        cachedMatrix <<- newMatrixToCache
        inverse_cachedMatrix <<- NULL
    }
    get <- function() cachedMatrix
    setInverse <- function(inverse) inverse_cachedMatrix <<- inverse
    getInverse <- function() inverse_cachedMatrix
    list(set= set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
