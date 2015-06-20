## This R file contains two functions that perform operations over a supplied matrix.
## Note that both functions assume that the supplied matrix is invertible!
##
## The functions are:
##
##      * makeCachedMatrix - This function stores a matrix and its inverse.
##
##      * cacheSolve - This function returns an inverse of a matrix created 
##                     calling the function "makeCachedMatrix". The inverse
##                     will be cached after the first computation.
## 
## Hereby an example of how to use these functions:
##      m <- matrix(c(4,2,7,6),2,2)             ##create a matrix m
##      cachedMatrix <- makeCacheMatrix(m)      ## store m in a cache
##      inverse_m <- cacheSolve(cachedMatrix)   ## returns the inverse of m stored in 'cached matrix' 
##
##
##
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

cacheSolve <- function(matrix, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverse <- matrix$getInverse()
    if(is.null(inverse)) {
        message("Inverse not yet cached. Performing calculations...")
        cachedMatrix <- matrix$get()
        inverse <- solve(cachedMatrix, ...)
        matrix$setInverse(inverse)
    }    
    return(inverse)
}
