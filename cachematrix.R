## This R file contains two functions that perform operations over a supplied matrix.
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
##      1. m <- matrix(c(4,2,7,6),2,2)             ## creates a matrix 'm'
##      2. cachedMatrix <- makeCacheMatrix(m)      ## stores the matrix 'm' in a cache
##      3. inverse_m <- cacheSolve(cachedMatrix)   ## returns the inverse of 'm' stored in 'cachedMatrix' 
##
## Note: Both functions assume that the supplied matrix is invertible!


##  Function: makeCachedMatrix
##  This function stores a matrix and its inverse. 
##  Input arguments:
##      * matrixToCache - input matrix to cache
##  Return:
##      * a list l containing four functions to get or set the cached matrix or 
##        its cached inverse:
##              1. l$get() - returns the currently cached matrix
##              2. l$set(newMatrixToCache) - the cached matrix is set to input 'newMatrixToCache'
##              3. l$getInverse() - returns the currently cached inverse 
##              4. l$setInverse(inverseToCache) - sets the cached inverse to 'inverseToCache'
##
makeCacheMatrix <- function(matrixToCache = matrix()) {
    
    ## the two cached variables
    cachedMatrix <- matrixToCache
    inverse_cachedMatrix <- NULL
    
    ## function to set the cached matrix
    set <- function(newMatrixToCache) {
        cachedMatrix <<- newMatrixToCache
        inverse_cachedMatrix <<- NULL
    }
    ## functionto get the cached matrix
    get <- function() cachedMatrix
    
    ## function to set the cached inverse 
    setInverse <- function(inverseToCache) inverse_cachedMatrix <<- inverseToCache
    
    ##function to get the cached inverse
    getInverse <- function() inverse_cachedMatrix
    
    ##return list with the 4 functions to get/set matrix and its inverse
    list(set= set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


##  Function: cacheSolve
##  This function returns the inverse of a matrix created using the function 
##  'makeCacheMatrix' above. In case matrix has already a cached inverse, this value 
##  will be returned. Otherwise, this function computes the inverse of the matrix, 
##  caches this inverse and returns it.
##  Input arguments:
##      * matrix - input matrix to which the inverse should be returned. Note that
##                 matrix should be created by using the function 'makeCachedMatrix'
##  Return:
##      * the inverse of supplied input matrix
##
cacheSolve <- function(matrix, ...) {
    
    #retrieving the currently stored value for the inverse of matrix        
    inverse <- matrix$getInverse()
    
    #testing whether inverse needs to be calculated
    if(is.null(inverse)) {
        message("Inverse not yet cached. Performing calculations...")
        cachedMatrix <- matrix$get()
        inverse <- solve(cachedMatrix, ...)
        matrix$setInverse(inverse)
    }

    #returning the cached value for the inverse of matrix
    return(inverse)
}
