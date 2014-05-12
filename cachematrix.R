## R Programming
## Assignment 2
## Salvino A. Salvaggio

## Summary:
## The 2 following functions (makeCacheMatrix and cacheSolve) inverse a matrix and
## save the outcome in the global environment to make it available outside of 
## its original function environment.
## In the case the matrix has not changed and its inverse has already been calculated,
## cacheSolve just retreive the inverse matrix from the cache.


## Scope of the makeCacheMatrix() function is to create a list containing
## functions for reading, inversing a matrix, and saving it in the cache and retreiving.

makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL         ## variable 'inverse' initialized and set to NULL
    
    setMatrix <- function(y) {
        x <<- y                                         ## original matrix will be read and saved in variable 'x' in the 
                                                        ## global environment.
        inverse <<- NULL                                ## variable 'inverse' is initialized in the global environment 
        												## and set to NULL.
    }
    
    getMatrix <- function() x                           ## Function to retreive the original matrix
    
    setInverse <- function(solve) inverse <<- solve     ## This function instructs that the 
                                                        ## solved/inverted matrix will be saved in the 
                                                        ## variable 'inverse' in the global environment
    
    getInverse <- function() inverse                    ## Function to retreive the inverse matrix
    
    list(setMatrix = setMatrix, getMatrix = getMatrix,
         setInverse = setInverse, getInverse = getInverse) 		## Define the list of available functions
}


## Scope of cacheSolve() function is 1. to retreive cached inverse matrix if already exists and
## original matrix not changed, or 2. to compute inverse of new original matrix and save in cache.

cacheSolve <- function(x, ...) {
    data <- x$getMatrix()                               ## load the original matrix        
    inverse <- x$getInverse()                           ## load the inverse matrix if it already exists 
    													## (if not, 'inverse' stays empty)
    
    if(!is.null(inverse)) {                             ## if there already is an inverse matrix, retreive/return it
        return(inverse)
    }
    
    data <- x$getMatrix()                               ## load original matrix into 'data'
    inverse <- solve(data, ...)                         ## inverse original matrix and save into 'inverse'
    x$setInverse(inverse)                               ## use function x$setInverse to save in the global environment
    inverse                                             ## output 'inverse'
}
