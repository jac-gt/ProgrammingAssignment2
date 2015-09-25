## This file contains the following functions:
##  1. makeCacheMatrix: This function creates a special "matrix" object that can cache
##                      its inverse.
##  2. cacheSolve: This function computes the inverse of the special "matrix" returned 
##                 by makeCacheMatrix above. If the inverse has already been calculated
##                 (and the matrix has not changed), then the cachesolve should retrieve
##                 the inverse from the cache.

##  The function creates a special "matrix" object that can cache its inverse.
##  Args: 
##     x: A square matrix
## Returns:
##        A special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()){
        
        #Initialize the inverse of the matrix to NULL
        mInv <- NULL
        
        #Initialize the updated Flag to false
        mUpdated <- FALSE 
        
        #Function to compare two matrices
        matEqual <- function(a, b)
                is.matrix(a) && is.matrix(b) && dim(a) == dim(b) && all(a == b)
        
        #Function to set the matrix
        setMatrix <- function(mat = matrix()){
                if(!matEqual(x,mat)) {
                        x <<- mat
                        mInv <<- NULL
                        mUpdated <<- TRUE      
                } else {
                        mUpdated <<- FALSE
                }
        }
        
        #Function to check if matrix is updated
        getUpdated <- function() {
                mUpdated 
        }
        
        #Function to set updated to false
        setUpdated <- function(updated = FALSE) {
                mUpdated <<- updated
        }  
        
        #Function to get the matrix
        getMatrix <- function() {
                x      
        } 
        #Function to set the inverse of the matrix
        setInverse <- function(invMatrix){
                mInv <<-  invMatrix
        }
        #Function to get the inverse of the matrix
        getInverse <- function() mInv
        list(setMatrix = setMatrix, 
             getMatrix = getMatrix,
             setInverse = setInverse, 
             getInverse = getInverse,
             getUpdated = getUpdated,
             setUpdated = setUpdated)
}
##  The function computes the inverse of the special "matrix" returned by makeCacheMatrix
##  Args: 
##     x: A special "matrix" returned by makeCacheMatrix
## Returns:
##         The inverse of the square matrix of the special matrix.
cacheSolve <- function(x,...){
        if(!x$getUpdated()){
                mInv <- x$getInverse()
                if(!is.null(mInv)) {
                        message("Getting cached data")
                        return(mInv)
                }
        }
        
        message("Calculating inverse")
        
        mat <- x$getMatrix()
        mInv <- solve(mat,...)
        x$setInverse(mInv)
        x$setUpdated(FALSE)
        
        mInv
}