##########################################################################
#
# Author: Luis Ramos
# Class:  R Programming
# School: Coursera
# Date: 3/17/2015
#
#
####################################################################

# Description: The following pair of functions will calculate the inverse
#   inverse of a matrix if it has not been already calculated; Otherewise, 
#   it retrieves the function inverse from a cache value.

## Function: makeCacheMatrix
##
## Description:  This function make a special "matrix" which is really a
##  list of functions used to process the matrix.  The matrix processing 
##  functions are as follows:
##      1. set -- This will set the matrix values
##      2. get -- This will get the matrix values
##      3. setMatrixInverse -- This will cache the matrix inverse. Note:
##                          function is not to be called directly. It must 
##                          called from the helper function "cachesolve"
##      4. getMatrixInverse -- This retrieves the cache matrix inverse 
##
##  Parameters:
##      x --> a matrix to be used by this generator function.
##      Returns a list of matrix handling functions
##

makeCacheMatrix <- function(x = matrix()) {
    
    ##
    ## Local environment variables of "makeCacheMatrix" function
    ##
    matrixInverse <- NULL # variable used to cache the matrix inverse
    
    ## Function: set
    ##
    ## Description: Initializes the matrix cache varialbel to null, and
    ##  sets the variable x with the matrix values.
    ##
    ## Parameters:
    ##  y  --> a matrix
    ##
    set <- function(y) {
        x <<- y  # Set X in the "makeCacheMatrix" argument environment to 
                 # the value of y
        matrixInverse <<- NULL  # Sets matrixInverse" to NULL at the local
                                # local environment of the "makeCacheMatrix"
                                # function.
    }
    
    ## Function: get
    ##
    ##
    ## Description: Retrieves the value of x
    ##
    ## Parameters:
    ##   Returns a matrix
    ##
    get <- function() x
    
    ## Function: setMatrixInverse
    ##
    ## Description: Caches the inverse of a matrix
    ##
    ## Parameters:
    ##  invertedMatrix --> a matrix containing the inverted matrix of "x"
    ##
    ## Note: This function is meant to be called only by the helper 
    ## function called cacheSolve.
    ##
    setMatrixInverse <- function(invertedMatrix) {
        matrixInverse <<- invertedMatrix  # Sets matrixInverse to the inverse
                                          # of the "x" matrix. This value will
                                          # be stored on the matrixInverse
                                          # which is located at the local
                                          # environment of the "makeCacheMatrix"
                                          # function.
    }
    
    ## Function: getMatrixInverse
    ##
    ## Description: Retrieves the cache matrix inverse of "x" matrix.
    ##
    ## Parameters:
    ##  Returns a matrix containing the cached inverse of the "x" matrix.
    ##
    getMatrixInverse <- function() matrixInverse
    
    ## The function return value a list of the embeded functions.
    list( set = set, get = get,
          setMatrixInverse = setMatrixInverse,
          getMatrixInverse = getMatrixInverse)
}


## Function: cacheSolve
##
## Description: This function calculates the inverse of a matrix. If it has
##  not been prior calculated. Otherwise, it retrieves the inverse matrix
##  from a cache value.
##
## Paramters:
##  x --> a matrix
##  Return the inverse matrix
cacheSolve <- function(x, ...) {
    
    ## Return a matrix the is the inverse of 'x'
    matrixInverse <- x$getMatrixInverse()
    if(!is.null(matrixInverse)) {
        message("Getting Cached Matrix Inverse")
        return(matrixInverse)
    }
    
    data <- x$get()  # Retrieve the matrix data
    matrixInverse <- solve(data, ...) # Calculate the matrix inverse
    x$setMatrixInverse(matrixInverse) # Cache the matrix inverse result
    matrixInverse # Return the matrixInverse
}
