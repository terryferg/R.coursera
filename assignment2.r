## Terry Ferg's assignment2 functions
## Your assignment is to write a pair of functions that cache the inverse of a
## matrix.

##  Write the following functions:
        
##  makeCacheMatrix: This function creates a special "matrix" object that can 
##  cache its inverse.
##  cacheSolve: This function computes the inverse of the special "matrix" 
##  returned by makeCacheMatrix above. If the inverse has already been 
##  calculated (and the matrix has not changed), then the cachesolve 
##  should retrieve the inverse from the cache.

##  Computing the inverse of a square matrix can be done with the solve 
##  function in R. For example, if X is a square invertible matrix, then
##  solve(X) returns its inverse.
##  For this assignment, assume that the matrix supplied is always
##  invertible.


makeCacheMatrix <- function(x = matrix()) {
        
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}