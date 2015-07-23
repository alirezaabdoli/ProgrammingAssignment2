## This file contains two function aimed at creating a matrix
## and subsequently getting the inverse of the matrix in case
## already cached otherwise inverse of the matrix is calculated.


## This function 'makeCacheMatrix' creates a matrix and
## provides a set of functions for manipulating the matrix

makeCacheMatrix <- function(x = matrix()) 
{
      inv = NULL
      ## Setting the initial matrix
      set_matrix = function(y)
      {
            x <<- y
            inv <<- NULL
      }
      ## Getting the initial matrix
      get_matrix = function()
      {
            x
      }
      ## Setting the inversed matrix
      set_inverse = function(inverse)
      {
            inv <<- inverse
      }
      ## Getting the inversed matrix
      get_inverse = function()
      {
            inv
      }
      
      list(set_matrix = set_matrix, get_matrix = get_matrix, 
           set_inverse = set_inverse, get_inverse = get_inverse)
}


## This function 'cacheSolve' returns the inverse of the matrix
## if already cached; otherwise the function calculates inverse of
## the matrix.

cacheSolve <- function(x, ...) 
{
      ## Return a matrix that is the inverse of 'x', if already
      ## cached
      inv = x$get_inverse()
      if (!is.null(inv))
      {
            message("Getting the cached matrix")
            return(inv)
      }
      ## Otherwise inverse of the matrix is calculated and
      ## cached for subsequent use.
            matrix = x$get_matrix()
            inv = solve(matrix, ...)
            x$set_inverse(inv)
            inv
}
