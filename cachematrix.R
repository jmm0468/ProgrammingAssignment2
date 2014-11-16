# author: jmm0468
#
# Matrix inversion is usually a costly computation and their may be some benefit
# to caching the inverse of a matrix rather than compute it repeatedly.
# This file contains a pair of functions that cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  # This function creates a special "matrix" object that can cache its inverse.
  # The object is really a list with get, set, getinverse, and setinverse
  # functions
  #
  # Arguments:
  #   x: a matrix
  # Returns:
  #   a list with the following function elements:
  # set - the value of the matrix
  # get - the value of the matrix
  # setinverse - the value of the matrix inverse
  # getinverse - the value of the matrix inverse

  cacheMatrix <- NULL
  set <- function(y) {
    x <<- y
    cacheMatrix <<- NULL
  }
  get <- function() {
    return(x)
  }
  setinverse <- function(inverseMatrix) {
    cacheMatrix <<- inverseMatrix
  }
  getinverse <- function() {
    return(cacheMatrix)
  }
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  # This function computes the inverse of the special "matrix" returned by
  # 'makeCacheMatrix' above. If the inverse has already been calculated (and the
  # matrix has not changed), then this function retrieves the inverse
  # from the cache.
  #
  # It is assumed that the supplied matrix is invertible.
  #
  # Notes: Computing the inverse of a square matrix can be done with the 'solve'
  # function in R. For example, if X is a square invertible matrix, then
  # solve(X) returns its inverse.
  #
  # Arguments:
  #   x: the value that 'makeCacheMatrix' returns
  #   ...: arguments that may be supplied to 'solve' beyond 'a' and 'b'
  # Returns:
  #   The matrix inverse

  inverseMatrix <- x$getinverse()
  if (!is.null(inverseMatrix)) {
    message("Getting cached inverse matrix")
    return(inverseMatrix)
  }
  squareMatrix <- x$get()
  if (is.matrix(squareMatrix) != T) {
    stop("A square matrix is required.")
  }
  if (ncol(squareMatrix) != nrow(squareMatrix)) {
    stop("A square matrix is required.")
  }
  identityMatrix <- diag(ncol(squareMatrix))
  inverseMatrix <- solve(a = squareMatrix, b = identityMatrix, ...)
  x$setinverse(inverseMatrix)
  inverseMatrix
}
