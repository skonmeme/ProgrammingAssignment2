###############################################################################
# makeCacheMatrix and cacheSolve functions are to improve the efficiency
# of inverse matrix management. Inverse of matrix spends quiet amount of CPU 
# power, therefore caching is quiet effient to keep CPU power.

## Write a short comment describing this function
# make an list object with a matrix and related functions (getter and setter)
makeCacheMatrix <- function(matrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(aMatrix) {
    matrix <<- aMatrix
    inverseMatrix <<- NULL
  }
  get <- function() matrix
  setInverse <- function(inverse) inverseMatrix <<- inverse
  getInverse <- function() inverseMatrix
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function
# return cached inverse matrix if there is cached nor newly computed (and cached)
# inverse matrix.
cacheSolve <- function(cacheMatrix, ...) {
  inverseMatrix <- cacheMatrix$getInverse()
  if (!is.null(inverseMatrix)) {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  matrix <- cacheMatrix$get()
  inverseMatrix <- solve(matrix, ...)
  cacheMatrix$setInverse(inverseMatrix)
  inverseMatrix
}
