# A function that creates the 'special' matrix with caching capabilities.
# Returns a list of functions (setters and getters).

makeCacheMatrix <- function(matrix) {
  inverse <- NULL
  
  setMatrix <- function(x) {
    matrix <<- x
    inverse <<- NULL
  }
  getMatrix <- function() matrix
  setInverse <- function(i) inverse <<- i
  getInverse <- function() inverse
  
  list(
    setMatrix = setMatrix,
    getMatrix = getMatrix,
    setInverse = setInverse,
    getInverse = getInverse
  )
}

# Solves the 'special' matrix.
# Returns the inverse of the matrix.

cacheSolve <- function(specialMatrix) {
  inverse <- specialMatrix$getInverse()
  if(!is.null(inverse)) {
	message('Getting cached data ...')
    return(inverse)
  }
  matrix <- specialMatrix$getMatrix()
  inverse <- solve(matrix)
  specialMatrix$setInverse(inverse)
  inverse
}
