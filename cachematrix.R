## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function() {
  mat <- NULL  # Initialize the matrix
  inv <- NULL  # Initialize the inverse
  set <- function(matrix) {
    # Set the matrix and invalidate the cached inverse
    mat <<- matrix
    inv <<- NULL
  }
  get <- function() mat  # Get the matrix
  setInverse <- function(inverse) inv <<- inverse  # Set the inverse
  getInverse <- function() inv  # Get the inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

# Function to compute the inverse of the special "matrix" and cache the result
cacheSolve <- function(matCache) {
  # Check if the inverse is already cached
  inverse <- matCache$getInverse()
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)  # Return the cached inverse
  }
  
  # If not cached, calculate the inverse using solve() and cache the result
  matrix <- matCache$get()
  if (!is.null(matrix)) {
    inverse <- solve(matrix)
    matCache$setInverse(inverse)
    return(inverse)
  } else {
    stop("Matrix not set. Use makeCacheMatrix to set the matrix.")
  }
}
