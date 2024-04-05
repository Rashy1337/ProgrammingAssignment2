## This function creates a special "matrix" object that can cache its inverse.
## x: A square numeric matrix.
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL  # Cache for the inverse of the matrix
  
  # Sets a new matrix and clears the inverse cache
  set <- function(y) {
    x <<- y
    inv <<- NULL  # Invalidate cached inverse because the matrix has changed
  }
  
  # Returns the matrix
  get <- function() x
  
  # Caches the inverse of the matrix
  setInverse <- function(inverse) inv <<- inverse
  
  # Returns the cached inverse of the matrix
  getInverse <- function() inv
  
  # Returns a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

### `cacheSolve` Function with Comments
## Computes the inverse of the special "matrix" returned by makeCacheMatrix.
## If the inverse is already cached and the matrix hasn't changed, it retrieves the inverse from the cache.
## x: An object returned by makeCacheMatrix containing a square matrix.
cacheSolve <- function(x, ...) {
  inv <- x$getInverse()  # Attempt to retrieve the cached inverse
  
  # If the inverse is already cached, it is returned immediately
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  
  # If the inverse was not cached, compute the inverse of the matrix
  data <- x$get()  # Get the matrix from the object
  inv <- solve(data, ...)  # Compute the inverse
  
  x$setInverse(inv)  # Cache the computed inverse
  inv  # Return the inverse
}
