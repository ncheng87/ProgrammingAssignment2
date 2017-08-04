## makeCacheMatrix is used to create an object (matrix) to cache the inverse.
 
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}
## cacheSolve is a function to calculate the inverse of the object (matrix) created by makeCacheMatrix function above.
## if the inverse is already existing, then inverse will be retrieved from cache
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
##test functions
my_matrix <- makeCacheMatrix(matrix(1:4, 2, 2))
my_matrix
cacheSolve(my_matrix)
 
