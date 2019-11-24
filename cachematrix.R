## The functions makeCacheMatrix and cacheSolve can be used in conjuction
## to calculate and store the inverse of a matrix without the need to 
## recalculate it on subsequent calls.



makeCacheMatrix <- function(x = matrix()) {
  ## Input: A square matrix
  ## Creates a 'matrix' object for use by the cacheSolve function
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) inv <<- solve
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  ## Where 'x' is a special object created by makeCacheMatrix
  ## If the matrix inverse already exists in x it will not be recalculated
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv)
  inv
}
