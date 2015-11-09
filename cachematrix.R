# Create a function that creates a matrix that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
# "inv" holds the cached values or NULL if nothing is cached
    inv <- NULL
# Store a matrix    
  set <- function(y) {
    x <<- y
# Since the matrix is assigned a new value, empty the cached values 
        inv <<- NULL
  }
# returns a stored matrix
  get <- function() x
# cache the given argument
  setInverse <- function(inverse) inv <<- inverse
# get the cached value
  getInverse <- function() inv
# return a list. Each named element of the list is a function
    list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


# This functions calculates the inverse of a special matrix created with makeChachMatrix
cacheSolve <- function(x, ...) {
# get the cached value
  inv <- x$getInverse()
# if there exists a value, return it  
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
# otherwise, calculate the matrix inverse and store it in cache  
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
# resturn the inverse
  inv
}
