## This function creates a special "matrix" object that can cache its inverse ##

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(y) {
    x <<- y
    i <<- NULL
  }

  get <- function() x
  
  setInverse <- function(inverse) i <<- inverse
  
  getInverse <- function() i
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix ##
## If the inverse has already been calculated (and the matrix has not changed),           ##
## then retrieve the inverse from the cache                                               ##

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)) {
    message('Getting cached data')
    return(i)
  }
  
  x$setInverse( solve(x$get(), ...) )
  x$getInverse()
}
