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

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  
  if (!is.null(i)) {
    message('Getting cached data')
    return(i)
  }
  
  x$setInverse( solve(x$get(), ...) )
  x$getInverse()
}
