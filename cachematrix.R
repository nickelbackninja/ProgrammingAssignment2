
## These function allow Caching the inverse of a matrix. 

## This function, makeCacheMatrix, can make a special type of matrix which can then cache the inverse. 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## This function, cacheSolve, computes the inverse of the new matrix made by the function above, makeCacheMatrix. It will retrieve the inverse matrix from the cache if
## there has been no changes made. 

cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if (!is.null(i)) {
    message("retrieving cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setInverse(i)
  i
}