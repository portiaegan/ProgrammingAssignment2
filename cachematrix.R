## Caching the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  xinv <- NULL
  set <- function(y) {
    x <<- y
    xinv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) xinv <<- inverse
  getinverse <- function() xinv
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the special "matrix" created with makeCacheMatrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  xinv <- x$getinverse()
  if (!is.null(xinv)) {
    message("getting cached data - inverse matrix")
    return(xinv)
  }
  mat.data <- x$get()
  xinv <- solve(mat.data, ...)
  x$setinverse(xinv)
  xinv
}
