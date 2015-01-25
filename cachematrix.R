## This functions (makeCacheMatrix, cacheSolve) creates a way to avoid recalculating the inverse of a matrix if
## the matrix hasn't been changed. It is an optimization to avoid make a posibily performance intensive operation.

## The makeCacheMatrix function returns a wrapper object to a matrix, that has the ability of remembering
## the inverse of the given matrix.
## Given a matrix `m`, the way to create a new cached matrix is:
## matrixWithCache <- makeCacheMatrix(m)
## then, the matrix can be obtained by writting matrixWithCache$get()

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The cacheSolve function calculates the inverse of the matrix provided, where this matrix is an object created
## by makeCacheMatrix.
## To get the inverse simply call: cacheSolve(matrixWithCache)
## This function ensures to use the cached version of the inverse matrix, or calculates the inverse matrix if necessary

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached inverse matrix")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
