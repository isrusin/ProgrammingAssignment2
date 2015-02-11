## CacheMatrix is a matrix wrapper that can cache the inverse of the matrix.
## Call cacheSolve() fuction instead of solve() one to benefit from the feature.

## Initializes a matrix wrapper that can cache the inverse of itself.

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(cmatrix) {
    x <<- cmatrix
    inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(imatrix) inverse <<- imatrix
  getinverse <- function() inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Returns the inverse of a matrix wrapped with CacheMatrix.

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    return(inverse)
  }
  cmatrix <- x$get()
  inverse <- solve(cmatrix, ...)
  x$setinverse(inverse)
  inverse
}
