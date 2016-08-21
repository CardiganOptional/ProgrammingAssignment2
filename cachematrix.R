## Coursera R Programming Assignment 2


## makeCatchMatrix creates matrix that can cache its inverse
## cacheSolve computes the invers of the previous matrix


## makeCacheMatrix, function 1

makeCacheMatrix <- function(x = matrix()) {
  v <- NULL
  set <- function(y) {
    x <<- y
    v <<- NULL
  }
  get <- function() x
  setInverse <- function(solveMatrix) v <<- solveMatrix
  getInverse <- function() v
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## cacheSolve, second function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  v <- x$getInverse()
  if(!is.null(v)) {
    message("getting cached data")
    return(v)
  }
  data <- x$get()
  v <- solve(data)
  x$setInverse(v)
  v
}
