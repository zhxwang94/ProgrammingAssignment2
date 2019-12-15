## Put comments here that give an overall description of what your
## functions do

## Set up the special "matrix" that can cache its value and the value of its inverse
## Contains four functions: get value, set value, get inverse value, set inverse value

makeCacheMatrix <- function(x = matrix()) {
  xInv <- NULL
  set <- function (y) {
    x <<- y
    xInv <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) xInv <<- solve
  getInverse <- function() xInv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Uses makeCacheMatrix to calculate the inverse of a simple matrix
## Calculates and caches the inverse of said matrix if it does not already exist
## Recalls the cached inverse if it already exists

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  xInv <- x$getInverse()
  if(!is.null(xInv)) {
    message("getting cached data")
    return (xInv)
  }
  data <- x$get()
  xInv <- solve(data, ...)
  x$setInverse(xInv)
}
