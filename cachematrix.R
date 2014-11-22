## Put comments here that give an overall description of what your
## functions do

## The function makeCacheMatrix(...) creates a set of functions that are used
## in cachesolve(). The created functions are capable of setting and getting matrix
## values, and also set/get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## Initializing i to NULL
  i <- NULL
  ## Defining the function set() to facilitate changing of the input matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ## Function get() returns the matrix 
  get <- function() x
  ## setinverse() sets the value of the cached inverse
  setinverse <- function(inverse) i <<- inverse
  ## Retrieve the value of the inverse:
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## The cunction cachSolve(...), either retrieves the cached inverse of...
## the matrix x (if it exists), or calculated the inverse and caches it for 
## future use.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  ##  Retrieving the cached inverse if it exists:
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data:")
    return(i)
  }
  ## Obtaining the data and calculating the inverse:
  data <- x$get()
  i <- solve(data, ...)
  ## Caching the inverse for future use:
  x$setinverse(i)
  i
}
