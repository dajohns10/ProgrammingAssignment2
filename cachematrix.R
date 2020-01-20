## The following two functions makeCacheMatrix and cacheSolve
## are a pair of functions that cache the inverse of a matrix.
## since computing the inverse of a matrix can be a time consuming
## computation, there are benefits to cacheing the computed value
## for later use rather than recalculation each time.

## This function creates a special "matrix" object that can cache its inverse.
## The function creates a list of functions to set the contents of the matrix,
## get the contents of the matrix, set the inverse of the matrix, 
## and get the inverse of the matrix.  These "sub" functions are:
## set(), get(), setinverse(), and getinverse().

makeCacheMatrix <- function(x = matrix()) {
  ## x is matrix, how to set default in above
  matrixInverse <- NULL
  set <- function(newmatrix = matrix()) {
    x <<- newmatrix
    matrixInverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrixInverse <<- inverse
  getinverse <- function() matrixInverse
  
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed),
## then cacheSolve will retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  matrixInverse <- x$getinverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  data <- x$get()
  matrixInverse <- solve(data, ...)
  x$setinverse(matrixInverse)
  matrixInverse
}