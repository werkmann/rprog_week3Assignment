## Documentation cachematrix.R
## This file contains functions to compute and cache the inverse of an invertible square matrix. Caching can save time if the inverse has to be computed several times. 
## To use this functionality, Create a cachable matrix m using makeCacheMatrix(). You can either use your own matrix object as a parameter here,
## or call m$set() later. In any case, this new object can now be used in repeated calls to cacheSolve() to take advantage of the caching behaviour.

## Create a cachable matrix object from the given R matrix, default. Used in tandem with cacheSolve().
makeCacheMatrix <- function(x = matrix()) {

  ## The methods below can refer to the variable 'inverse' because it is visible in their lexical scope.
  ## It will the inverse of the given matrix x after the first call to cacheSolve()
  inverse <- NULL 
  
  ## set a new matrix to be used by subsequent calls by the rest of the functions.
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  
  setInverse <- function(inv) inverse <<- inv
  getInverse <- function() inverse
  
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function returns the inverse of the given cachable matrix. For the first call the inverse is computed, subsequent calls return
## the cached matrix from the first call. Use makeCacheMatrix() to get a cachable matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInverse(inv) ## cache the inverse for later use
  inv
}

###
### Below are only functions for testing
###

testMatrix1 <- function()
{
  matrix(c(1,  0,  0, 
           0,  1,  0,
           0,  0,  1),
    nrow=3, ncol=3
    )
}
testMatrix2 <- function()
{
  matrix(c(-1,  3, -3, 
           0, -6,  5,
           -5, -3,  1),
         nrow=3, ncol=3
  )
}
testRun <- function()
{
    x <- makeCacheMatrix(testMatrix1())
    cacheSolve(x)
    cacheSolve(x)
}