## The following functions aimed to reduce computational resource in solving
## matrix inversion repeatedly.

## Assumption: The supplied matrix is always invertible

## The makeCacheMatrix function is used to create a special object that stores a matrix 
## and caches its inverse.
## The result is a List of four (4) useable functions:
## - set: Stores the Matrix
## - get: Retrieves the stored Matrix
## - setinverse: Stores the Inverse of the Matrix
## - getinverse: Retrieves the stored Inverse of the Matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("Cached inverse of the matrix...")
    return(inv)
  }
  
  message("Calculating the inverse of the matrix for the first time...")
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  return(inv)
  
}

## Usage:
##
## > M <- matrix(rnorm(9),3,3)
## > M2 <- makeCacheMatrix(M)
## > M2$get()
##            [,1]         [,2]      [,3]
## [1,]  0.4636541 -0.772961625 0.9692373
## [2,] -0.8618837 -0.512527884 0.4893423
## [3,] -1.0846288 -0.002187224 2.5423706
## > cacheSolve(M2)
## Calculating the inverse of the matrix for the first time...
##            [,1]       [,2]        [,3]
## [1,]  0.5370884 -0.8097935 -0.04889132
## [2,] -0.6849803 -0.9199401  0.43820286
## [3,]  0.2285439 -0.3462664  0.37285261
## > cacheSolve(M2)
## Cached inverse of the matrix...
##            [,1]       [,2]        [,3]
## [1,]  0.5370884 -0.8097935 -0.04889132
## [2,] -0.6849803 -0.9199401  0.43820286
## [3,]  0.2285439 -0.3462664  0.37285261
## > 
