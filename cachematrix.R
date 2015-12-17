## Overall description

## Matrix inversion is usually a costly computation and there may be some benefit to caching
## the inverse of a matrix rather than computing it.

## Below are two functions that are used to create a special object that stores a matrix and caches its inverse.
## 1. makeCacheMatrix: creates a special "matrix" object that can cache its inverse.
## 2.	cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve 
## the inverse from the cache. 
## Note that that the matrix supplied should be square and is assumed to be invertible.

## Example
## x <- matrix(1:4,nrow=2,ncol=2)  # x is square and invertible matrix
## y <- makeCacheMatrix(x)         # y is special matrix in cache
## z <- cacheSolve(y)              # z is the inverse of x, directly from cache, or computed if not in cache
## x%*%z                           # multiplication results in Identity matrix

## The "makeCacheMatrix" function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## input:  "x" is matrix
  ## output: "list" with four functions: "set", "get", "setsolve", "getsolve"
  
  ## example:
  ## x <- matrix(1:4,nrow=2,ncol=2)
  ## y <- makeCacheMatrix(x)
  
  ## set the value of the matrix
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  
  ## get the value of the matrix
  get <- function() x
  
  ## set the value of the inverse of the matrix
  setsolve <- function(solve) s <<- solve
  
  ## get the value of the inverse of the matrix
  getsolve <- function() s
  
  ## output is a list with four functions
  list(set = set, 
       get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The "cacheSolve" function computes the inverse of the special "matrix" returned by makeCacheMatrix function.

cacheSolve <- function(x, ...) {
  
  ## input:  "x" is a special "matrix" returned by makeCacheMatrix function. "x" assumed to be invertible. 
  ## output: "s" is the inverse of matrix "x".
  
  ## example:
  ## x <- matrix(1:4,nrow=2,ncol=2)
  ## y <- makeCacheMatrix(x)
  ## z <- cacheSolve(y)
  ## x%*%z ## multiplication should result in Identity matrix
  
  ## Return a matrix that is the inverse of "x"
  s <- x$getsolve()
  
  ## Check if inverse has already been calculated (and matrix has not changed). 
  ## If so, get inverse from cache and skip computation. 
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  ## Otherwise, calculate inverse of the data and set inverse in cache via setsolve function.
  data <- x$get()
  s <- solve(data, ...)
  x$setsolve(s)
  s    
}
