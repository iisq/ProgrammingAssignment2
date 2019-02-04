## This file contains 2 functions for caching the inverse of a matrix. 
## 1. The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.
## 2. The cacheSolve fucntiong computes the inverse of the special "matrix" returned by makeCacheMatrix above.

## Create a special "matrix" object that can cache its inverse.
## Return a vector which contains set(), get(), setInverse(), getInverse() functions

makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      getInverse <- function() m
      list(set=set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
              message("getting cached data")
              return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setInverse(m)
        m
}
