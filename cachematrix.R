## The functions makeCacheMatrix and cacheSolve will return the inverse of an invertible
## matrix. The inverse is cached so when asked to calculate the inverse of the same matrix
## it is retrieved from the cache rather than recalculated.

## The function makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions that - (1) sets the value of the matrix, (2) gets the value
## of the matrix, (3) sets the value of the inverse and (4) gets the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL 
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) inv <<- solve
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## The function cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix. If the inverse has already been calculated (and the matrix has not changed),
## then it is retrieved from the cache.

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("Getting cached data...")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv      ## Return a matrix that is the inverse of 'x'
}
