## This pair of functions will first calculate the inverse of an input matrix,
## and then retrieves the cached value of the inverse upon subsequent calls.


## To cache the inverse value of a matrix, use makeCacheMatrix() with your matrix
## as its sole argument.

makeCacheMatrix <- function(x = matrix()) {
      i <- NULL
      set <- function(y) {
            x <<- y
            i <<- NULL
      }
      get <- function() x
      setinverse <- function(solve) i <<- solve
      getinverse <- function() i
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
}

## To calculate inverse of a matrix, use cacheSolve() with your matrix as its
## sole argument. If the inverse has been calculated previously, the cached
## result will be provided, along with a message notifying you that a cached
## value has been retrieved.

cacheSolve <- function(x, ...) {
      i <- x$getinverse()
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
}
