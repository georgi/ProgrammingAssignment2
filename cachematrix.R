## makeCacheMatrix and solveCache can be used to cache
## the matrix inverse computation. A matrix created by
## makeCacheMatrix is a
## special list containing functions to set and get
## the matrix and to get and set the inverse as well.
## solveCache will calculate the inverse and cache
## the result of that computation.


## Create a matrix which allows caching the inverse
## Return a list with four functions
## 1. set the matrix
## 2. get the matrix
## 3. set the inverse of the matrix
## 4. get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, 
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## cacheSolve returns the inverse of a special
## matrix created with the makeCacheMatrix
## results of the computation will be cached
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
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
