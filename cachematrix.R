
##	Purpose of this pair of functions is to cache an inverse value and make it
##	available if it has already been calculated, and if not, to calculate it.

## 	First function will create a special matrix that can cache its inverse:


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## 	Second function computes inverse of the matrix above.  If the inverse exists and 
##	the matrix has not changed, retrieve inverse from cache, otherwise recalculate and
##	store.

cacheSolve <- function(x, ...) {
  y<<- x
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  result <- x$get()
  inv <- solve(result, ...)
  x$setinverse(inv)
  inv
}
