## The functions makeCacheMatrix and cacheSolve work 
## in tandem to store a user-supplied matrix and 
## return its inverse.

## makeCacheMatrix accepts a matrix and returns a list 
## of 4 functions: set, get, setinv (set Inverse), getinv (get Inverse)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) inv <<- inverse
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns the inverse of the "special" matrix
## created with makeCacheMatrix.
## If the inverse is already calculated, returns cached value.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
