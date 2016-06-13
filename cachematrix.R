
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inverse) m <<- inverse 
  getinv <- function() m
  list(set=set, get=get, setinv=setinv, getinv=getinv)
}


## computes the inverse of the "matrix" returned by makeCacheMatrix(). If the inverse has already been calculated and the matrix has not changed, it'll retrieves the inverse from the cache directly.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if (!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat.data <- x$get()
  m <- solve(mat.data, ...)
  x$setinv(m)
  
  return(m)
}
