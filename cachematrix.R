## This function will inverse a matrix, but before each inverse is computed,
## this function will check the cache to see if this has already been done in
## the past. If this is the case, it will take this result, instead of
## computing it again

makeCacheMatrix <- function(x = matrix()) {
  inverse_matrix <- NULL
  set <- function (y) {
    x <<- y
    inverse_matrix <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverse_matrix <<- inverse
  getinverse <- function() inverse_matrix
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  inverse_matrix <- x$getinverse()
  if(!is.null(inverse_matrix)) {
    message("getting cached data")
    return(inverse_matrix)
  }
  data <- x$get()
  inverse_matrix <- solve(data, ...)
  x$setinverse(inverse_matrix)
  inverse_matrix
}
  
