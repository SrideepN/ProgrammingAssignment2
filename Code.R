
##The input should be a matrix which is specified as x
makeCacheMatrix <- function(x = matrix()) {
  ##The the output is specified as null i.e. i
    i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  ##Here is to find the inverse of inputed matrix 
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

CacheSolve <- function(x, ...) {
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinv(i)
  i
}