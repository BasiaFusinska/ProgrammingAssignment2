## The makeCacheMatrix function is creating a special matrix object
## that provides functions to set and get the matrix values
## and to get and set the inverse functions

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
}


## The cacheSolve function is using the special matrix object 
## to perform inversion if it's not found in the cache
## otherwise the inverted value is taken from the cache 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, diag(nrow(data)), ...)
  x$setsolve(m)
  m
}
