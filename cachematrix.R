## This function creates a cached matrix object and returns it as a list.
## The list contains four functions: set, get, setsolve, and getsolve.
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL  # initialize inverse of matrix to NULL
  set <- function(y) {
    x <<- y  # set value of matrix to y
    s <<- NULL  # reset inverse of matrix to NULL
  }
  get <- function() x  # return value of matrix
  setsolve <- function(solve) s <<- solve  # set value of inverse of matrix to solve
  getsolve <- function() s  # return value of inverse of matrix
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)  # return list of functions
}

## This function takes a cached matrix object as input and returns its inverse.
cacheSolve <- function(x, ...) {
  s <- x$getsolve()  # get value of inverse of matrix from cached object
  if(!is.null(s)) {  # if inverse of matrix is cached
    message("getting cached")
    return(s)  # return cached value of inverse of matrix
  }
  data <- x$get()  # get value of matrix from cached object
  s <- solve(data, ...)  # calculate inverse of matrix
  x$setsolve(s)  # cache value of inverse of matrix
  s  # return calculated value of inverse of matrix
}
