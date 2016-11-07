## Functions compute the inverse of a matrix asuming it it invertible
## It stores it so the computation of the inverse can be faster. 

## This function computes the inverse of the matrix and stores it for later use. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}

## This function computes the inversion of the matrix but it first checks if it has been computed first.
## If it has been computed first then it returns the inverse of the matrix. 

cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setsolve(m)
  m
}


