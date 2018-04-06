## The first function creates a matrix and it's possible to query the matrix
## with the get()-subfunction. The second function calculates the inverse
## and if it's already calculated, then it returns the cached results.
## Example of use:
## test_matrix <- makeCacheMatrix(matrix(1:4,2)) --creates the matrix
## test_matrix$get() --returns the created matrix
## cacheSolve(test_matrix) --inverses the matrix
## cacheSolve(test_matrix) --uses the cached inverse of the matrix

## Function to create a desired matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function() inv <<- solve(x)
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Function to calculate the inverse. Returns cached results if already calculated.
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv()
  inv
}

