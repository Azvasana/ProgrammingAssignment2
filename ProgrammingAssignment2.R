makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(matrix) {
    x <<- matrix
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  cache_inverse <- function() {
    if (!is.null(inverse)) {
      message("Returning cached inverse.")
      return(inverse)
    }
    message("Computing inverse and caching.")
    inverse <- solve(x)
    return(inverse)
  }
  list(set = set, get = get, cache_inverse = cache_inverse)
}
cacheSolve <- function(matrix, ...) {
  if (!inherits(matrix$cache_inverse, "function")) {
    stop("Input is not a valid cached matrix object.")
  }
  matrix$cache_inverse()
}



