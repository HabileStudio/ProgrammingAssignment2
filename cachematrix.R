# These functions generate the inverse of a matrix
# and cache the result

## This returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  # The inverse of the matrix
  inv <- NULL
  # Sets everything to initial conditions
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  # Returns the matrix
  get <- function() x
  # Sets the inverse of the matrix
  setInv <- function(inverse) inv <<- inverse
  # Returns the inverse of the matrix
  getInv <- function() inv

  # We return all defined functions inside a list
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}


## This is the caching function
## It either computes and cache the matrix inverse
## Or returns the cached version

cacheSolve <- function(x, ...) {
  # We look for the inverse in x
  inv <- x$getInv()
  # If we have already computed the inverse,
  if(!is.null(inv)) {
    message("Getting cached data...")
    # we return the cached version
    return(inv)
  }
  # Else, we get the original matrix,
  data <- x$get()
  # compute the inverse,
  inv <- solve(data, ...)
  # put it in cache,
  x$setInv(inv)
  # And finally return it
  inv
}
