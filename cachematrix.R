## The "makeCacheMatrix" function takes a matrix 'x' and returns a list of four functions.
## The 'set(y)' function updates the value of 'x' with the argument 'y'.
## The 'get()' function retrieves the value of 'x' from the cache.
## The 'setinverse(inverse)' function sets the inverse of 'x' with the argument 'inverse'.
## The 'getinverse()' function retrieves the inverse of 'x' from the cache.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,
       get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## The function "makeCacheMatrix" takes an argument 'x', which should be a matrix.
## If the matrix 'x' is invertible, the function calculates its inverse and stores it in the cache.
## Subsequent calls to retrieve the inverse will return the stored value.
## If 'x' is not invertible, the function returns null.
## This function provides a convenient way to calculate and cache the inverse of a matrix,
## avoiding redundant calculations when the inverse is needed multiple times.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
        }
        originalmatrix <- x$get()
        msg <<- "calculated inverse"
        tryCatch(inv <- solve(originalmatrix),
                 error = function(e) {msg <<- "This matrix is not invertible"})
        x$setinverse(inv)
        message(msg)
        inv
}
