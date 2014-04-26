## These functions help to cache calculated inverted matrices in
## order to avoid repetiotion of the time-consuming operation
##
## Test example and example of usage:
## m1 <- matrix(c(1,1,0, 0,1,0, 0,0,1), 3, 3)
## The following output should be equal:
## m1
## cacheSolve(makeCacheMatrix(cacheSolve(makeCacheMatrix(m1))))

## makeCacheMatrix keeps data of original and inverted matrices
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(solve) inv <<- solve
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve returns an inverted matrix, retrieving it from the
## cache if possible
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinv(inv)
  inv
}
