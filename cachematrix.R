## These functions are able to cache the result of matrix inverse, a potentially
## time-consuming computations for big matrix.
## The inverse is computed only the first time, and if the data don't change,
## its value is looked up in the cache rather than recomputed.
##
## Example:
##   Create a matrix
##     > A = matrix( c(2, 4, 3, 1, 5, 7, 3, 5, 9), nrow=3, ncol=3, byrow = TRUE)
##   Create a cache matrix:
##     > Ac = makeCacheMatrix(A)
##   Compute the inverse with the cache matrix:
##     > cacheSolve(Ac)
##     ... (inverse output)
##   Recompute the inverse with the cache matrix as many time as needed:
##     > cacheSolve(Ac)
##     getting cached data
##     ... (inverse output)

## makeCacheMatrix creates a special "matrix", which is really a list containing
## a function to:
## set the value of the matrix (set)
## get the value of the matrix (get)
## set the value of the matrix inverse (setinverse)
## get the value of the matrix inverse (getinverse)
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}

## cacheSolve calculates the inverse matrix of the special "matrix" created with
## the above function. It first checks to see if the inverse has already been calculated.
## If so, it gets the inverse from the cache and skips the computation. Otherwise,
## it calculates the inverse of the data and sets the value of the inverse in the cache
## via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
