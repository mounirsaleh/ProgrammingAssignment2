## Put comments here that give an overall description of what your
## functions do

# These functions cache the inverse of a matrix

## Write a short comment describing this function
# makeCacheMatrix: This function generates a "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  Inv <- NULL
  set <- function(y) {
        x <<- y
        Inv <<- NULL
        }
  get <- function() x
  setInv <- function(inverse) Inv <<- inverse
  getInv <- function() Inv
  list(set = set, get = get,
      setInv = setInv,
      getInv = getInv)
}


## Write a short comment describing this function
# cacheSolve: This function computes the inverse of the "matrix" returned by makeCacheMatrix. 
# If the inverse has already been calculated without the matrix being changed, then the cachesolve
# retrieves the inverse from the cache.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Inv <- x$getInv()
    if(!is.null(Inv)) {
       message("getting cached data")
      return(Inv)
      }
   data <- x$get()
   Inv <- inverse(data, ...)
   x$setinverse(Inv)
   Inv

   }
