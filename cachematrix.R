## Put comments here that give an overall description of what your
## functions do

##These functions implement a way to calculate the inverse of a matrix using cached values for
##improved performance

## Write a short comment describing this function

##makeCacheMatrix transform a matrix into a object that contains a cached inverse calculation
##return a list of function that set the values of the matrix, set the cache calculation,
##return the cache calculation

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Write a short comment describing this function

## cacheSolve calculates the inverse in the case there is no cached value 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
