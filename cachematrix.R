

## makeCacheMatrix

##
## This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {

   m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(solve) m <<- solve
  getInv <- function() m

   ## create the set and get accessors for this computed inverse
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
   
}

## cacheSolve
##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## returns the inverse of the 'x' matrix
##
## detailed approach computed today
## steps this function performs
## check and see if cache already has calculated the inverse
## else compute the inverse
##
## if X is a square invertible matrix, then solve(X) returns its inverse.
##
## For this assignment, assume that the matrix supplied is always invertible.
##
cacheSolve <- function(x,...) {

## check and see if cache already has calculated the inverse
  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached inverse data")
    ## return the cached version
    return(m)
  }

  ## else compute the inverse, start by fetching the data of the matrix
 ## data <- as.numeric(x$get())
 data <- x$get()
  ## invert it
  m <- solve(data, ...)

  ## cache it for future lookups
  x$setInv(m)
  
  ## return it
  m
 
}
