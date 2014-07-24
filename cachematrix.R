
## Make Vector function
makeVector <- function(x = numeric()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setmean <- function(mean) m <<- mean
        getmean <- function() m
        list(set = set, get = get,
             setmean = setmean,
             getmean = getmean)
}

## cachemean function
cachemean <- function(x, ...) {
        m <- x$getmean()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- mean(data, ...)
        x$setmean(m)
        m
}

## makeCacheMatrix

##
## This function creates a special "matrix" object that can cache its inverse.
##
makeCacheMatrix <- function(x = matrix()) {

  m <- x$getInv()
  if(!is.null(m)) {
    message("getting cached inverse data")
    return(m)
  }
  ## fetch the data of the matrix
  data <- x$get()
  
  ## invert it
  m <- solve(data, ...)

  ## cache it
  x$setInv(m)
  
  ## return it
  m
  
}

## cacheSolve
##
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.
##
## returns the inverse of the 'x' matrix
##
cacheSolve <- function(x,...) {

  ## check and see if cache already has calculated the inverse
  
  ## else compute the inverse
  ##
  ## if X is a square invertible matrix, then solve(X) returns its inverse.
  ##
  ## For this assignment, assume that the matrix supplied is always invertible.
  ##
  Xinv <- solve(x)
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInv <- function(inverse) m <<- inverse
  getInv <- function() m
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)

  ## cache the computed inverse 

}
