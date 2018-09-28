## The functions makeCacheMatrix and cacheSolve cache the inverse 
## of a matix

## makeCacheMatrix makes an R object that can store a matrix (x) and
## the inverse of the matrix (im)

makeCacheMatrix <- function(x = matrix()) {
  im <- NULL
  set <- function(y) {
    x <<- y
    im <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) im <<- solve
  getsolve <- function() im
  list(set = set, get = get, setsolve = setsolve, getsolve = getsolve)
}


## cacheSolve either retrieves the inverse matrix from the input object,
## or if the inverse matrix has not been computed it gets the
## matrix from the input object, computes the inverse and cache it in 
## the input object

cacheSolve <- function(x, ...) {
  im <- x$getsolve()
  if(!is.null(im)) {
    message("getting cached data")
    return (im)
  }
  data <- x$get()
  im <- solve(data, ...)
  x$setsolve(im)
  im
        ## Return a matrix that is the inverse of 'x'
}
