## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  inversed_x <- NULL
  #1. set the value of the matrix
  set <- function(y) {
    x <<- y
    inversed_x <<- NULL
  }
  get <- function() x
  setinversed <- function(solve) inversed_x <<- solve
  getinversed <- function() inversed_x
  
  list(set = set, get = get,  setinversed= setinversed, getinversed = getinversed)
}


## Write a short comment describing this function
cacheSolve <- function(x) {
  inversed_x <- x$getinversed()
  if (!is.null(inversed_x)) {
    message("getting cached data")
    return(inversed_x)
  }
  data <- x$get()
  inversed_x <- solve(data)
  x$setinversed(inversed_x)
  inversed_x
  ## Return a matrix that is the inverse of 'x'
}