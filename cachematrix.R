 
##This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
      ## Sets and gets value of matrix for subsetting in the cacheSolve function
    i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
    }
  get <- function() x
  setinv <- function(inverse) i <<- inverse
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


      ##This function returns a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
      
  i <- x$getinv()
      ## Check if i has a value set to it
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
      ## sets inverse of matrix to i
  i <- solve(data, ...)
  x$setinv(i)
  i
  
}

