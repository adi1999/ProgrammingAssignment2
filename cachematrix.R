## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve          ##creates an anonymous function to get the inverse
  getinverse <- function() m                         ##creates an anonymous function to get data from the cache
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {                   ##checks if an inverted marix already exits
    message("getting cached data")
    return(m)                         ##returns the cached matrix, if found
  }
  data <- x$get()                     ##solves the inverse if there is no data in the cache
  m <- solve(data, ...)
  x$setinverse(m)
  m
  }
