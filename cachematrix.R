## Two function which cache the inverse of a matrix
## in order to be able to save time if the matrix inverse is
## repeatedly needed as it takes time to compute


## makeCacheMatrix is a function to create a special list containing functions to:
## set the value of a matrix, get the value of a matrix,
## set the value of the inverse, get the value of the inverse
## based on the makeVector example provided
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a function to check if inverse is already calculated and if so, get mean from cache
## if not, set value of the inverse in the cache
## based on the cachemean example provided
cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    ## if chached data present then return that data
    message("getting cached data")
    return(m)
  }
  ## if no cached data then calculate and store in cache
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}
