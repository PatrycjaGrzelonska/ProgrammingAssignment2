## function makeCacheMatrix takes a 2x2 matrix as an input
## and returns a list of four functions:
## 1)set
## 2)get
## 3)setinv
## 4)getinv
makeCacheMatrix <- function(x = matrix()) {
  
  ## m is a placeholder, initially it's set to "0" matrix
  m <- matrix(0,2,2)
  
  ## set is a 1) function that let us change the matrix
  ## for which we want to compute the inverse matrix
  ## it also resets the placeholder to "0" matrix
  set <- function(y) {
    x <<- y
    m <<- matrix(0,2,2)
  }
  
  ## get is a 2) function that returns matrix x
  get <- function() {x}
  
  ## setinv is a 3) function that sets placeholder m equal to 
  ## matrix z
  setinv <- function(z) {m <<- z}
  
  ## getinv is a 4) function that returns m
  getinv <- function() {m}
  
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}

## cacheSolve is a function that checks if the inverted x exists and if so it returns it from a cache
## otherwise it computes it and caches it
cacheSolve <- function(x, ...) {
  m <- x$getinv() ##taking matrix from a cache
  if(!identical(m,matrix(0,2,2))) { ##if it's not a "0" matrix, return it and finish
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...) ##if it's "0" matrix than compute the inverse of x, return it and finish
  x$setinv(m)
  m
}