## This function store the inverse of a matrix object in cache 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setMatInv <- function(solve) m <<- solve
  getMatInv <- function() m
  list(set = set, get = get,
       setMatInv = setMatInv,
       getMatInv = getMatInv)
  
}


## This function checks the inverse of a matix is present in the cache and returns that
## if not it will compute the inverse of the matrix and return it.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getMatInv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setMatInv(m)
  m
}