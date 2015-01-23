## Put comments here that give an overall description of what your
  ## functions do
##makeCacheMatrix creates a new environment in which a list object 
  ##of 4 items (get,set,invertmatrix, getmatrix) is stored.

##cacheSolve inputs a cached matrix and returns its inverse

## Write a short comment describing this function
  ##makeCacheMatrix inputs a matrix and stores it in a variable in a different
      ##environment/ scope.

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) { 
    x <<- y 
    m <<- NULL
  }
  get <- function() x 
  invertmatrix <- function(matrix) m <<- matrix 
  getmatrix <- function() {m}
  list( set = set, get = get,invertmatrix = invertmatrix, getmatrix = getmatrix)
}


## Write a short comment describing this function
## cacheSolve inputs a cached list, searches for a particular matrix, 
## and goes into that environment and retrieves.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getmatrix() 
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$invertmatrix(m)
  m
}