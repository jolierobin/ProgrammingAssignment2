#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()){
  
  ## Set matrix
  m <- NULL
  set <- function (y) {
    x <<- y
    m <<- NULL
  }
  
  ## Get inverse of matrix
  get <- function() x
  
  ## Set value of inverse matrix
  set_inv <- function(inv) m <<- inv
  
  ## get value of inverse matix
  get_inv <- function() m 
  
  list (set = set, get=get,
  set_inv =set_inv
  get_inv = get_inv)
}

  # Function that returns the inverse of of a matrix. To reduce redundant computing, this function will first check 
  # whether the answer is already in cache. If so, it will return the cached data instead of computing it again. 
  cacheSolve <- function(x, ...) {
  m <- x$get_inv()
 
  ##If a inverse matrix is cached, return this. Otherwise compute inverse matrix and return
  if(!is.null(m)){
    message("Getting cached data")
    return(m)
    
  }

  else { 
    ##calculating inverse of matrix x
    data <- x$get()
    m <- inv(data, ...)
    x$set_inv(m)
    m 
  }
  
}
