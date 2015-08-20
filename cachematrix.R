## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix creates a special "vector", which is really a list containing a function to
##
##   set the value of the vector
##   get the value of the vector
##   set the value of the inverse matrix
##   get the value of the inverse matrix


makeCacheMatrix <- function(x = matrix()) {
    # initialize m value to NULL
    m <- NULL
    
    # create set function
    set <- function (y) {
        x <<- y
        m <<- NULL
    }
    
    # create get function
    get <- function() x
    
    # crate set_inverse function
    set_inverse <- function(inv) m <<- inv
    
    # crate set_inverse function
    get_inverse <- function() m
    
    # create return value with set function, get function, 
    # set_inverse function, & get_inverse function
    list (set=set, get=get, set_inverse=set_inverse, get_inverse=get_inverse)
}


## cacheSolve will check to see if the matix that was input has already been inverted
##               if it has the funciton will return the cached version, if not it will
##               invert the data, cache and return the new matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
  # get stored matrix      
  m <- x$get_inverse()
  
  # if stored value is null return cached value
  if(!is.null(m)) {
      message("getting cached data")
      return(m)
  }
  
  # get empty matrix
  data <- x$get()
  
  # get inverse of matrix
  m <- solve(data, ...)
  
  # store inverse of matrix
  x$set_inverse(m)
  
  # return inverted matrix
  m        
}
