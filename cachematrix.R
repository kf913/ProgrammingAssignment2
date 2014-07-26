# These are two functions that are used to create an object that stores a matrix and caches its inverse

# makeCacheMatrix is a function that creates a special "Matrix" object that can cache its inverse.
#  This is actually a list containing functions to
#      1. set the value of the matrix
#      2. get the value of the matrix
#      3. set the value of the inverse of the matrix
#      4. get the value of the inverse of the matrix

makeCacheMatrix <- function(x = numeric()) {
  i <- NULL
  set <- function(y) {  # Assigns the passed value to x
    x <<- y  
    
    i <<- NULL          # Since x may have changed, the inverse of x may have changed.
                        #    Therefore, i is set to NULL
  }
  
  get <- function() x   # Returns the value of x (which was set at creation or assigned using set() )
  
  setinverse <- function(inverse) i <<- inverse
                        # Assigns the passed value to i (the inverse of x)
                        # i represents the stored inverse of matrix x
  
  getinverse <- function() i
                        # Returns the value of i (which was assigned using the function setinverse)
                        # If x has changed, and setinverse has not been called, i should be NULL
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
                        # Simply puts these functions into a list and returns them
}

# This function returns the inverse of the "special" matrix created with makeCacheMatrix
# If this is the first time the inverse is requested for the matrix, the inverse is computed using
#    solve and cached (in this special matrix)
# If the inverse is cached already, then it is returned, with a message saying "getting cached data"

cacheSolve <- function(x, ...) {
  i <- x$getinverse()   # Checks the cached "inverse of the special matrix" using getinverse
  
  if(!is.null(i)) {     # If the cached inverse exists (i.e., not NULL), then it prints the 
                        #   message "getting cached data" and returns the cached value
    message("getting cached data")
    return(i)
  }
  
  data <- x$get()       # If the cached value does not exist, get the value of the matrix, compute
  i <- solve(data, ...) #    its inverse, store it, and return it.
  x$setinverse(i)
  i
}
