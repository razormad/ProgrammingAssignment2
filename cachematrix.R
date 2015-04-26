# ------------------- makeCacheMatrix -------------------
# Function  is a function that returns a list of functions.
# It store a matrix and a cached value of the inverse of the 
# matrix. 
# Contains the following functions:
# * set:             set the Value of a matrix
# * get:             get the Vlue of a matrix
# * setInverse:      set the Value of the inverse matrix
# * getInverse:      get the Value of the inverse matrix

makeCacheMatrix <- function(x = numeric()) {
  #init cache NULL value if nothing is cached
  cache <- NULL  
  
  set <- function(y) {
    x <<- y
    cache <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(inv) cache <<- inv
  
  getInverse <- function() cache
  
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}



# ------------------- cacheSolve -------------------
# Calculate the inverse of the matrix created especially in the above function. 
# Used the stored result if it has been previously cached.

cacheSolve <- function(x, ...) {
  # get the cached value
  inv <- x$getInverse()
  
  # if the value cached exists exists return it
  if(!is.null(inv)) {
    message("return a cached value")
    return(inv)
  }
  
  #if not get the matrix
  data <- x$get()
  
  #calculate the matrix
  inv <- solve(data, ...)
  
  #store the matrix
  x$setInverse(inv)
  
  #return 
  inv
}

