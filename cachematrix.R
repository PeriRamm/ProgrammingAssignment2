#
# These functions are VERY similar in form and functionality to the example functions:
# makeVector() and cachemean(). In fact, almost only the variable names differ!
#


# #Some testing code, for your convenience 
# source("cachematrix.R")
# testMatr = matrix(rnorm(9), 3,3)
# 
# testMatrObj = makeCacheMatrix(testMatr)
# identical(testMatr,testMatrObj$get()) # MUST BE TRUE
# 
# inv = cacheSolve(testMatrObj) # Will echo "getting cached data" whenever it does
# identical(inv,solve(testMatr)) # MUST BE TRUE



# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #Very important to delete the cache when creating a new instance!
  
  set <- function(mat) {
    # This isn't a necessary part of the exercise, and that's why it's not called!
    # It would be called though if makeCacheMatrix() was called without an argument.
    
    # Setting the variables in the parent environment, the makeCacheMatrix() environment!
    x   <<- mat
    inv <<- NULL
  }
  
  get <- function() x
  
  # Setting the variable in the parent environment, after the inverse was calculated elsewhere!
  setInv <- function(inverse) inv <<- inverse

  getInv <- function() inv
  
  # return a list with all the functionalities
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}




# This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
# If the inverse has already been calculated (and the matrix has not changed), then the cachesolve
# should retrieve the inverse from the cache.
cacheSolve <- function(x, ...) {
  
  inv <- x$getInv() #Attempt to read the pre-calculated inverse
  
  if (!is.null(inv)){
    # Inverse has already been calculated and stored in the cache. We have already read it actually.
    message("getting cached data")
    return(inv) #returns and doesn't reach further calculations in the cacheSolve() function
  }
  
  # Inverse hasn't been calculated, so we'll do it now.
  inputMatrix <- x$get()
  inv <- solve(inputMatrix, ...) #this is where the Inverse actually gets calculated
  # The ellipsis seem not useful in our case. just for syntax example?
  
  x$setInv(inv)  #and this is where it's stored in the parent environment
  
  inv # Return a matrix that is the inverse of 'x'. In quotes, because 'x' is not a matrix, it's containing a matrix.
}



