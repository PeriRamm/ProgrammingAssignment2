# In this example we introduce the <<- operator which can be used to assign a value to an object in an
# environment that is different from the current environment. Below are two functions that are used to
# create a special object that stores a numeric vector and cache's its mean.
#
# `<<-` lets you search through parent environments, and if it does not find a variable with that name,
# it will create a new variable all the way up in the global environment. 
#
# The first function, makeVector creates a special "vector", which is really a list containing a function to:
# 1.set the value of the vector #resets the m, and stores its input to 'makeVector's environment(?)
# 2.get the value of the vector (locally stored)
# 3.set the value of the mean
# 4.get the value of the mean

makeVector <- function(x = numeric()) { # default argument is numeric(0), whatever this means and why....
  
  m <- NULL #sets the local m variable to NULL
  
  set <- function(y) {  #stores the x,m in the parent environment.
    # This isn't a necessary part of the exercise, and that's why it's not called!
    
    # This accesses the parent environment... Which is the +1 environment when DEFINED, not when CALLED.
    x <<- y   
    m <<- NULL
  }
  
  get <- function() x
  
  setmean <- function(mean) m <<- mean #after the mean has been calculated elsewhere....
  # ...it stores it to the parent environment, and returns it at the same time.
  
  getmean <- function() m
  
  list(set = set, get = get,
       setmean = setmean,
       getmean = getmean) #return object
  # this may be confusing: The lefthand sides are the names of the elements of the list we're creating...
  # ...the righthand side are the functions we have just defined.
  # The fact that they are the same is just for convenience!
}



rrr =makeVector("test")
rrr$get() #[1] "test" !!! 
rrr$set("test2") 
rrr$get() #[1] "test2"
rrr$getmean() #NULL
rrr$setmean() 
rrr$getmean() #NULL

testVec = c(1,2,3,4,5,6,7)
fours = c(4,4,4,4,4)

myVec = makeVector(c(1,2,3,4,5,6,7)) #this doesn't set the variables in the parent environment
myVec$get() # [1] 1 2 3 4 5 6 7
myVec$set(c(1,2,3,4,5,6,7)) #this does set the variables in the parent environment
myVec$get() # [1] 1 2 3 4 5 6 7
# how can we tell apart wether the variable in the parent environment has been set or not?????

myVec$getmean() #NULL

myVec$setmean() #setmean is intended to take as argument the already calculated mean!
myVec$getmean()

######################################################################################################################


# The following function calculates the mean of the special "vector" created with the above function.
# However, it first checks to see if the mean has already been calculated. If so, it gets the mean from
# the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value
# of the mean in the cache via the setmean function.

cachemean <- function(x, ...) { # the ellipsis is useless and probably only aesthetic here
  
  m <- x$getmean() 
  
  if(!is.null(m)) {
    # Mean has already been calulated and stored in the cache. We have already read it actually.
    message("getting cached data")
    return(m) #returns and never reaches further calculations in the function
  }
  
  # Mean hasn't been calculated, so we need to calculate and store it
  data <- x$get()
  m <- mean(data, ...) #this is where the mean actually gets calculated... The ellipses are just for syntax example, not useful in our case
  x$setmean(m) #and this is where it's stored in the parent environment
  #can be done in one line: x$setmean( mean(x$get(), ...) )
  
  m #return value
}


# lets try them together
myVec = makeVector(c(1,2,3,4,5,6,7)) #this doesn't set the variables in the parent environment

cachemean(myVec) # 4
cachemean(myVec) # getting cached data 4  This time it gets it from cached data!



######################################################################################################################
######################################################################################################################
######################################################################################################################



# cm <- makeCacheMatrix(matrix(rnorm(9), 3,3))

testMatr  =matrix(rnorm(9), 3,3)
solve(testMatr)

# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  inv <- NULL #
  
  set <- function(mat) {
    # This isn't a necessary part of the exercise, and that's why it's not called!
    # Setting the variables in the parent environment, the makeCacheMatrix() environment!
    x   <<- mat
    inv <<- NULL
  }
  
  get <- function() x
  
  setInv <- function(inverse) inv <<- inverse # after the inverse has been calculated elsewhere....
  getInv <- function() inv
  
  # return a list with all the functionalities
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

testCacheMatr = makeCacheMatrix(testMatr)
testCacheMatr$set(testMatr)
testCacheMatr$get()
testCacheMatr$getInv()

x= testCacheMatr

# makeVector <- function(x = numeric()) {
#   m <- NULL
#   set <- function(y) {
#     x <<- y
#     m <<- NULL
#   }
#   get <- function() x
#   setmean <- function(mean) m <<- mean
#   getmean <- function() m
#   list(set = set, get = get,
#        setmean = setmean,
#        getmean = getmean)
# }



######################################################################################################################


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
  inv <- solve(inputMatrix, ...) #this is where the Inverse actually gets calculated... 
  # The ellipsis seem not useful in our case. just for syntax example?
  x$setInv(inv)  #and this is where it's stored in the parent environment
  
  inv # Return a matrix that is the inverse of 'x'
}


# OVERALL TESTING
source("cachematrix.R")
testMatr = matrix(rnorm(9), 3,3)

testMatrObj = makeCacheMatrix(testMatr)
identical(testMatr,testMatrObj$get())

inv = cacheSolve(testMatrObj)
identical(inv,solve(testMatr))




# cachemean <- function(x, ...) { # the ellipsis is useless and probably only aesthetic here
#   m <- x$getmean() 
#   if(!is.null(m)) {
#     message("getting cached data")
#     return(m) #returns and never reaches further calculations in the function
#   }
#   data <- x$get()
#   m <- mean(data, ...) #this is where the mean actually gets calculated... The ellipses are just for syntax example, not useful in our case
#   x$setmean(m) #and this is where it's stored in the parent environment
#   m #return value
# }




#############################################

#dangerous assignment usage





caller <- function(x=10){
  add1toParent(x)
}
  

add1toParent <- function

























