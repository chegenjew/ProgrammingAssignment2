## These are two functions that will be used to cache the inverse of a matrix 
## the makecachematrix returns a list of functions  
## The cachesolve function will check if the inverse of the currently supplied matrix is cached and if not the 
# function will calculate the inverse and store it 

# Define the makeCacheMatrix 

makeCacheMatrix <- function(x = matrix()) {
  #initialize the inverse of the matrix
  myinv <- NULL
  
  #define a setter function to initialize the matrix and the inverse of the matrix to NULL
  setmat <- function(y, ...){
    x <<- y 
    myinv <<- NULL
  }
  
  #define a getter function that returns the matrix 
  getmat <- function()x 
  
  #define a getter function that returns the cached inverse
  getinv <- function()myinv
  
  #define a setter function that sets the inverse
  setinv <- function(theinv) myinv <<- theinv
  
  #return  a list of functions for the makeCacheMatrix 
  list(setmat = setmat, getmat = getmat, 
       getinv = getinv, setinv= setinv)

}


## the function returns cached inverse or calculates and stores the inverse otherwise 

cacheSolve <- function(x, ...) {
  #retrieve the stored cache
  myinv <- x$getinv()
  
  #check if the inverse has been previously stored
  if (!is.null(myinv)){
    message("Printing cached inverse")
    return(myinv)
  }
  
  #retrieve the stored matrix 
  data <- x$getmat()
  
  #calculate the inverse of the data 
  myinv <- solve(data, ...)
  
  #cache the inverse 
  x$setinv(myinv)
  
  #return the inverse 
  myinv
}
