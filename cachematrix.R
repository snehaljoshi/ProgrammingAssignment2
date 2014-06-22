#makeCacheMatrix function creates a matrix object

makeCacheMatrix <- function(x = matrix()) {
    xmt_inv = NULL
    
    
#Setting the Matrix function
    set <- function(m) {
      x <<- m
      xmt_inv <<- NULL
    }
#Getting the Matrix function
getmatrix <- function() x

#Setting the inverse of matrix function
setinv<- function(inv) xmt_inv <<-inv

#Getting the Inverse of matrix function
getinv <- function() xmt_inv
list(set = set, getmatrix = getmatrix,
     setinv = setinv,
     getinv = getinv)


}


## The function cacheSolve returns the inverse of matrix
## The function checks if the inverse is available in cache and retrives and returns the same inverse.
## If the inverse is not available, then the function computes it and sets the cache

cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
  x_in = x$getinv()
  
  #Check if cache already has inverse of the matrix
  
  if (!is.null(x_in)) {
    message("displaying cached inverse of the matrix")
    return(x_in)
  } 
  else {
    x_in = solve(x$getmatrix())
    x$setinv(x_in)
    return(x_in)
  }
}


# r=makeCacheMatrix(matrix(rnorm(100),10,10))
# cacheSolve(r)
# 
# f=solve(matrix(rnorm(100),10,10))
