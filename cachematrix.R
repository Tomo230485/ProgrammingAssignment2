## The below function overall enables users to cache the inverse matrix and 
## extract the result if the value you try to extract is the same as the cached value. 
## By doing so, it improved the efficiency of the programme as it no longer needs to calculate repeatedly.


## makeCacheMatrix function collaborates with cacheSolve function.
## The purpose of this function is to have number of functions to be used in the cacheSolve
## as well as storing the value once calculated by utilising the scoping rule in R.
makeCacheMatrix <- function(x = matrix()) {

## initialise the stored_matrix variable to NULL.
  cacheMatrix <- NULL
  
## Value of x overwritten as y in its parent environment so does for cacheMatrix.
  set <- function(y=NULL) {
    x <<- y
    cacheMatrix <<- NULL
  }

## return the value of X.
  get <- function() x
  
## store the inversematrix to chacheMatrix to extract next time.
  setinvmt <- function(inverse) cacheMatrix <<- inverse
  
## return the value of cacheMatrix
  getinvmt <- function() cacheMatrix
  
## list all the above functions to call individually by using $.
  list(get= get, setinvmt = setinvmt, getinvmt = getinvmt)
}    


## The below function returns the value of cached inverse matrix if the value is saved in the vector.
## If none, calculate the inverse matrix for matrix x, return the value and cached the value to cacheMatrix.
cacheSolve <- function(x, ...) {
  cacheMatrix <- x$getinvmt()
  if(!is.null(cacheMatrix)) {
    message("Downloading cached inverse matrix")
    return(cacheMatrix)
  } 
  data <- x$get()
  cacheMatrix <- solve(data)
  x$setinvmt(cacheMatrix)
  return(cacheMatrix)
}
