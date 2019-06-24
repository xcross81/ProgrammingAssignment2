## Used for creating a matrix that stores its inverse
## Includes function for calculating and setting inverse


##Creates a list with functions that can set the matrix,
## show the matrix, and set/show the matrix inverse
makeCacheMatrix <- function(x = matrix()) {
  
  invx <- NULL          #sets stored inverse null
  set <- function(y) {
    x <<- y             #sets the stored value of matrix
    invx <<- NULL       #if matrix changes, clear stored inverse
  }
  value <- function() x #value of the matrix
  inverse <- function(inversedata=NULL){
    if (is.null(invx)) {
      invx <<- inversedata
      invx
    }
    else{
      invx
    }
  }
  
  
  list(set= set, value = value, inverse = inverse)

}


## cacheSolve takes in a matrix made from an object
## untilizing makeCacheMatrix, returns the matrix inverse
## from cache if already calculated and cached, otherwise
## calculates inverse and caches

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  inverse <- x$inverse()
  if(!is.null(inverse)){
    message("Using cached data")
    inverse
  }
  else{
    matrixdata <- x$value()
    inverse <- solve(matrixdata)
    x$inverse(inverse)
    inverse
  }
  
  
}

####makeCacheMatrix addition notes
## When the matrix is set, the inverse (if set) is cleared
## in order to prevent a wrong inverse value from staying stored
## The value function shows the value of the stored matrix
## 
## The inverse function will return the value of the
## stored inverse or set(and return) the value of the inverse
## if the inverse hasn't already been assigned. The
## default argument value for the inverse function is NULL.

## Usage example
## specialmatrix <- makeCacheMatrix()
## matrixdata <- matrix(c(1,8,5,7,9,3,2,4,3),nrow=3,ncol=3)
## specialmatrix$set(matrixdata)
## specialmatrix$value()
## specialmatrix$inverse(inversedata)
## cacheSolve(specialmatrix)