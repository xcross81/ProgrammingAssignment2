## Creates a list with function that can set the matrix,
## show the matrix
## and set/show the matrix inverse
##
## When the matrix is set, the inverse (if set) is cleared
## in order to prevent a wrong inverse value from staying stored
## The value function shows the value of the stored matrix
## 
## The inverse function will return the value of the
## stored inverse or set(and return) the value of the inverse
## if the inverse hasn't already been assigned. The
## default argument value for the inverse function is NULL.
##
## Usage example
## specialmatrix <- makeCacheMatrix()
## matrixdata <- matrix(c(1:9),nrow=3,ncol=3)
## specialmatrix$set(matrixdata)
## specialmatrix$value()
## specialmatrix$inverse(inversedata)



## Write a short comment describing this function

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


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
}
