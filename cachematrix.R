## A pair of functions that caache the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
  
  ## Initialize the inverse matrix
  m <- NULL
  
  ## Method to set the matrix
  set <- function(matrix) {
    x <<- matrix
    m <<- NULL
  }
  
  ## Method to get the matrix
  get <- function(){
    x
  } 
  
  ## Method to set the inverse
  setinverse <- function(inverse){
    m <<- inverse
  }
  
  ## Method to get the inverse
  getinverse <- function(){
    m
  }
  
  ## Return a list of the methods
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Computes the inverse of the special "matrix" returned by mackeCacheMatrix above
cacheSolve <- function(x, ...) {
  
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  ## Return the inverse when it is already set
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  ## Get the matrix
  data <- x$get()
  
  ## Compute the inverse 
  m <- solve(data) %*% data
  
  ## Set the inverse to the object
  x$setInverse(m)
  
  ## Return the matrix
  m
} 
