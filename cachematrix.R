## COURSERA: R Programming_Week3_Assignment
## Author: Sahil 

## Assignment Name: Caching the inverse of a matrix
## Assignment Gist: Matrix inversion is usually a costly computation and there maybe some benefit to caching the
## the inverse of a matrix rather than compute it repeatedly (there are also alt. to matrix inversion).  
## The assignment is to write a pair of functions that cache the inverse of a matrix. 
## Computing the inverse of a square matrix can be done with solve function. If 'x' is a square invertible matrix then 'solve(x)' returns its inverse.

##Example:
##testmatrix1 <- makeCacheMatrix(matrix(c(1,1,2,4),nrow=2,ncol=2))  #New 2*2 matrix
##testmatrix1$get()  #Returns matrix result
##testmatrix1$getinverse()  #Returns NULL
##testmatrix1$set(matrix(1:4,2,2)) #New 2*2 matrix
##testmatrix1$get() #Returns matrix result
##cacheSolve(testmatrix1) #Inverse of matrix
##testmatrix1$getinverse() #Inverse of matrix

##Function1: makeCacheMatrix:  
## creates a special matrix object that can cache its inverse.
## makeCachematrix function is used to create a matrix.
## setInverse sets the inversed matrix. getInverse returns the inversed matrix. 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse)
    m <<- inverse
  getInverse <- function()
    m
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

## Function 2: cacheSolve:
## cacheSolve function calculates the inverse of the matirx. 
## If the inverse has been calculated, then cacheSolve function retrieves the inverse from the cache. 
## x$getInverse gets the inverse result and if not caluclated will be null. x$setInverse sets the result to the object which is returned. 

cacheSolve <- function(x, ...) {
  n <- x$getInverse()
  if(!is.null(n)){
    message("getting cached data")
    return(n)
  }
  data <- x$get()
  n <- solve(data,...)
  x$setInverse(n)
  n
}


