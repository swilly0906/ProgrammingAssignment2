## Put comments here that give an overall description of what your
## functions do
##A:I create 2 functions and learn lexical scoping.These functions not only make matrix to inverse,but also save the matrix
##which its inverse.

## Write a short comment describing this function
## First function makeCacheMatrix(): This function make the object of invrse matrix.But it is incomplete,so it need other function 
## cacheSolve(). 
makeCacheMatrix <- function(x = matrix()) {
          invmat <- NULL
          set <- function(y){
              x <<- y
              invmat <<- NULL
          }
          get <- function() x
          set_inv <- function(inv) invmat <<- inv
          get_inv <- function() invmat
          list(set=set, get=get, set_inv=set_inv, get_inv=get_inv)
}

## Write a short comment describing this function
## Second  function cacheSolve(): This function retrieves cache matrix data, and it can inverse 
## this matrix

cacheSolve <- function(x, ...) {
          invmat <- x$get_inv()
          if(!(is.null(invmat))){ 
                message("get cached matrix data")
                return(invmat)
          }
          data <- x$get()
          invmat <- solve(data,...)
          x$set_inv(invmat)
          invmat   ## Return a matrix that is the inverse of 'x'
}
m1 <- matrix(c(1,2,5,3,4,8,9,5,1),3,3)
m1_inv <-makeCacheMatrix(m1)
cacheSolve(m1_inv) #The output is m1's inverse matrix
cacheSolve(m1_inv) #get cached matrix data
m1_inv$set(matrix(c(1,3,5,3,5,8,9,9,1),3,3))
cacheSolve(m1_inv) #this outcome is matrix(c(1,3,5,3,5,8,9,9,1),3,3)'s inverse matrix
cacheSolve(m1_inv) #get  another cached matrix data