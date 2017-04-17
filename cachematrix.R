##  Put comments here that give an overall description of what your
## functions do

##  There are two functions with the purpose of creating a special
##  object that stores a matrix and cache its inverse.

##  Write a short comment describing this function
##  makeCacheMatrix is a function that creates special matrix object that can cache 
##  its inverse, and returns a list of functions to:-

##  1) Set the values of the matrix
##  2) Get the values of the matrix
##  3) Set the inverse of the matrix
##  4) Get the inverse of the matrix


makeCacheMatrix <- function(x = matrix()) {
 inv <- NULL
 
# Set/ Get for Matrix
 set <- function (y){
   x <<- y
   inv <<- NULL
 }
 get <- function () x
 
 ## Get/Set for Matrix inverse
 setInverse <- function (inverse) inv <<- inverse
 getInverse <- function () inv

   # Return a list of functions for Matrix
  list (get=get, set=set, getInverse=getInverse, setInverse=setInverse)
}  
    
    


## Write a short comment describing this function
## cacheSolve function returns the inverse of the special matrix created by 
## makeCacheMatrix. If the inverse has already been calculated, then the function 
## directly gets the result and skip the computation.
## Otherwise, it calculates the inverse of the data and set the inverse matrix in
## the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 inv <- x$getInverse()
 
 ## If a cached value exist return it
 if(!is.null(inv)) {
   message ("getting cached data")
   return (inv)
 }
 
 ## Otherwise, calculate the inverse and store it in the cache.
 matrix <- x$get()
 inv <- solve (matrix, ...)
 x$setInverse(inv)
 inv
}
 



