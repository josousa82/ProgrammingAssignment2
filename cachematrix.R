## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

## In practice the function receives a matrix as argument, 
## and inside we have 4 functions, to manage that attribute (inversed matrix) inside 
## the main function.  The set functions are used to set values, and
## use the deep assignment operator to assign the received data
## structure, (in this case a matrix), to variables outside of the 
## function environment, or closure.
## Propose of the makeCacheMatrix() function is to take advantage of 
## the scoping rules to store a result in memory. Itch function has its own
## environment, and that will make it possible to store variables on the environment
## o itch one. Similar to the Object Oriented approach in Object Oriented Programing.
## For itch ‘instantiation’ of the function (behaving like an object), data is kept, 
## an ready for reuse. This technique avoids the fresh start  that R functions use.

makeCacheMatrix <- function(x = matrix()) {
  
  mat.inv <- NULL
  
  set <- function(mt) {
    x <<- mt
    mat.inv <<- NULL
  }
  
  get <- function() x
  
  set.inv <- function(mtinv){
    
    mat.inv <<- mtinv
    
  }

  get.inv <- function() mat.inv
  
  list(set=set, get=get, getinv=get.inv, setinv=set.inv)
}


## Write a short comment describing this function

## cacheSolve() function, it´s a very simple function that receives the matrix, 
## with its associated functions, from the makeCacheMatrix(). 
## The goal of this function is to solve the matrix or computing its inverse. 
## So, we don’t waste resources computing what was already computes, first this function try 
## to retrieve the already cached matrix with get.inv() function, and if it exists, it will just 
## return the already cached inversed matrix. If 
## a method in an object), it retrieves the matrix from the matrix built with 
## makeCacheMatrix(). 
## The first step is to check if the inverse of the matrix is already cached, in the 
## makeCacheMatrix() function, if it is, it returns the inversed matrix, and ends.
## This is accomplished with a simple control structure, an if() statement, 
## (if(!is.null(x$getinv()). In case that it x$getinv() returns null, than we
## proceed, and calculate the inverse of the matrix with the function solve,
## (solve(x$get()), next step is to cache the inverse of the matrix in the
## makeCacheMatrix() function environment, and return the inversed 
## matrix.
## one way to have conciseness of the scoping, and the environments here
## is to call the function get() and getinv() in the end, and confirm that not only 
## the inverted matrix is cached, but also the original, itch is its environments.



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  mat.inv <- x$getinv()
  
  if(!is.null(mat.inv)){
    message('Getting Data...')
    return(mat.inv)
  }
  mat.inv <- solve(x$get())
  x$setinv(mat.inv)
  mat.inv
  
}
