## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix takes a matrix, and creates 4 functions
## set is useful if we want to change the matrix associated with a particular instance of makeCacheMatrix
## get retrieves the matrix associated with a particular instance
## setinverse is useful when we compute the inverse for a particular instance in cacheSolve if we don't already know it
## getinverse retrieves the inverse if we have already stored it
## all results are stored in the new 'cache' environment

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## cachesolve takes an instance of the makeCachematrix class
## if the inverse is already stored, it simply retrieves it from the cache using getinverse()
## if the inverse is not stored, it calculates it by retrieving the matrix for the instance (get) and then using the solve function
## it then sets the getinverse() attribute to the new calculated inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
  
}
