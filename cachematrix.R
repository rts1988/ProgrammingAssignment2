## Put comments here that give an overall description of what your
## functions do
## makeCachematrix takes a square matrix as argument and creates a vector of functions that each return matrix values of the same dimension.
## set - takes as argument a square matrix and sets the new value of the matrix x, resets the value of the inverse to NULL
## get - gets the current value of the matrix x
## setinverse - takes as argument the calculated inverse and sets the value of the inverse.
## getinverse - returns the cached value of the inverse if not NULL. 



## makeCacheMatrix intializes the above list of functions for a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve takes as argument the list of functions computed in makecacheMatrix. computes the value of the inverse if it is not cached. 
## IF it is cached it returns the cached value and updates it in the cache vector.  

cacheSolve <- function(listfun, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- listfun$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- listfun$get()
  inv <- solve(data, ...)
  listfun$setinverse(inv)
  inv
  
}
