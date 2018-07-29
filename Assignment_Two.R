## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
  inv <- NULL                             
  set <- function(y) {                   
    x <<- y                            
    inv <<- NULL                       
  }
  get <- function() x                  ## define the get fucntion - returns value of the matrix argument 
  setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
  getinverse <- function() inv                     ## gets the value of inv where called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}