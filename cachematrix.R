
## INV

makeCacheMatrix <- function(x = matrix()) {
  inverse  <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() {
    x
  }
  setinverse <- function(inv){ 
    inverse <<- inv
  }
  getinverse <- function(){
    inverse
  } 
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## INV_CACHE

cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  data <- x$get()
  inverse <- solve(data, ...)
  x$setinverse(inverse)
  inverse
}
