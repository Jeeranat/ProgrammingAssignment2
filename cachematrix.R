

makeCacheMatrix <- function(x = matrix()) {
  #assign initial value for xinverse
  xinverse <-NULL
  
  #set the value of the matrix
  set <- function(y) {
    x <<- y
    xinverse <<- NULL
  }
  
  #get the value of the matrix
  get <- function() x
  
  #set the value of the inverse matrix
  setinverse <- function(inverse) xinverse <<- inverse
  
  #get the value of the inverse matrix
  getinverse <- function() xinverse
  list(set = set, get = get, setinverse = setinverse, 
       getinverse = getinverse)
  
}

## This function return a matrix that is the inverse of 'x'
cacheSolve <- function(x, ...) {
        
  xinverse <- x$getinverse()
  if(!is.null(xinverse)){
    message("getting cached data")
    return(xinverse)
  }
  data <- x$get()
  xinverse <- solve(data, ...)
  x$setinverse(xinverse)
  xinverse  
  
}
