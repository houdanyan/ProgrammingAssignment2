## This function is a matrix than put in cache the inverse


## we create a matrix and define the inverse

makeCacheMatrix <- function(x = matrix()) {
  mat <- Null                      
  set <- function(y) {                 
    x <<- y 
    mat <<- Null 
  }                                               
  get <- function() x                    
  
  setinverse <- function(inverse) mat <<- inverse  
  getinverse <- function() mat                     
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  
}


## and then we return the matrix of the inverse. IF it has already been 
## generated, the result is taken from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getinverse()
  if(!is.null(mat)) {
    message("getting cached data")
    return(mat)
  }
  data <- x$get()
  mat <- solve(data, ...)
  x$setinverse(mat)
  mat
}
