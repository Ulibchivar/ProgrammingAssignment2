##2 step function. 1st step vreates a matrix and inverse it.
## 2nd step caches it if it is already inverted

## Creating a matrix 

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
     x <<- y
     m <<- NULL
  }
  
   get <- function()x
   setinverse <- function(inv) m <<- inv
   getinverse <- function() m
   list(set = set , get = get,
       
       setinverse = setinverse,
       getinverse = getinverse)
  
}




## Calculating the inverse of the matrix above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  ##If inverse already calculated, then will use the cached value
  
  data <- x$get()
  
   m <- solve(data, ...)
   x$setinverse(m)
   m
}