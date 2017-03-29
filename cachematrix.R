## Matrix inversion could potentially be a costly computation and there may 
## be some benefit to caching the inverse of a matrix rather than compute it 
## repeatedly.  The functions in this file computes the inverse of a matrix
## and caches it.

## This function creates a special "matrix" object that can cache its inverse
## The function creates a list containing functions to:
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the value of the inverse of the matrix
## 4. gets the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y){
          x <<- y
          i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function () i
    
    list (set = set,
          get = get,
          setinverse = setinverse,
          getinverse = getinverse)
}


## This function returns the inverse of a matrix. It checks whether
## the inverse has already been calculated and if so, returns it.  If
## the inverse has not been calculated, it calls calculates the inverse, 
## and sets the cache value
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if (!is.null(i))
  {
    return(i)
  }
  
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  
  i
}
