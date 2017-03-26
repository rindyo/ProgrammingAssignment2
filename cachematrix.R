## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" oject that can cache its invers.  

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y){
    x <<-y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list (set = set, get = get, 
        setinverse = setinverse, 
        getinverse = getinverse)
  }


## cacheSolve:  This function computes the invese of the special "matrix" returned by 
## MakeCahceMatrix above. If the inverse has already been calculated (and the matrix has 
## not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of the matrix stored in 'x$get'
      i <- x$getinverse()
      if (!is.null(i)) {
            message("getting cached data")
            return(i)
      }
      data <- x$get()
      i <- solve(data, ...)
      x$setinverse(i)
      i
  }
