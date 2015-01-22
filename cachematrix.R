## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

#makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#set the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  
    m <- NULL
    set <- function(y) {
      x <<- y
      m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function
#inverts  the "matrix" created by makeCacheMatrix. 
#it first checks to see if the inverse has already been calculated.
# If so, it gets the inverse maxtrix from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the maxtri and sets the value of the maxtrix in the cache via the setinverse function.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}



