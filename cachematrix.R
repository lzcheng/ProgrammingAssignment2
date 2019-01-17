##writing a pair of functions that cache the inverse of a matrix.


#The makeCacheMatrix function creates a list containing 
#four functions that:
#1.set the value of the matrix
#2.get the value of the matrix
#3.set the value of the inverse
#4.get the value of the inverse

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

#This function computes the inverse of the special 
#"matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated and 
#the matrix has not changed, then the cachesolve 
#should retrieve the inverse from the cache.


cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
  # Return a matrix that is the inverse of 'x'
}

#testing example
mat<-cbind(c(1,3),c(0,1))
#solve(mat)
temp<-makeCacheMatrix(mat)
cacheSolve(temp)
