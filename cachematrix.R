## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function


## MakeCacheMatrix is a function with inner functions.
## it creates a special kind of matrix with set and get methods 
## Methods getinv and setinv are used by CacheSolve,
## getinv and setinv shouldn't be changed manually
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## Write a short comment describing this function
## Calculates the inverse of the special matrix 
## created by makeCacheMatrix. It uses the methods 
## getinv and setinv to get and write data to the 
## special matrix. If matrix is already used then 
## it will indicate that is getting cached data.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ## function solve is actually the one that calculates the inverse
  m <- solve(data, ...)
  x$setinv(m)
  m
}


##Code for testing the functions
tau <- c(0, 0, 0, 0, 1,
            1, 0, 0, 0, 0,
            0, 1, 0, 0, 0,
            0, 0, 1, 0, 0,
            0, 0, 0, 1, 0)
tau <- matrix(tau, ncol=5, byrow=TRUE) 
class(tau) ## verify it is a matrix
a<-makeCacheMatrix() ##create the matrix
a$set(tau) ## using method set to fill the matrix a
a$get() ## verify the matrix
##  You should get
##      [,1] [,2] [,3] [,4] [,5]
##      [1,]    0    0    0    0    1
##      [2,]    1    0    0    0    0
##      [3,]    0    1    0    0    0
##      [4,]    0    0    1    0    0
##      [5,]    0    0    0    1    0
##
cacheSolve (a)
# Shows result of calling cacheSolve 1st time
# [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    0    0    0
# [2,]    0    0    1    0    0
# [3,]    0    0    0    1    0
# [4,]    0    0    0    0    1
# [5,]    1    0    0    0    0
cacheSolve (a)
# Shows result of calling cacheSolve 1st time
# getting cached data
# [,1] [,2] [,3] [,4] [,5]
# [1,]    0    1    0    0    0
# [2,]    0    0    1    0    0
# [3,]    0    0    0    1    0
# [4,]    0    0    0    0    1
# [5,]    1    0    0    0    0
