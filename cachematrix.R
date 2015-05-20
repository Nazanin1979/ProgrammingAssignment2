# Matrix inversion is costly computation. 
# Caching the matrix inversion is usefull for avoiding the repeatition. 
# Below two functions are used to cache the inversion of matrix. 

#makeCacheMatrix creats a special matrix that can cache its inverse:
#1. set the matrix
#2. get the matrix
#3. set the inverse of the matrix
#4. get the inverse of the matrix 

makeCacheMatrix <- function(x = matrix ()){
  inv <- NULL
  set <- function(y){
    #use <<- to assign a valuse to an object in an invironment different from the current environment.
    x <<- y
    inv <<- NULL
    }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function()inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Below function returns the inverse to matrix.
#1. check whether the inverse has already been computed
#2. If yes, it gets the result and skip the computation. 
#3. If not, it computes the inverse, set the value in the cache through setinverse function.

# This function supposes matrix is always invertible. 
cacheSolve <- function(x, ...){
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return (inv)
  }
myData <- x$get()
inv <- solve(myData)
set <- x$setinverse(inv)
inv
}


#example
## > x <- rbind(c(1, 0.5), c(0.5, 1))
## > m = makeCacheMatrix(x)
## > m$get()

## There is no cache in the first run. 
## > cachesolve(m)

## Retrieve the cache data in the second run.
## > cachesolve(m)
## > getting cached data
