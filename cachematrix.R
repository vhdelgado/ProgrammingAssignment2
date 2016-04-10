## The first function  a matrix that can cache its inverse 
## the second one computes the inverse of matrix returned by the first function
## if it is not calculated yet

## I create a special matrix that can run four functions
## with set we sen to cache, get bring back the matrix 
##setinverse and getinverse cumpute the inverse for the matrix 

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL #Store
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(get=get,
       set=set,
       setinverse=setinverse,
       getinverse=getinverse)
}


#takes the matrix created with the last funtion and
##calculates the inverse if it has not been calculated. 
## If it was created calls from cahce. If not it calculates and store. 

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data <- x$get() #recall the matrix
  inv <- solve(data,...) #solve
  x$setinverse(inv) #set
  inv #print the inverse
}
