#Paulina 24.09.2020
#ProgrammingAssignment2 for Coursera course

makeCacheMatrix <- function(x = matrix()) { #initializes of empty matrix x
  inv <- NULL #sets inverse matrix as NULL
  set <- function(y){
    x <<- y #sets matrix y as a matrix x
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) inv <<- solve
  getinverse <- function() inv
  list(set = set, get = get, 
       setinverse = setinverse, 
       getinverse=getinverse)

}


cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv) ## Return a matrix that is the inverse of 'x'
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv ## Return a matrix that is the inverse of 'x'
}
