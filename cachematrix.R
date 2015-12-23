# MakeCacheMatrix function  with get, set , set inverse and get inverse functions
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# sample value for running the function
x = rbind(c(1, 2), c(2, 1))
m = makeCacheMatrix(x)
m$get()

# Cache solve function with message displyed as "getting cached data for clarity
cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

cacheSolve(m)