## the following code creates a special matrix via a function called makeCacheMatrix that 
## will work in conjunction with the cacheSolve fuction to return the inverse of a 
## matrix


makeCacheMatrix <- function(x = matrix()) {
  M <- NULL
  set <- function(y) {
    x <<- y
    M <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) M <<- solve
  getinverse <- function() M
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}
