## Put comments here that give an overall description of what your
## functions do

##makeCacheMatrix creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()){
  m <- NULL
  ##set the value of the matrix
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  ##get the value of the matrix
  get <- function() x
  ##set the value of inverse of the matrix
  setinverse <- function(inverse) m <<- inverse
  ##get the value of inverse of the matrix
  getinverse <- function() m
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


##compute the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  ##get the value and assign it to m with the getinverse function
  m <- x$getinverse()
  ##check to see if m Is Not Null
  ##if m Is Not Null return the cached data for m
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  ##compute the inverse of the matrix
  m <- solve(data, ...)
  ##set the value of the cache with the setinverse function
  x$setinverse(m)
  m
}
