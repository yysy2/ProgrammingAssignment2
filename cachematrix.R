## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  #print(x)
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

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}

k <- matrix((c(3,2,0,0,0,1,2,-2,1)), nrow =3, ncol=3)
#print(k)
myMatrix <- makeCacheMatrix(k)
myMatrix[[2]]()
cacheSolve(myMatrix)

#myVector <- makeVector(1:15)
#myVector[[2]]()
#cachemean(myVector)