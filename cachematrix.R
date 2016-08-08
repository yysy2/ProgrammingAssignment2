## This is the submission of the assigment for week 3, course 2

## This function creates the vector which contains the functions set, get, setinv, getinv in a list

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


## This function checks the cache and computes the inverse if not found

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

#k <- matrix((c(3,2,0,0,0,1,2,-2,1)), nrow =3, ncol=3)
#myMatrix <- makeCacheMatrix(k)
#myMatrix[[2]]()
#cacheSolve(myMatrix)
