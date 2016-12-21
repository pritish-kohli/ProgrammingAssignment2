## This function stores the value of the inverse
## If the solution is not done before then it returns null
## It takes matrix as argument and returns a special "vector"

makeCacheMatrix <- function(x = matrix())
{
  m <- NULL
  set <- function(y) 
  {
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

##cacheSolve is used to solve the matrix by taking the object of
##makeCacheMatrix function as an argument
##it checks for previously solved matrix
##otherwise solves it an gives output

cacheSolve <- function(x, ...)
{
  m <- x$getinverse()
  if(!is.null(m))
  {
      message("getting cached data")
      return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}