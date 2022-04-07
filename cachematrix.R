## These function will get and set a square matrix and its inverse in memory


## Using lexical scoping this function will create a list of getters and setters
## for storing and retrieving a square matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) 
{
   m <- NULL
   set <- function(y) {
      x <<- y
      m <<- NULL
   }
   get <- function() x
   setsolve <- function(solve) m <<- solve
   getsolve <- function() m
   list(set = set, get = get,
        setsolve = setsolve,
        getsolve = getsolve)
}


## Using lexical scoping this function will either calculate the inverse of a
## square matrix or retrieve the cached matrix if it exists

cacheSolve <- function(x, ...) 
{
   ## Return a matrix that is the inverse of 'x'
   
   
   m <- x$getsolve()
   if(!is.null(m)) {
      message("getting cached data")
      return(m)
   }
   data <- x$get()
   m <- solve(data, ...)
   x$setsolve(m)
   m
   
}
