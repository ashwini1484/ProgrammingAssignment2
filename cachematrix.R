## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(setmatrix = matrix()) 
                   {
                      Inverse <- NULL
                      setMatrix  <- function(x)
                      {
                          setmatrix <<- x
                          Inverse <<- NULL
                      }
                      getMatrix <- function() setmatrix
                      setInverse <- function(x) Inverse <<- x
                      getInverse <- function() Inverse
                      list(setMatrix = setMatrix,getMatrix=getMatrix,
                           setInverse=setInverse,getInverse=getInverse)
                   }


## Write a short comment describing this function

cacheSolve <- function(x, ...) 
              {
        ## Return a matrix that is the inverse of 'x'
                 Inverse <- x$getInverse()
                 if(!is.null(Inverse)) 
                 {
                    message("getting cached data")
                    return(Inverse)
                 }
                 data <- x$getMatrix()
                 Inverse <- solve(data)
                 x$setInverse(Inverse)
                 Inverse
              }

