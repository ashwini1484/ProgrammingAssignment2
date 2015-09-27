## makeCacheMatrix: 
## This funcion does 4 operations:
## 1. Set a matrix
## 2. Get a stored matrix
## 3. Store the matrix that is provided as input
## 4. Get the stored inverse
## All these functions are stored in a single function so that it can be assigned
## to a variable

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


## cacheSolve:
## This function gets the cached inverse matrix if it was already computed 
## Calculates the inverse of the input matrix if the input is different from 
## the one already stored

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

