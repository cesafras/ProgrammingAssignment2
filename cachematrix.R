## The objective of these functions is to create a matrix and store
## its inverse in memory in order that little to no processing 
## power is required to retrieve the solution (i.e. the inverse)

## This function creates a "special" matrix, the inverse solution
## to which can be cached in memory

makeCacheMatrix <- function(x,a) {
## Pass two arguments to create a square matrix where x represents 
## the values in the matrix, and a represents both the number of 
## rows and columns in the matrix
  s <- NULL ## Set the solution to be NULL by default
  set <- function(y,b) ## Allow the user to reset the matrix with 
                       ## different arguments
  {
    x <<- y
    a <<- b
    s <<- NULL
  }
  get <- function() ## Function to get the contents of the 
                    ## original function...
  {
    matrix(x,nrow=a,ncol=a) ## ...which is a matrix with values of 
                            ## x and a rows and a columns
  }
  setsolve <- function(solve)  { ## Function that stores the 
                                 ## solution 
    s <<- solve
}
getsolve <- function() ## Function that retrieves the solution
{
  s
}
list(set=set,get=get,
     setsolve=setsolve,getsolve=getsolve) ## List of the different 
                                          ## functions available 
                                          ## in the special matrix
}


## This function refers to the "special" matrix defined earlier
## and either 1 - solves the matrix and caches the solution or
## 2 - returns the previously cached solution

cacheSolve <- function(x,...) {
  s <- x$getsolve() ## First we need to retrieve the solution 
                    ## by calling the getsolve function from
                    ## within the "special" matrix
  if(!is.null(s)) { ## If there is already a cached solution
                    ## in memory...
    message("getting cached data") ## ...we want to return a
                                   ## a message letting us 
                                   ## know that the solution
                                   ## was cached...
    return(s)                      ## ...as well as the 
                                   ## solution
  }
  data <- x$get() ## If there is no cached solution we want
                  ## to retrieve the matrix
  s <- solve(data,...) ## Solve it
  x$setsolve(s) ## Store the solution in memory
  return(s) ## And return the solution
}
