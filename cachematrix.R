## The following two functions calculates the
## inverse of a matrix and caches it so that
## the computer doesn't have to calculate the
## same thing over and over.

## makeCacheMatrix takes the matrix and records the 
## inverse if calculated. It returns a list of functions

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  
  # set function
  set <- function(y)
  {
    x <<- y
    inv <- NULL
  }
  
  # get the matrix
  get <- function() x
  
  # set inverse
  setInverse <- function(inverse) inv <<- inverse
  
  # get the inverse
  getInverse <- function() inv
  
  # return a list that creates the vector
  list(set = set, get = get, setInv = setInverse, getInv = getInverse)
}


## cacheSolve calculates the inverse if not
## calculated previously and sends the
## information to makeCacheMatrix to save.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # get the inverse
  inv <- x$getInv()
  
  # check if "inv" is null or not
  if (!is.null(inv))
  {
    message("getting cached data...")
    return(inv)
  }
  else
  {
    matrix <- x$get()
    inv <- solve(matrix)
    x$setInv(inv)
    inv
  }
}
