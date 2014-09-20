
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  # this is a function to set the matrix. At this point the inverse of the matrix does not exist and we set the vaiable i to NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  # function to etrieve the matrix
  get <- function() x
  # function to set the inverse of the matrix
  setInverse <- function(inverse) i <<- inverse
  # function to get the inverse of the matrix
  getInverse <- function() i
  # return a list that contains functions for getting and setting
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

  # get the inverse from the cache
  inverse <- x$getInverse()
  # the set() function in makeCacheMatrix() sets the inverse to NULL. This means that if the matrix changes, the previous inverse gets remove and set to NULL
  # therefore the condition "and the matrix has not changed" of the assignment task is alos fulfilled
  # check if our attempt to pull the inverse from the cache was successfull or if it does not exists
  if(!is.null(inverse)) {
    message("getting cached data")
    # if the inverse exists then return this data as returnvalue of the function
    return(inverse)
  }
  # if we arrived here then this means no invese is existing and we need to create it
  # to do this we first need to get data
  data <- x$get()
  # then we invese the matrix using the solve function
  inverse <- solve(data, ...)
  # then set the invese in the cache for future reference
  x$setInverse(inverse)
  # return the inverse as return value of this function
  inverse  
}

