## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  ## Return a list containing a function to
  ## 1. set the value of the matrix
  ## 2. get the value of the matrix
  ## 3. set the value of inverse of the matrix
  ## 4. get the value of inverse of the matrix

  inversed_matrix <- NULL

  # set the value of the matrix
  set <- function(y) {
    x <<- y
    inversed_matrix <<- NULL
  }

  # get the value of the matrix
  get <- function() x

  # set the value of inverse of the matrix
  set_inverse <- function(inverse) inversed_matrix <<- inverse

  # get the value of inverse of the matrix
  get_inverse <- function() inversed_matrix

  # return a list
  list(set = set, get = get,
       setinv = set_inverse,
       getinv = get_inverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  # get the inverse matrix
  inversed_matrix <- x$getinv()
  
  # if the inverse matrix is not NULL, get it from the cache
  if (!is.null(inversed_matrix)) {
    message("getting cached data")
    return(inversed_matrix)
  }
  
  # otherwise, get the matrix, calculate the inverse matrix, and put it in the cache
  data <- x$get()
  
  # calculate the inverse matrix
  inversed_matrix <- solve(data, ...)
  
  # put the inverse matrix in the cache
  x$setinv(inversed_matrix)
  
  # return the inverse matrix
  inversed_matrix
}
