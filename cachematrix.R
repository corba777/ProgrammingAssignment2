## Put comments here that give an overall description of what your
## functions do

## The function creates a list containing a function to
## 1. set matrix
## 2. get matrix
## 3. set inverse matrix
## 4. get inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invmatr) inv <<- invmatr
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## The function computes the inverse of the special "matrix" returned by makeCacheMatrix. 
## If the inverse matrix has already been calculated and the original matrix has not changed, 
## then this function retrieves the inverse matrix from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinv(inv)
  inv
}
