## makeCacheMatrix and cacheSolve are a pair of functions that can be used to quickly retrieve the inverse of a matrix.

## makeCacheMatrix provides an interface to retrieve and update a matrix value x and its cached inverse.
## Matrix can be updated via set and retrieved via get.
## If matrix inverse is computed via cacheSolve, it can be retrieved via getinv.
## Setinv is for internal use only.

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinv <- function(inv) i <<- inv
  getinv <- function() i
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## cacheSolve calculates the inverse of the input matrix.
## If inverse is already computed, it retrieves value from the cache.
## Otherwise, the inverse is computed using "solve" and stored in the cache.
## NOTE: Extra arguments to "solve" are being ignored.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinv()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data) #ignore extra arguments (specifically b)-- see ?solve
  x$setinv(i)
  i
  
}


## Test is a short test of functionality for makeCacheMatrix and cacheSolve. It prints output to the terminal.
test <- function(){
  
  x <- makeCacheMatrix(matrix(c(-1, 2, 3, 4, 5, 6, 7, 8, 9), nrow=3, ncol=3))
  print("Initial matrix")
  print(x$get())
  print("Inverse of initial matrix")
  print(solve(x$get()))
  print("Inverse of matrix using cacheSolve")
  print(cacheSolve(x))
  print("Cached inverse using cacheSolve")
  print(cacheSolve(x))
  x$set(matrix(c(3, 5, 7, 9), nrow = 2, ncol = 2))
  print("Updated matrix, from getinv")
  print(x$getinv())
}
