## These two functions form an object that takes a matrix, inverses it and caches the inverse. 
## If the  matrix is input again ,its cached inverse can be used, saving the 
## costs of recalculation, useful when handling large datasets.

## 1.The first function,makeCacheMatrix creates a 'special matrix' which is really a list. 
## It sets the value of the matrix,gets the value of the matrix, and caches the inverse. 


makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(
    set = set,
    get = get,
    setinverse = setinverse,
    getinverse = getinverse)
}
## 2.The final function, cacheSolve  first checks if the inverse has  already been calculated . 
## If yes- the function gets the cached  inverse . 
## Otherwise,  the inverse  of the new matrix is calculated.

cacheSolve <- function(x, ...) {
  
   inv <- x$getinverse()
   if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
  inv
}

