## In this script there are two functions; one that creates a list of functions
##and store the inverse and the matrix. The second one will verify the
##information of the first one either compute and return or just return the 
##inverse

## makeCacheMatrix will have the argument of a Matrix and by default it has an
##empty matrix. This function will return a list with functions set,get, 
##SetInverse and getInverse

makeCacheMatrix <- function(x = matrix()) {
  inverse<- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }  
  get <- function() {x}
  setInverse <- function(solve) {inverse <<- solve}
  getInverse <- function() inverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The following function will verifies if there exist already the inverse and
##will return the existing inverse otherwise it will compute it and return it

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverse <- x$getInverse()
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  mat <- x$get()
  inverse <- solve(mat, ...)
  x$setInverse(inverse)
  inverse
}
