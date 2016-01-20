## cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
rm(list=ls())
makeCacheMatrix <- function(x = matrix()) {
  invM <- NULL
  set <- function(y) {
    x <<- y
    invM <<- NULL
  }
  get <- function() x
  setinvM <- function(invMat) invM <<- invMat
  getinvM <- function() invM
  list(set = set, get = get, setinvM = setinvM, getinvM = getinvM)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invM <- x$getinvM()
  if(!is.null(invM)) {
    message("Getting cache matrix")
    return(invM)
  }
  mat <- x$get()
  invM <- solve(mat,...)
  x$setinvM(invM)
  invM
}

## Testing the Code
a <- makeCacheMatrix(matrix(c(1,0,-4,0,1,0,0,0,1),3,3)) 
cacheSolve(a) ## Setting Matrix Inverse in Memory
cacheSolve(a) ## Cache the inverse 

a <- makeCacheMatrix(matrix(c(0,1,4,1,0,1,1,1,0),3,3))
cacheSolve(a) ## Setting Matrix Inverse in Memory
cacheSolve(a) ## Cache the inverse 
