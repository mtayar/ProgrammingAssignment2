
## This function creates a special "matrix" object that can cache its inverse.
## get(): get cached matrix, set() set cached matrix, getinv(): get cached inverse
##matrix setinv(): set cached inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinv <- function(invm) inv <<- invm
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## This function computes the inverse of the special "matrix" returned by 
##makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed),
##then the function retrieve the inverse from the cache otherwise it will
##calculate and return the inverse.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  ## check if the cached inverse is not null and the input matrix is
  ## equal to the cached matrix
  mat<-x$get()
  
  ## check if the input matrix is equal to the cached matrix
  equalMat<-all.equal(x$get(),...)#
  if(equalMat == TRUE ) {
    invm = x$getinv()
    if (!is.null(invm)){
      message("getting cached inverse matrix")
      return(invm)
    }
  }
  #if new matrix solve for inverse matrix and cache it
  invm = solve(...)
  x$set(...)
  x$setinv(invm)
  invm
}
