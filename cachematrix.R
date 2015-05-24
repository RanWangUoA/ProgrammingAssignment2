## The first function, makeCacheMAtrix creates a special "matrix", 
##which is really a list containing a function to

##set the value of the matrix
##get the value of the matrix
##set the value of inverse of the Matrix
##get the value of inverse of the Matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y){
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m<<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}

## The following function calculates the inverse matrix
## of the special "matrix" created with the above function. 
## It first checks to see if the inverse has already been calculated.
## If so, it gets the matrix from the cache and skips the computation. 
## Otherwise, it calculates the inverse and sets the value 
## of the inverse in the cache via the setmatrix function.

cacheSolve <- function(x=matrix(), ...) {
  m <- x$getmatrix()
  if(!is.null(m)){
    message("getting cached matrix")
    return(m)
  }
  matrix<-x$get
  m<-solve(matrix, ...)
  x$setmatrix(m)
  m
}
