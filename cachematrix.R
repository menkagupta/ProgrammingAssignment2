#makeCacheMatrix: 
#This function creates a special "matrix" object that can cache its inverse.
#cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.
#It also outputs a message if the matrix is non-invertible(singular).

##makeCacheMatrix()
###This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <-function(solve) m <<-solve
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


#cacheSolve: 
#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
#If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should 
#retrieve the inverse from the cache.
#It also outputs a message if the matrix is non-invertible(singular).

cacheSolve <- function(x, ...) {

  m <- x$getinverse()
  if(!is.null(m)) {
    message("Inverse has been already calculated, getting cached data")
    return(m)
  }
  data <- x$get()
  
  #Check if the matrix is invertible
  z<-ifelse(det(data)==0, FALSE, TRUE)
  
  if (z){
   m <- solve(data, ...)
    x$setinverse(m)
    m  
  }
  else {
    message("matrix is singular")
  }
  
}