## This set of functions returns the inverse of a matrix from a list in cache or calculating if it is not available in the list 


## This function adds matrices and its inverses in a list. 
## The inverted matrix has been calculated by cacheSolve function.

makeCacheMatrix <- function(mat=matrix()) {  
  m <- NULL  
  set <- function(newmat){  
    mat <<- newmat
    m <<- NULL
  }
  get <- function() mat
  setsolve <- function(solve) m <<- solve
  getsolve <- function() m
  list(set=set , get=get,
       setsolve=setsolve, 
       getsolve=getsolve)
}


## This function checks if the inverse of the matrix has been computed and stored in cache.
## If it is in cache, the inverse matrix is returned. 
## If it isn't in cache, the inverted matrix is calculated and added to the list in cache.

cacheSolve <- function(makeCacheMatrix,...){
  m <- makeCacheMatrix$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data <- makeCacheMatrix$get()
  newsolve <- solve(data)
  makeCacheMatrix$setsolve(newsolve)
  newsolve
}
