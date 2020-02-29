## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  a<-NULL
  set<-function(y){                              ## Method for setting the matrix
    x<<-y                                       
    a<<-NULL
  }
  get<-function() x                              ## Method for getting the matrix
  setinverse<-function(inverse) a<<-inverse      ## Method for setting the inverse of the matrix
  getinverse<-function() a                       ## Method for getting the inverse of the matrix
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}
  
## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
cacheSolve<-function(x, ...){
  ## Return a matrix that is the inverse of 'x'
  a<-x$getinverse()
  if(!is.null(a)){
    message("getting cached data")
    return(a)
  }
  da<-x$get()                                     ## Get the matrix from our object
  a<-solve(da, ...)                               ## Calculating the inverse of the matrix
  x$setinverse(a)                                 ## Set the inverse in our object
  a                                               ## Return a matrix that is the inverse of 'x'
}
