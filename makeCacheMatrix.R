## A pair of functions that cache the inverse of a matrix.
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix<-function(x=matrix()){
  a<-NULL
  set<-function(y){
    x<<-y
    a<<-NULL
  }
  get<-function() x
  setinverse<-function(inverse) a<<-inverse
  getinverse<-function() a
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
  da<-x$get()
  a<-solve(da, ...)
  x$setinverse(a)
  a
}