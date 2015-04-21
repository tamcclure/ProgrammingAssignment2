## Creates a Cache for solving an inverse matrix
##  Uses two functions: makeCacheMatrix as the list and CacheSolve for final solution

## It creates a list to allow the use of the stored value of the variable s, 
## which is used to solve the inverse matrix

makeCacheMatrix<-function(x=matrix){
  s<-NULL
  set<-function(y){
    x<<-y
    s<<-NULL    
  }
  get<-function()x
  setinverse<-function(solve)s<<-solve
  getinverse<-function()s
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## It finds the stored value for the variable s if it has been stored;
## Otherwise cacheSolve will create the value for the variable s and store it in the cache

cacheSolve<-function(x,...){
  if(!is.null(s)){
    return(s)
  }
  data<-x$get()
  s<-solve(data,...)
  x$setinverse(s)
  s
}