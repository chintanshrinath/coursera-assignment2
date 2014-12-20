makeCacheMatrix<-function(x=matrix()){
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setInverse<-function(inverse) inv<<-inverse
  getInverse<-function() inv
  list(set=set,get=get,setInverse=setInverse,getInverse=getInverse)
  
}
catchSolve<-function(x,...){
  inv<-x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  data<-x$get()
  inv<-solve(data)
  x$setInverse(inv)
  inv
}
