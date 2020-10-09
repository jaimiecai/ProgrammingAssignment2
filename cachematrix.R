## These functions allow a matrix to be inverted via solve
## and allow for the inverted matrix to be cached.


## This sets the matrix to be solved and cached. It also resets 
## m to null when it's run so the cacheSolve function will know to 
## run the solve function if m is NULL. 

makeCacheMatrix <- function(x = matrix()) {
  m<<-NULL
  set<-function(y){
    x <<-y
    m<<-NULL
  }
  get <-function() x
  setsolve <- function(solve) m<<-solve
  getsolve <- function() m
  list(set=set, get=get, setsolve=setsolve, getsolve=getsolve)
}


## This function gets the inverted matrix of the inputed 
## object and then determines if there is there is a value stored 
## if it is not null (that is not stored) then it returns 
## the inverted matrix of the input
cacheSolve <- function(x, ...) {
  m <-x$getsolve()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
