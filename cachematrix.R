## A pair of function that cache the inverse of matrix

## Create a matrix 'x' that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y){
    x <<-y
    inv <<- NULL
  }
  get <- function(){x}
  setinverse <- function(inverse){
    inv<<-inverse
  }
  getinverse<-function(){inv}
  list(set=set,get=get,setinverse=setinverse,getinverse=getinverse)
}


## Return a matrix that is the inverse of 'x'

cacheSolve <- function(x, ...) {
  inv <-x$getinverse()
  if(!is.null(inv)){
    return(inv)
  }
  mat<-x$get()
  inv<-solve(mat,...)
  x$setinverse(inv)
  inv
}
