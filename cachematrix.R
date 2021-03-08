## Put comments here that give an overall description of what your
## functions do
## Write a short comment describing this function
## "makeCacheMatrix" is used to create a specical matrix to cache the 
## inverse of a matrix for its input. 


makeCacheMatrix <- function(x = matrix()) {
  a<-NULL
  set<-function(y){
        x<<-y
        a<<-NULL
  }
        get<-function()x
        setinverse<-function(inverse) a<<-inverse
        getinverse <-function() a
        list(set=set,
             get= get,
             setinverse=setinverse,
             getinverse=getinverse)
}


## Write a short comment describing this function
## This function compute the matrix inverse created 
## by the abovementioned "makeCacheMatrix".If the inverse has already been calculated 
## then it should retrieve the inverse from the cache


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    a<-x$getinverse ()
    if (!is.null(a)) {
        message("getting cached data")
        return(a)
    }
    mat<-x$get()
    a<-solve(mat,...)
    x$setinverse(a)
    a
}
