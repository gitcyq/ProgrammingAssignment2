## makeCacheMatrix creates a function which contains 4 functions
##1.set the value of the matrix
##2.get the value of the matrix
##3.set the value of the inverse
##4.get the value of the inverse

## The list is used for cachesolve

makeCacheMatrix <- function(x = matrix()) {
        cs<-NULL
        set <- function(y) {
                x <<- y
                cs <<- NULL
        }
        get<-function()x
        setsolve<-function(solve) cs<<-solve
        getsolve<-function() cs
        list(set=set, get=get, setsolve=setsolve,getsolve=getsolve)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        cs<-x$getsolve()
        if(!is.null(cs)){
                message("getting cached data")
                return(cs)
        }
        data<-x$get()
        cs<-solve(data, ...)
        x$setsolve(cs)
        cs
        ## Return a matrix that is the inverse of 'x'
}
