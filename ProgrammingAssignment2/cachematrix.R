## Creates a matrix and then either solves inverse or gets cached result
## if already solved

## This function initializes variables and creates a matrix for solving.
makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x<<-y
                m<<-NULL
        }
        get<-function() x
        setinverse<-function(inverse) m<<-inverse
        getinverse<-function() m
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## This function either solves the inverse of the matrix or returns
## result if already cached.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m<-x$getinverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data<-x$get()
        m<-solve(data,...)
        x$setinverse(m)
        return(m)
}
