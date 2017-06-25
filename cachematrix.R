## The makeCacheMatrix funtion takes a matrix as an argument where the values for function(y)
## are cached in object x and inv (short for inverse) using the <<- operator 

makeCacheMatrix <- function(x = matrix()) {inv<-NULL 
set<-function(y) {
        x<<- y     ## y is undefined 
        inv<<- NULL
}
get<- function() x
setinverse<- function() inv<<-inverse ##sets values for matrix
getinverse<- function() inv            ##gets values of the matrix 
list(set=set, get=get, setinverse=setinverse, getinverse=getinverse) }

## returns the inverse using values defined in the makeCacheMatrix 
## double checks if the inverse has already been calculated so it doesnt have to recalculate 

cacheSolve <- function(x, ...) {
        inv<- x$getinverse()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data<-x$get()
        inv<-inverse(data,...)
        x$setinverse(inv)
        inv
}
