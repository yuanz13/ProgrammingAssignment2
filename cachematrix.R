## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## four functions in the list: set,get,setinv and getinv. The set function sets the inverse to Null when the matrix is updated.
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y){
                if(!(is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y))){
                        x <<- y
                        m <<- NULL
                }
        }
        get <- function() x
        setinv <- function(inv) m <<- inv
        getinv <- function() m
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        
}


## Write a short comment describing this function
## if the inv has been cashed, load it. Otherwise calculate it.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinv()
        if(!is.null(m)) {
                message("getting cashed data")
                return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m
}
