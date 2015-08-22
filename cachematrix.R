## Stores the matrix "x" and returns its inverse 

## This function stores its 4 functions contained within it
## and the input matrix x, the resulting value "m" stores the value
## of the inversed matrix


makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    
    set <- function(y) {
        x <<- y #changes x into y 
        m <<- NULL #Restores TO null the value m
    }
    
    get <- function() x #returns x
    
    setinv <- function(inv) m <<- inv #stores a value m of the inverse
    
    getinv <- function() m #returns m
    
    list(set = set, get = get, setinv = setinv, getinv = getinv)
    # This list stores all 4 functions. 
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    m <- x$getinv() #checks if the var m exists. 
    
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    
    data <- x$get() #gets stored matrix x
    m <- solve(data, ...)
    
    x$setinv(m)#returns inverse matrix
    m
}







