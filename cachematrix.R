
## The function makeCacheMatrix creates a list containing a function that:
## 1 - sets value of a matrix
## 2 - gets value of a matrix
## 3 - sets value of the matrix's inverse
## 4 - gets value of the matrix's inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y){
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
        
}


## The cacheSolve function returns the value of a matrix's inverse.
## If the matrix's inverse has already been solved and cached, that value is extracted and returned.
## If the matrix's inverse has not been solved, the function finds and stores the matrix's inverse.

cacheSolve <- function(x, ...) {
    x$getinv()
    if(!is.null(inv)){
        message("getting Cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data)
    x$setinv(inv)
    inv
}
