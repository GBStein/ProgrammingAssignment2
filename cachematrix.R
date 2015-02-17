##Programming assignment #2
##Geoff Steinhart
##2/17/15

##The combination of makeCacheMatrix and cacheSolve can compute the inverse
##of a matrix and store it in R's cache to save computing time for large
##matrices.


##makeCacheMatrix creates a list of functions calls to cache 
##the inverse of a matrix in conjunction with cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL     ##resets inv to NULL for future use
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }     ##sets matrix x to new matrix y and resets inv
        
        get <- function() x     ##returns matrix x
        setinv <- function(solve) inv <<- solve   ##sets inv of matrix
        getinv <- function() inv     ##returns inv of matrix
        list(set=set, get=get, setinv=setinv, getinv=getinv)
        ##returns list containing all the previous functions
}


##Checks to see if the inverse of the matrix from makeCacheMatrix
##is is the cache. If not, it calculates the inverse and returns it. Otherwise,
##it retrieves it from the cache.
cacheSolve <- function(x, ...) {
        inv <- x$getinv() ##assign inverse to inv
        if(!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        }     ##if inv value is not null, return message & inv
        
        data <- x$get()  ##if the inv is not stored, calculate & return inv
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
