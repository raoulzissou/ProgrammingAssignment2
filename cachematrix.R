## The first function, makeVector creates a special "vector" 
## which is really a list
## a list containing a function to:
##set the value of the matrix
##get the value of the matrix
##set the value of the matrix inverse
##get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() {x}
       setinv <- function(inv) {m <<- inv}
       getinv <- function() {m}
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}


## The following function calculates the inverse of the 
# matrix created with the above function. 
# However, it first checks to see if the inverse has already been calculated. 
# If so, it gets the inverse from the cache and skips the computation. 
# Otherwise, it calculates the inverse of the matrix and sets the 
# matrix in the cache via the setinv function.

cacheSolve <- function(x, ...) {
       m <- x$getinv()        ## brings in the cached inverse matrix m
       if(!is.null(m)) {
              message("getting cached data")
              return(m)       ## if cached matrix is present then non 
              ## need to calculate, just return value
       }
       data <- x$get()        ## otherwise..
       m <- solve(data, ...)       ## calculate the inverse of x
       x$setinv(m)                 ## cache the inverse in m
       m ## Return a matrix that is the inverse of 'x'
}