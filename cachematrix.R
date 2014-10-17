
## Function 'makeCacheMatrix'
## This function accepts a matrix 'x' as an argument and returns a list of 
## functions that allow to i. set the value of the matrix ii. get the value
## of the matrix iii. set the value of the inverse of the matrix iv. get the
## value of the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}


## Function 'cacheSolve'
## This function accepts as an argument a list returned by the function
## 'makeCacheMatrix' and returns the inverse of the matrix 'x' passed as an
## argument to the function 'makeCacheMatrix'. It first checks to see if the
## inverse has already been calculated. If so, it `get`s the inverse from the
## cache and skips the computation. Otherwise, it calculates the inverse
## and sets the value of the inverse in the cache via the `setinv` function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
