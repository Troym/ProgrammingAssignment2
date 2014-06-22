##
## This is a pair of functions that work together to create and cache an
## inverted matrix.
##  - makeCacheMatrix
##  - cacheSolve
##
## A matrix is added to che cache with makeCacheMatrix.  The inverse is
## either computed and stored in the cache or retrieved from the cache
## with the cacheSolve function.
##
## Example:
##
##  a <- matrix(1:4, 2, 2)
##  b <- makeCacheMatrix(a)
##      --- Now compute the inverse for the first time and store in c
##  c <- cacheSolve(b)
##      --- Do other stuff
##      --- Retrieve the cached inverse matrix
##  c <- cacheSolve(b)


## makeCacheMatrix - Create an object to compute and cache a matrix inverse
##
## Parameters
##  x - The matrix to take the inverse of
##
## Returns an object (list of functions) that can be used by the cacheSolve
## to conpute and cache the inverse of the matrix.
##

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(i) inv <<- i
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve - Compute and cache the inverse of a matrix held in an object 
##      created by makeCacheMatrix
##
## Parameters
##  x - The object created by makeCacheMatrix that contains the matrix
##
## Returns the inverse of the matrix as calculated by the solve) function.
##      This result may have been cached on a previous call.
##

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    message("computing inverse")
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
