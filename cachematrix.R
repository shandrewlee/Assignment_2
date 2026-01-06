## This file will fetch a matrix and cache it's inverse if it's previously computed.
## If not the inverse will be computed and cache in the makeCacheMatrix function.
## The purpose of the caching is to prevent repetitive computation unless the matrix is changed

## makeCacheMatris is a function that reads a matrix (assume invertible)
## set is to store the matrix
## setinv is to cache the matrix's inverse. Default as NULL
## get and getinv are functions for reading the matrix and its inverse respectively
makeCacheMatrix <-function(curr_mat) {
    curr_inv <- NULL
    set <- function(x) {
        curr_mat <<- x
        curr_inv <<- NULL
    }
    get <- function() curr_mat
    setinv <- function(x) curr_inv <<- x
    getinv <- function() curr_inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve is a function to read the list makeCacheMatrix
## if getinv is NULL, it will calculate the inverse of the matrix and store in makeCacheMatrix
## else it will read directly from getinv. No further calculation involved
cacheSolve <- function(x, ...) {
    if (!is.null(x$getinv())){
        print("Fetching cached data")
        return (x$getinv())
    }
    x$setinv(solve(x$get()))
    x$getinv()
}