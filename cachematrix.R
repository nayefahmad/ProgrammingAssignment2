## === Put comments here that give an overall description of what your
## functions do ===
## These two functions work together to allow the user to set an 
## input matrix, find its inverse, and cache the inverse so that it can 
## be recalled later without having to be calculated. 


## === Write a short comment describing this function ===
## This function creates a list that can be subsetted in particular ways
## to set a particular matrix, and to return its inverse (once the inverse)
## has been calculated by the cacheSolve function). Note that makeVector
## does not actually calculate the inverse. 


makeCacheMatrix <- function(x = matrix()) {
        i <- NULL # i stands for inverse 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inv) i <<- inv
        getinverse <- function() i
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## === Write a short comment describing this function ===
## This function interacts with the makeCacheMatrix function to check 
## whether the inverse of the inputted matrix has been calculated. If not, 
## it calculates the inverse and stores/caches the inverse in the 
## global environment. 


cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}








