## Programming Assignment 2, R Programming Week 3
## Thank you for reviewing my assignment and best of luck in the course.

## Scripts to calculate or retrieve from cache the inverse of a matrix.
## Demonstrate use of lexical scoping to store objects in memory
## in order to increase efficiency of computation.
## Heavily based on makeVector, cachemean!

## CacheSolve would be the function called by the user.
## However, makeCacheMatrix must be run first, with a matrix as input,
## in order to define a list of functions that are used by CacheSolve, 
## and which might be called directly by the user.

## makeCacheMatrix takes a matrix as input and initializes two variables: 
## 1) matx, a matrix
## 2) inv, its inverse
## and also defines and returns a list of four functions:
## 1) set() # set new matrix if user so wishes
## 2) get() # retrieve matrix if previously defined 
## 3) setinverse() # set the inverse matrix (function used by makeCacheMatrix)
## 4) getinverse() # retrieve cached inverse if it exists

makeCacheMatrix <- function(matx = matrix()) {
        inv <- NULL
        set <- function(matx.set) {
                matx <<- matx.set
                inv <<- NULL
        }
        get <- function() matx
        setinverse <- function(inv.set) inv <<- inv.set
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## A script to solve for and return inverse of an input matrix -- but, first, check
## to see whether inverse has already been calculated and cached in memory.
## If it has, the cached inverse is retrieved and returned. This would 
## increase efficiency/reduce time of computation, for large matrices.

cacheSolve <- function(input, ...) {
        
        ## Check to see whether inverse in cache
        inv <- input$getinverse()
        if(!is.null(inv)) { # If it is, retrieve and return inverse
                message("getting cached data")
                return(inv)
        }
        data <- input$get() # If not, then proceed to calculate inverse
        inv <- solve(data, ...)
        input$setinverse(inv)
        inv
}
