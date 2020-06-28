
# The following functions take advantage of Lexical Scoping to create an R 
# Object that stores a matrix and its inverse. The lexical scoping permits
# nested functions to access and use objects that are defined in the parent
# frame. 


# This function returns a list that contains 4 functions, two "getters" and two 
# "setters". To do so, it includes two data objects: one of them is a formal 
# variable (the x argument that is a matrix) the other is a local variable 
# (the inv object that is set NULL). The 2 "setter" functions use the <<- 
# assignment operator to assign a value to an object that is defined in
# the parent frame. The 2 "getter" functions retrieve objects of the parent 
# frame. So the 4 nested functions take advantage of the lexical scoping of R.


makeCacheMatrix <- function(x = matrix()) {   # x is the formal variable
        inv <- NULL                           # inv is the local variable
        set <- function(y) {                  # set () can be used to set a new matrix 
                 x <<- y                      # (different from the x defined before)
                 inv <<- NULL               
        }
        get <- function() x                   # get() retrieves the matrix
        setinv <- function(inv1) inv <<- inv1 # setinv() set the value of the inverse
        getinv <- function() inv              # getinv() retrieves the value of the inverse
        list( set = set, get = get,        
              setinv = setinv, getinv = getinv) # assigns each function to a 
                                                # named element within a list
}


# With an object of the type makeCacheMatrix() we can use the next function to
# compute the inverse of the matrix defined and cache it.This is done through the
# use of the "getters" and "setters" that were defined in the makeCacheMatrix
# function. In the case that the inverse has already been calculated, the 
# function skips the calculation and returns the inverse form the case. In another
# case, the function calculates the inverse and set it in the cache with the
# setinv() function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
}

