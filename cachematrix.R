# The first funtion, makeCacheMatrix creates a list containing functions to:
# 1. Set the value of the matrix
# 2. Get the value of the matrix
# 3. Set the value of the inverse matrix
# 4. Get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {

     m <- NULL
     set <- function(y) {
          x <<- y
          m <<- NULL
     }
     get <- function() x
     setInv <- function(solve) m <<- solve
     getInv <- function() m
     list(set = set, get = get,
          setInv = setInv,
          getInv = getInv)         
}


#CacheSolve first tries to retrieve the inverse matrix passed to it by invoking the getInv function.
#It then evaluates whether there is a matrix being retrieved or is it a null value.
# If there is a matrix passed to it, it prints the message "getting cached data"
# And returns the variable(inverse matrix) and the function ends.
# If the variable is a null,the function gets the original matrix by using the get() function.
# It then computes the inverse of the matrix by using the solve function.
# It then calls the setInv() function to set the the inverse value and returns the inverse matrix value.

cacheSolve <- function(x, ...) {
    
     m <- x$getInv()
     if(!is.null(m)) {
          message("getting cached data")
          return(m)
     }
     data <- x$get()
     m <- solve(data, ...)
     x$setInv(m)
     m
}
