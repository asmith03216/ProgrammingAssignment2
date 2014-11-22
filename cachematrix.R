## The purpose of these functions is to demonstrate how superassignment operators
## work.  The first function takes a matrix, and uses a superassignment operator
## to enable it to be used in another environment other than that function.

## makeCacheMatrix returns a list with the following functions:
     ## It creates a variable i for the inverse and sets it to NULL
     ## Then it creates a set functions using superassignment to set both
          ## x (the passed matrix) and i (the inverse variable)
     ## Set - give the ability to change the matrix orignally passed
     ## Get - to get the matrix stored by the superassigment operator
     ## setInv - a function to set the inverse matrix
     ## getInv - a functions to get the inverse matrix stored 
               ## by the superassigment operator

##cacheSolve is passed the list of functions from makeCacheMatrix. 
     ## It gets  i - the inverse variable from makeCacheMatrix
     ## It then looks at i, determines if it is null.
     ## If i is not null then it returns a message, and the value of i
     ## if i is null, 
          ## It get the x(matrix) value from makeCacheMatrix,
          ## Then it calculates the inverse using solve() and returns
          ## the value of i (the inverse of the matrix)

## NOTE - THE MATRIX MUST BE SQUARE TO BE  INVERTIBE, if not invertible an
## error will be thrown

makeCacheMatrix <- function(x = matrix()) {
     
     i <- NULL
     
     ## create a set function to change the original matrix and
          ## the inverse matrix to variables that can be retrieved outside
          ## of this function with the superassignment operator
     
     set <- function(y) {
          x <<- y
          i <<- NULL
     }
     
     ## create set and get functions
     
     get <- function() x
     setinv <- function(inv) i <<- inv
     getinv <- function() i
     
     ## create and return a variable called list contains all of the set
     ## and get functions
     
     list(set = set, get = get, setinv = setinv,
          getinv = getinv)
}


cacheSolve <- function(x, ...) {
     
     ## retrieves the i variable from makeCacheMatrix using the function list 
          ## passed into this function
     
     i <- x$getinv()
     
     ## determines if null - if not sends a message and returns i
          ## if null, it gets the matrix and using the solve function gets
          ## and returns the inverse
     
     if(!is.null(i)) {
          message("getting cached data")
          return(i)
     }
     data <- x$get()
     i <- solve(data, ...)
     x$setinv(i)
     i
}

## TESTING
##  x <- matrix(1:4,2,2)
##  a <- makeCacheMatrix(x)
## cacheSolve(a)

## another example of a 'square' matrix to use for test
## x <- matrix(c(1,2,4,6,5,1,4,4,3),3,3)
