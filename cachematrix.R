## Put comments here that give an overall description of what your
## functions do

## The assignment's aim is to write functions named 'makeCacheMatrix' and 'cacheSolve' to complete Coursera's R Programming Week 3 assignment
## Completed by Devanshoo Jain on 28th March 2020.

## Write a short comment describing this function

##'makeCacheMatrix' function can create a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {  ## define the argument with default mode of "matrix"
     inv <- NULL                             ## initialize inv as NULL, will hold value of matrix inverse 
     set <- function(y) {                    ## define the set function to assign new 
         x <<- y                             ## value of matrix in parent environment
         inv <<- NULL                        ## if there is a new matrix, reset inv to NULL
     }
     get <- function() x                     ## define the get fucntion - returns value of the matrix argument

     setinverse <- function(inverse) inv <<- inverse  ## assigns value of inv in parent environment
     getinverse <- function() inv                     ## gets the value of inv where called
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)  ## we need this in order to refer to the functions with the $ operator
 }


}


## Write a short comment describing this function

## 'cacheSolve' function computes the inverse of the special 'matrix' returned by 'makeCacheMatrix'.


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
     if(!is.null(inv)) {
         message("getting cached data")
         return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinverse(inv)
     inv
}



##_______________________________________________##
                ##OUTPUT

##> m <- matrix(rnorm(16),4,4)
##> m1<-makeCacheMatrix(m)
##> cacheSolve(m1)
##          [,1]       [,2]       [,3]      [,4]
##[1,] 0.09338523  0.5927375  0.1103438 -1.323638
##[2,] 0.16984998  0.1332557  0.1753123 -1.963493
##[3,] 0.02936029 -0.1497410  0.9699254  1.692374
##[4,] 0.80429278  0.6423970 -0.6478038 -3.516878
> 

