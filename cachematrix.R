## The two functions below helps to calculate the inverse of the matrices, 
## by caching the data which can reduce computing time, 
## if the same matrix is called twice.

## Example:
## > matrix1 <- matrix(c(1, 3, 3, 4), 2, 2)
## > a <- makeCacheMatrix()
## > a$set(matrix1)
## > cacheSolve(a)
##         [,1] [,2]
## [1,] -0.8  0.6
## [2,]  0.6 -0.2
## > cacheSolve(a)
## Getting cached data!
##         [,1] [,2]
## [1,] -0.8  0.6
## [2,]  0.6 -0.2
## >


## create a list of functions to store and to return 
## the original matrix and the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        # create an empty object named 'inv' to store inverse matrix when it is created.
        inv <- NULL
        
        # create a function which creates two object to store the original matrix and its inverse
        set <- function(x) {
                mx <<- x
                inv <<- NULL
        }
        
        # create a function which simply returns the original matrix
        get <- function() {
                return(mx)
        }
        
        # create a function which stores the calculated inverse matrix data
        setinverse <- function(inverse) {
                inv <<- inverse
                return(inv)
        }
        
        # create a function which returns the stored invers matrix data
        getinverse <- function() {
                return(inv)
        }
        
        # create a list to store the created functions. 
        # That makes possible to use the fuctions with the '$' subsetting symbol.
        list(set = set, get = get,
             setinverse = setinverse, 
             getinverse = getinverse)
}


## Return a matrix that is the inverse of 'x' matrix.
cacheSolve <- function(x, ...) {
        
        # assign a value to the inv object 
        # by calling the getinverse function 
        # from the makeCacheMatrix function.
        inv <- x$getinverse()
        
        # check whether inv contains a matrix
        # and return the matrix if it contains it along with a message
        if(!is.null(inv)) {
                message('Getting cached data!')
                return(inv)
        } 
        
        # assign the original matrix to the 'data' object 
        # by calling the get function from the 
        # makeCacheMatrix function
        data <- x$get()

        # calculate the inverse of the matrix
        inv <- solve(data)
        # store the inverse of the matrix by calling the setinverse function
        # and returns the inverse of the matrix
        x$setinverse(inv)
        inv
}