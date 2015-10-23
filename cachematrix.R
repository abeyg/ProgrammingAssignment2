## Return a matrix that is the inverse of the input matrix.
## These two functions cache and return the inverse of the input matrix x
## provided the input matrix can be inversed.

## Here is an example of how to use it from the R console.
## > a<-matrix(c(1,3,2,4,3,2,5,4,3),nrow=3,ncol=3)
## > b<-makeCacheMatrix(a)
## > cacheSolve(b)
##            [,1]       [,2]       [,3]
## [1,] -0.3333333  0.6666667 -0.3333333
## [2,]  0.3333333  2.3333333 -3.6666667
## [3,]  0.0000000 -2.0000000  3.0000000
## > cacheSolve(b)
## getting cached inverse
##            [,1]       [,2]       [,3]
## [1,] -0.3333333  0.6666667 -0.3333333
## [2,]  0.3333333  2.3333333 -3.6666667
## [3,]  0.0000000 -2.0000000  3.0000000

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## the four functions to set, get the matrix and its inverse are returned as a matrix as 
## per the specification of the problem. The returned matrix is a 4x1 matrix with the following
## functions as its elements.
## set - set the value of the matrix
## get - get the value of the matrix
## setinvese - set the value of the inverse
## getinvese - get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse <- function() inv
        as.matrix(c(set = set, get = get, setinverse = setinverse, getinverse = getinverse))
}

## cacheSolve: This function computes the inverse of the special "matrix"
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## retrieve the inverse from the cache.
## Otherwise, it will be computed using the solve function and updated
## in the cache before returning the inverse.
cacheSolve <- function(x, ...) {
        ## convert it first to a list to access it easily.
        l=as.list(x[,])
        m <- l$getinverse()
        if(!is.null(m)) {
                message("getting cached inverse")
                return(m)
        }
        data <- l$get()
        m <- solve(data)
        l$setinverse(m)
        m
}
