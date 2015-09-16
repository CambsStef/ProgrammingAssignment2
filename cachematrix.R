
## The following pair of functions is used to store the content of a square
## matrix and to cache the inverse of the square matrix.
##
## Below the two functions you may find an example list of prompt commands that
## could be used to verify that the functions are working correctly.

## 'makeCacheMatrix' take a matrix x as argument and return a special 'matrix'
## object, actually a list of functions, storing x and caching its inverse.
## These functions are:
## 1. 'setM' which sets the value of the matrix x.
## 2. 'getM' which gets the value of the matrix x.
## 3. 'setInverse' which calculates and sets the value of the inverse of x.
## 4. 'getInverse' which gets the value of the inverse of x.

makeCacheMatrix<- function(x = matrix()) {
        InverseM <- NULL
        setM <- function(y) {
                x <<- y
                inverseM <<- NULL
        }
        getM <- function() x
        setInverse <- function(solve) InverseM <<- solve
        getInverse <- function() InverseM
        list(setM = setM, getM = getM,
             setInverse = setInverse,
             getInverse = getInverse)
}

## 'cacheSolve' takes  a special 'matrix' object x, previously returned by
## the function 'makeCacheMatrix', and returns its inverse. In order to do this,
## 'cacheSolve' should collect the inverse from the cache if the inverse  
## has previously been calculated and the matrix has not changed.

cacheSolve <- function(x, ...) {
        InverseM <- x$getInverse()
        if(!is.null(InverseM)) {
                message("getting cached data")
                return(InverseM)
        }
        OriginalMatrix <- x$getM()
        InverseM <- solve(OriginalMatrix, ...)
        x$setInverse(InverseM)
        InverseM
}

## Example commands allowing to verify that the functions work
##
##
## > X<-matrix(1:4,nrow = 2, ncol = 2)
## > cachedX<-makeCacheMatrix(X)
## > inverseX<-cacheSolve(cachedX)
## > inverseX
##       [,1] [,2]
## [1,]   -2  1.5
## [2,]    1 -0.5
## > inverseX %*% X
##       [,1] [,2]
## [1,]    1    0
## [2,]    0    1
##
## As expected, inverseX is the inverse of X.
## Notice that in order 'solve' and 'cacheSolve' to work, X should be a square
## matrix with non-zero determinant. If 'det(X)' returns 0, 
## the the system is singular and 'solve(X)', as well as 'cacheSolve(X)', 
## return the error:
##
## > solve(X)
## Error in solve.default(X) : 
##        Lapack routine dgesv: system is exactly singular:

