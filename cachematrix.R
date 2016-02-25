## These functions can be used in combination to cache the inverse of
## an invertible matrix. Call makeCacheMatrix and pass it an invertible
## matrix to return a special matrix object that can store the cache of 
## the inverse of the matrix passed to it. Calling x$get  at the console 
## will return the original matrix. Once you have created the special matrix
## by calling makeCacheMatrix, call cacheSolve(x) to get the inverse of 
## the matrix. The first time you call cacheSolve(x) will calculate the 
## inverse of the matrix, every subsequent will return the inverse from cache.

## ****************************sample usage*********************************
## create an invertible matrix called f at the console by first running:
## f <- stats::rnorm(16)
## this will yield a vector of 16 values. To convert that vector to a 4X4
## invertible matrix, run the following at the console:
## dim(f) <- c(4,4)
## f now contains an invertible 4X4 matrix 

## To create a special matrix x, run the following at the console:
## x <- makeCacheMatrix(f)
## x now contains a special matrix which contains the uninverted matrix f 
## along with a function for managing the cache.
## you can check the content of x by running the following at the console:
## x$get()
## this will yield the 4X4 matrix f
## you can check that this is the same as f
## by running the following at the console:
## identical(f, x$get())
## this should yield TRUE.

## you can check that the cache isn't populated by running the 
## following code at the console:
## x$getInverse()
##this should yield NULL

## To populate the cache with the inverse of f, run the following code
## at the console:
## cacheSolve(x)
## this will yield a 4X4 matrix which is the inverse of f (and x$get())
## x now has the original matrix cached as x$get() and also has the 
## inverse of the original matrix cached as x$getInverse()
## you can check that the cache is populted by running the
## following code at the console:
## x$getInverse()
## this should yield a 4X4 matrix which is the inverse of f (and x$get())
## If you run cacheSolve(x) again, you will get the message 'getting cached matrix'
## before the 4X4 matrix is returned. 

## You can check that the 4X4 matrix in the cache is identical to the 
## inverse of f by running the following at the console:
## identical(solve(f), x$getInverse())
## This should yield TRUE.

## You can further check that you are getting data from the cache by running:
## identical(solve(f), cacheSolve(x))
## which ## will yield 'getting cached matrix' and TRUE

## The first time you run cacheSolve for x it will populate the cache, 
## every subsequent run will return the cached data, so instead of using 
## x$getInverse() you should use cacheSolve(x) so that if the cache is empty
## then the cache will be calculated and populated. Never use x$getInverse(),
## instead you should always use cacheSolve(x)
## **************************end sample usage*******************************

## This function takes a matrix and returns a list containing a function that
## 1. sets the value of the matrix
## 2. gets the value of the matrix
## 3. sets the inverse of the matrix
## 4. gets the inverse of the matrix
## Call this function and pass it an invertible matrix, then call
## cacheSolve to populate the cache with the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setInverse <- function(solve) m <<- solve
        getInverse <- function() m
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This funtion takes a paramter of special matrix returned from 
## makeCacheMatrix and checks to see if the inverse of the matrix 
## has been populated. If it has, it returns the cached matrix, if
## not, it calculates the inverse of the matrix, caches it and returns
## the inverse matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'        
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached matrix")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
