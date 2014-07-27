## The Assignment 3 will give the advantage of the scoping rules
## of the R language and how they can be manipulated
## to preserve state inside of an R object.

## Matrix inversion is usually a costly computation and there may
## be some benefit to caching the inverse of a matrix rather
## than computed it repeatedly.

## The project assignment is to write a pari of function that
## cache the inverse of a matrix.

## The makeCacheMatrix function creates a special "matrix" object
## that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        ## initialize the inverse matrix
        invm <- NULL
        
        ## set the matrix
        set <- function(matrix1) {  
                x <<- matrix1
                invm <<- NULL
        }
        
        ## get the matrix
        get <- function() {
                x
        }
        
        ## set the inverse of a matrix
        setinverse <- function(inverse) {
                invm <<- inverse
        }
        
        ## get the inverse of a matrix
        getinverse <- function() {
                invm
        }
        
        ## return a list of the matrix with defined functions
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The cacheSolve function computes the inverse of the special
## "matrix" returned by makeCacheMatrix. In case the inverse has
## already been calculated, then it should retrieve the inverse
## from the cache

cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        invm <- x$getinverse()
        if(!is.null(invm)) {
                message('getting cached data')
                return(invm)
        }
        
        ## calculate the inverse matrix
        data <- x$get()
        invm <- solve(data, ...)
        
        ## cache the inverse matrix
        x$setinverse(invm)
        
        ## return the inverse matrix
        invm
}