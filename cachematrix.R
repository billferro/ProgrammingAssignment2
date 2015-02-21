## CacheMatrix.R
##  Coursera R Programming Assignment 2
##  Bill Ferro http://github.com/billferro

## Test Cases
##  Create square 2x2 matrix, took example from KhanAcademy Inverse example
##  https://www.khanacademy.org/math/precalculus/precalc-matrices/inverting_matrices/v/inverse-of-a-2x2-matrix
##  Inverse is:
##      [ 2/41=0.04878049,      -5/41=-0.12195122
##        7/41=0.17073171,       3/41= 0.70317073  ]
##  Prep work
##      d <- matrix(c(3,-7,5,2), nrow=2, ncol=2)
##      my_matrix <- makeCacheMatrix(d)
##      class(my_matrix)   ==> List
##      my_matrix$get()     ==> square matrix with values same as d
##      my_matrix$getinverse() ==> NULL meaning no cache value is set
##  Test Case 1 - Positive
##      d_inverse <- cacheSolve(my_matrix)
##              first time call should return inversed matrix without message
##      d_inverse ==> compare to values listed above
##      d_inverse2 <- cacheSolve(my_matrix)
##              Second (and subsequent) call should return inversed matrix with message
##      identical(d_inverse, d_inverse2)  ==> TRUE, resulting values should be the same
##  Test case 2 - Changed
##      d_inverse <- cacheSolve(my_matrix)
##      my_matrix$set(matrix(c(3,-7,5,1),nrow=2,ncol=2))
##      d_inverse2 <- cacheSolve(my_matrix)
##          no message should appear because $set() clears the cache
##      d_inverse3 <- cacheSolve(my_matrix)
##          message should appear
##      identical(d_inverse, d_inverse2) ==> FALSE, not the same matrix
##      identical(d_inverse2, d_inverse3)==> TRUE, same matrix

## makeCacheMatrix - Create a CacheMatrix object and associated caching functions
##
##  INPUT:  x - Creates x as a Cache Matrix Object
##  METHODS:
##      set(y)  - Sets matrix data to Y and clears the cache
##      get()   - Returns the matrix data
##      setinverse(y)   - Caches passed matrix as the inverse of y
##      getinverse()    - Returns the cached inverse matrix
##  RETURN: list of methods

makeCacheMatrix <- function(x = matrix()) {

        ## Cached value
        m <- NULL
        
        set <- function (y) {
            x <<- y
            m <<- NULL
        }
        
        get <- function() x
        
        setinverse <- function(y) m <<- y
        
        getinverse <- function() m
        
        ## Return methods
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve - return a matrix that is the inverse of x
##              pulling the inverse from the cache,
##              if value is already in the cache
## INPUT: x - CacheMatrix containing data to inverse
## RETURN: reversed matrix

cacheSolve <- function(x, ...) {

        ## lookup the data in the cache and return it, if present
        m <- x$getinverse()
        if (!is.null(m)) {
            message("getting cached data")
            return(m)
        }
        ## otherwise, inverse the matrix and cache the value
        data <- x$get()
        m <- solve(data)
        x$setinverse(m)
        
        ## return the inversed matrix
        m
}
