"
--------------------------------------
Functions:    
             makeCacheMatrix( x = numeric())
             cacheSolve(x, ...)

Author:      Cyrus Munshi

Description: 

makeCacheMatrix:
        Given a matrix, return a list of functions that may be used to cache the
        matrix in the outer environment.  In the course assignment this returned 
        object is referred to as a 'special' matrix.

cacheSolve(x, ...):
        Given a special invertable matrix object returned by the makeCacheMatrix
        function, retreive the inverse matrix from cache, if present in the
        cache, otherwise compute and return the inverse after storing in cache.
        Display a message when inverse was found in the cache.

Usage Example:    
        > m<-makeCacheMatrix(matrix(c(4,3,3,2),nrow=2,ncol=2))
        > m$get()
              [,1]  [,2]
        [1,]    4     3
        [2,]    3     2
        > m$getsolve()
        NULL
        > cacheSolve(m)
              [,1]  [,2]
        [1,]   -2     3
        [2,]    3    -4
        > m$getsolve()
              [,1]  [,2]
        [1,]   -2     3
        [2,]    3    -4
        > cacheSolve(m)
        getting cached data
              [,1]  [,2]
        [1,]   -2     3
        [2,]    3    -4
--------------------------------------
"  

## given invertable numeric matrix, return list of functions enabling caching of inverse

makeCacheMatrix <- function(x = numeric()) {
    # create reference
    s <- NULL

    # define function to cache a new matrix and clear the old inverted matrix
    # from the cache.
    set <- function(y) {
        x <<- y
        s <<- NULL
    }

    # define function returning matrix (cached).
    get <- function() x

    # define function to save inverted matrix to outer environment (cache).
    setsolve <- function(solve) s <<- solve

    # define function to retreive cached inverted matrix.
    getsolve <- function() s

    # create and return list of functions.
    list(set= set, get= get, setsolve= setsolve, getsolve= getsolve)
}


## Return inverted matrix from cache, or compute and return if not cached.

cacheSolve <- function(x, ...) {

  # retreive cached value of matrix inverse and 
  # return to caller if found.
  s <- x$getsolve()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }

  # matrix inverse not found in cache.

  # retreive original matrix from cache and compute its inverse. 
  data <- x$get()
  s <- solve(data, ...)

  # store inverse matrix in cache
  x$setsolve(s)
  
  # return inverse matrix to caller
  s
}
