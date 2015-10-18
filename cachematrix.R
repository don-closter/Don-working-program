# dkcloster@comcast.net
# R programming
# programming assignment 2
#
# Program takes a square matrix input and returns the matrix inverse
#       If the inverse has been previously computed, inverse is pulled from cache
#       to save computation time.

# makeCacheMatrix function
#       Takes the input matrix, stores a copy to cache, and sets up the set & get functions
#
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) inv <<- solve
        getinverse <- function() inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

#cacheSolve function
#       Computes the matrix inverse if it has not been previously calculated.
#       If it was calculated, it pulls the inverse from cache.
   
cacheSolve <- function(x, ...) {
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

# test program
# Q<- makeCacheMatrix(matrix(c(2,5,9,3,5,7,4,5,6),nrow=3,ncol=3))
# makeCacheMatrix(Q)
# cacheSolve(Q)
# cacheSolve(Q)