#
#> given matrix m below
#     [,1] [,2]
#[1,]    4    3
#[2,]    3    2
#
# you can use makeCackeMatrix to save m, and retrieve it's inverse.
# > mm <- makeCacheMatrix(m)
# you can change the matrix at amy time:
# > mm$set(m2)

# it creates a list of functions (set, get, setinverse, getinverse) that can set or get a matrix (one was originally passed in),
# as well as set (change) or get the inverse of that matrix.  the saved inverse can be retrieved from cache
# note that this function gives you the ability to cache the inverse of a matrix using lexical scoping, but it doesn't 
# have any logic about when to do so.
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL
    set <-function(y) {
        x <<- y
        inverse <<- NULL
    }
    get <- function() x
    setinverse <- function(inv) inverse <<- inv
    getinverse <- function() inverse
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

# here's how you may use this, given a matrix cached through makeCackeMatrix above:
# > m_inv <- cacheSolve(mm)
# will return the inverse of m:
#      [,1] [,2]
#[1,]   -2    3
#[2,]    3   -4
# calling it a second time, it will report that it returns a cached value:  "getting cached data"
# cacheSolve(mm) returns the inverse of the matrix that was originally stored with mm (see comments above)
# when called multiple times, it doesn't run solve again - it asks mm for the inverse it has already computed and stored
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}
