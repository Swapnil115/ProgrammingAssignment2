# For a very long vector, it may take too long to compute the mean, 
# especially if it has to be computed repeatedly (e.g. in a loop). 
# If the contents of a vector are not changing, it may make sense to cache 
# the value of the mean so that when we need it again, it can be looked up 
# in the cache rather than recomputed. In this Program we will 
# take advantage of the scoping rules of the R language and how they can be 
# manipulated to preserve state inside of an R object in this case a matrix.

# makeCacheMatrix creates a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	# inv will store the cached inverse matrix
    inv <- NULL

    # setter for matrix
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    # getter for matrix
    get <- function() x

    # setter for inverse
    setinverse <- function(inverse) inv <<- inverse

    # getter for inverse
    getinverse <- function() inv

    # return matrix
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


# cacheSolve: Computes the inverse of the matrix. If the inverse is already
# calculated before, it returns the cached inverse;
# else it will calculate the inverse, cache it and then return it.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    # if inverse is already calculated then return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    # if not, calclutate it
    data <- x$get()
    inv <- solve(data, ...)

    # cache it
    x$setinverse(inv)

    # return it
    inv
}

# Sample run:
# > x <- matrix(rnorm(16), nrow = 4)          // Create a matrix x
# > cx <- makeCacheMatrix(x)                  // Create our special matrix
# > cx$get()                                  // Return the matrix
# > cacheSolve(cx)                            // Return the inverse
# > cacheSolve(cx)                            // Call the 2nd time, to return
#                                             // the cached inverse