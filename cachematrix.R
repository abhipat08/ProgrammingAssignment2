#This function creates a special "matrix" object that can cache its inverse.
# It takes in a numeric matrix as input and creates 4 list objects
# i.e. set the matrix, get the matrix, set the inverse of matrix & get inverse

makeCacheMatrix <- function (x = matrix(numeric())) {
#Ensure i is assigned is a null object
        i <- NULL

#Create a new matrix that will require minimal steps after the first run of
# makeCacheMatrix.  We can use x$set to set the data in round 2 onwards
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
#Assign x to x$get
        get <- function() x
#assign the computed inverse matrix to m
        setinverse <- function(inv) i <<- inv
#assign the inverse to x$getinverse, so it can be called if inverse is already 
# assigned
        getinverse <- function() i
#group x$set, x$get, x$setinverse & x$getinverse as list objects
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


#This function computes the inverse of the special "matrix" returned by 
# makeCacheMatrix above. If the inverse has already been calculated 
# (and the matrix has not changed), then the cachesolve would retrieve 
# the inverse from the cache.
cacheSolve <- function(x,...) {

#first check if an inverse has already been created & if so, use that
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached inverse matrix")
                return(i)
        }

#if inverse was not already created, then compute the inverse of the input matrix
        data <- x$get()
        i <- solve(data,...)

#assign the new inverse matrix to i, and return it
        x$setinverse(i)
        i
}
