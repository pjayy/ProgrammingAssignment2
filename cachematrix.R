## Put comments here that give an overall description of what your
## functions do

#makeCacheMatrix stores a matrix (e.g. x) and cacheSolve finds the inverse 
#of the stored matrix (e.g. x).

## Write a short comment describing this function

#makeCacheMatrix creates an empty matrix which is then filled by the 
#variable in the function parameters. It is then converted to a list
#for later use.

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y){
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


## Write a short comment describing this function
#cacheSolve first calls the matrix stored as a list using $ operator and
#getInverse from the makeCacheMatrix function. Then, it checks to see if
#that particular matrix has already been solved. If so, it returns a 
#message instead of rerunning the solve fucntion on it. If it is the first 
#time running solve on the matrix, then it will be solved normally.
cacheSolve <- function(x, ...) {
        m <- x$getInverse()
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setInverse(m)
        m
}
