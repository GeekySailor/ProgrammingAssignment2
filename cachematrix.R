##On this R file you will find 2 functions.  One that creates
##creates the inverse of a matrix and 2 that computes it.


## This function takes a matrix as an argument and caches its inverse
makeCacheMatrix <- function(x = matrix()) {
        
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
    
}


##This function computes the inverse of a matrix
cacheSolve <- function(x, ...) {

    m <- x$getInverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    ## Return a matrix that is the inverse of 'x'
    m
}

##Use this code to test
##Thanks to Gregory D. Horne for posting code to test
##https://class.coursera.org/rprog-003/forum/thread?thread_id=650
amatrix = makeCacheMatrix(matrix(c(1,2,3,4), nrow=2, ncol=2))
amatrix$get()         # Returns original matrix
cacheSolve(amatrix)   # Computes, caches, and returns    matrix inverse
amatrix$getInverse()  # Returns matrix inverse
cacheSolve(amatrix)   # Returns cached matrix inverse using previously computed matrix inverse