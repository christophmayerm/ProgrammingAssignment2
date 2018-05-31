## the following function returns a list that can be used to store a matrix and its inverse
## the function always provides helper-function to get or set the data or the inverse

makeCacheMatrix <- function(m = matrix()) {
        # initialize variable to store inverse of matrix with NULL
        inversem <- NULL
        # provide function to set matrix
        set <- function(y){
                # use double arrow << to make m available in environment of calling function
                m <<- y
                # reset variable that stores inverse if new matrix is assigned
                inversem <<- NULL
        }
        # get function to access matrix
        get <- function() m
        # function to store inverse of the matrix m
        setinverse <- function(inv) inversem <<- inv
        # function to access the inverse of m if already calculated 
        getinverse <- function() inversem
        # return a list with all created functions
        # the environment of these functions contains the variables
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## Following function checks if the inverse of a matrix has been calculated already and calculates the inverse if necessary
## the function stores the calculated value in the matrix object

cacheSolve <- function(m, ...) {
        # get the inverse of the matrix
        inversem <- m$getinverse()
        # check if inverse has already been calculated and cached, return inverse if cached
        if(!is.null(inversem)){
                message("getting cached data")
                return(inversem)
        }
        # if inverse is not in cache, get matrix
        data <- m$get()
        # get the inverse of the matrix using the solve() function
        inversem <- solve(data, ...)
        # cache the inverse in the m object
        m$setinverse(inversem)
        
        # return the object m that conatins the matrix and the inverse
        invisible(m)
}
