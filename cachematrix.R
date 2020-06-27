makeCacheMatrix <- function(mtx = matrix()) { 
    inv <- NULL
    set <- function(y) {  # set the value of the matrix
        mtx <<- y
        inv <<- NULL
    }
    get <- function() mtx # get the value of the matrix
    setsolve <- function(solve) inv <<- solve # set the value of the inverse
    getsolve <- function() inv # get the value of the inverse
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


cacheSolve <- function(mtx, ...) {
    inv <- mtx$getsolve() #check if the inverse of a matrix has already been calculated
    if(!is.null(inv)) { 
        message("Getting Inverse Matrix")
        return(inv)  # If so, it gets the inverse from the cache and skips the computation
    }
    data <- mtx$get() 
    inv <- solve(data, ...) # Otherwise, it calculates the inverse of the data 
    mtx$setsolve(inv) # Sets the value of the inverse in the cache via the setsolve function.
    inv ## Return a matrix that is the inverse of 'x'
}


##################
##### Example ####
##################
a <- matrix(rnorm(25),5,5) # Firts, enter the matrix to which the inverse will be calculated
b <- makeCacheMatrix(a) # Second, run the function makeCacheMatrix whose input mtx is a matrix
cacheSolve(b) # Finally, this function computes the inverse of the special "matrix" returned by makeCacheMatrix above
