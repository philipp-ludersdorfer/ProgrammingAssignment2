## Programming assignment 2: 
## The following pair of functions allow one to cache the inverse of a matrix.

## The first function, makeCacheMatrix creates a special matrix object (it's 
## actually a list) containing 4 functions: (1) set and (2) get the value of the
## matrix, and (3) set and (4) get the inverse of the matrix

makeCacheMatrix <- function(x = matrix())  {
    m <- NULL                           # initialize 'm' (used for the inverse) as empty
    set <- function(y) {                # create function 'set' which assigns the ...
        x <<- y                         # input arg. 'y' to 'x' and sets ...
        m <<- NULL                      # 'm' to empty (the 'set' function is ...
    }                                   # actually not necessary for this assignment)
    get <- function() x                 # create function 'get' which returns 'x' (the matrix) 
    setinverse <- function(solve) {     # create function 'setinverse' ...
        m <<- solve                     # which assigns the input arg. 'solve' to 'm'
    }  
    getinverse <- function() m          # create function 'getinverse' which returns 'm'
    list(set = set, get = get,          # return list (of the created functions)
         setinverse = setinverse,
         getinverse = getinverse)
}


## The second function, cacheSolve either computes the inverse of the special 
## "matrix" returned by makeCacheMatrix or, if the inverse has already been 
## calculated (and the matrix has not changed), retrieves the inverse from 
## the cache.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()                 # get cached inverse of 'x' and assign to 'm'
    if(!is.null(m)) {                   # if 'm' is not empty (= inverse already computed) ...
        message("Getting cached data")  # print this message, ...
        return(m)                       # return the inverse and quit the function
    }                                   # (if m is empty:)
    data <- x$get()                     # get the matrix 'x' and assign to 'data'
    m <- solve(data, ...)               # compute the inverse of data and assign to 'm'
    x$setinverse(m)                     # set 'm' as the inverse of 'x'
    m                                   # return the inverse (m)
}