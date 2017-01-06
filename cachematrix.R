## Cache the inverse of a matrix


## make a list of functions to set and get the value of a matrix, 
## and set and get the vaule of the inverse of the matrix from parent enviroment

makeCacheMatrix <- function(x = matrix()) {
        s <- NULL  ## initialize s (the inverse of a matrix)
        set <- function(y) {    
                x <<- y       ## assign the input argument to x (a matrix here )in parent environment
                s <<- NULL    ## assign null to s in parent environment
        }
        get <- function() x   ## retrieve the value of x from parent environment
        setinverse <- function(solve) s <<- solve  ## assign the input argument to s in parent environment
        getinverse <- function() s ## retrieve the value of s from parent environment
        list(set = set, get = get,   ## assign the four function as an element of a list, and returns it to the parent environment
             setinverse = setinverse,
             getinverse = getinverse)
}


## get the inverse of a matrix from cached data or calucate the inverse and cache it

cacheSolve <- function(x, ...) {  ## x is the list of functions returned from makeCacheMatrix.R
        ## Return a matrix that is the inverse of 'x'
        s <- x$getinverse() ## retrieve the value of s from parent environment
        if(!is.null(s)) {  ## if s exists, get this data
                message("getting cached data")
                return(s)
        }
        data <- x$get()  ##x if s does not exist, retrieve the value of x (a matrix) from parent environment
        s <- solve(data) ## calculate the inverse of the matrix 
        x$setinverse(s) ## assign the inverse of the matrix to s in parent environment
        s ## return s, the inverse of the matrix 
}
