## Put comments here that give an overall description of what your
## functions do

#       The makeCacheMatrix function creates a special matrix object
#       to cache the input matrix and its inverse (if it has been provided by 
#       cacheSolve). It provides/returns a list of functions (used to
#       store and retrieve these matrices from cache).

#       The cacheSolve function first checks to see if the inverse matrix 
#       has already been cashed by makeCacheMatrix, if so (and the input 
#       matrix has not changed) it returns that inverse matrix. If not it
#       calculates the inverse matrix, then gives it to makeCacheMatrix
#       to be cached along with the input matrix, as well as returning 
#       the calculated inverse matrix.
#       This saves time by not having to calculate an inverse of a matrix
#       if it has already been done.


## Write a short comment describing this function

#makeCacheMatrix is a function that creates a special "matrix" object
#caching the input matrix and its inverse. See 0 - 5 below. 

makeCacheMatrix <- function(x = matrix()) {             #0 input matrix (x)
        
        im <- NULL
        set <- function(y) {  #1 set the value of the input matrix in cache
                x <<- y       #  and m to null
                m <<- NULL
        }
        
        get <- function() x     #2 get the value of the input matrix (x)
        
        #3 set the value of the inverse matrix (m)
        setinverse <- function(inverse) m <<- inverse   
        
        getinverse <- function() m    #4 get the value of the inverse matrix (m)
        
        list(set = set, get = get,           #5 Return a list of the functions 
             setinverse = setinverse,        #  to access the cached matrices
             getinverse = getinverse)
}


## Write a short comment describing this function

#If it has not already been calculated, function cacheSolve calculates the inverse
#of the matrix object (x) from makeCasheMatrix and gets makeCasheMatrix to cache 
#the value of this inverse matrix.... and then returns the inverse matrix. If the 
#inverse has already been calculated cacheSolve just retrieves that and returns it.

cacheSolve <- function(x, ...) {
        
        m <- x$getinverse()                       #get inverse from cache 
                                                  #and if not null returns it
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        data <- x$get()        #if m is null, get the stored input matrix,  
        m <- solve(data, ...)  #calculate its inverse
        x$setinverse(m)        #and store that in cache together with original matrix.
        m               ## Return a matrix that is the inverse of 'x'
}
