## These functions calculate and cache the inverse of a matrix, improving run time and negating the need to perform
## the matrix inversion multiple times.  



## makeCacheMatrix creates a special object (a list of functions) that allows the assignment, storage and 
## retreival of a matrix and its cached inverse. The function accepts the matrix to be inverted as an argument.


makeCacheMatrix <- function(x = matrix()) {
        
        #create a null object that will store the inverted matrix. 
        i <- NULL
        
        #sets the matrix to be inverted directly and resets the inversion to null (i.e. empties the cache). 
        set <- function(y) {
                x <<- y
                i <<- NULL
        }

        #returns the matrix.
        get <- function() x
        
        
        #sets the inverted matrix from an argument passed directly.  
        setInverse <- function(inverse) i <<- inverse

        ##returns the inverted matrix. 
        getInverse <- function() i

        ##returns the "matrix object" (i.e. a list of functions)
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve is a companion function to makeCacheMatrix.  The function accepts a special matrix object 
##(created using makeCacheMatrix) and returns the inverse of the matrix of interest. If the matrix's inverse 
##has already been cached, it retrieves it from the makeCacheMatrix environment.  If not, the function calculates 
##the inverse, and caches it for future quick retreival.   

cacheSolve <- function(x, ...) {

        # tests if the inverted matrix already exists in the cache. 
        i <- x$getInverse()
        
        #if it exists, it returns the cached inverted matrix. 
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        #if it doesn't exist, calculate the inverse and store it in the cache. 
        data <- x$get()
        i <- solve(data, ...)
        x$setInverse(i)

        ## Return a matrix that is the inverse of 'x'
        i
        
}
