

## function makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse
## cacheSolve: This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        m<-NULL
        set<-function(y){
                x <<- y
                m <<- NULL
        }
        get<-function() x    # get the matrix
        
        setmatrix <- function(solve)  m <<- solve  # setting the inverse of matrix
        
        getmatrix <- function() m    # getting the inverse value of matrix
        
        list(setmatrix=setmatrix,    # list to set and get the matrix and its inverse
             getmatrix=getmatrix,
             set=set, get=get)
}


cacheSolve <- function(x=matrix(), ...) {
        m <- x$getmatrix()
        
        
        if(!is.null(m)){                            ##getting inverse from the cache
                message("getting cached data")
                return(m)
        }
        
        matrix <- x$get()              #other wise get the matrix
        
        m <- solve(matrix, ...)         #solve for its inverse
        
        x$setmatrix(m)                  #set the value of inverse
        m                               #return the inverse
}