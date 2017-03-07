## The first function, makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#set the value of the matrix
#get the value of the matrix
#set the value of the inverse
#get the value of the inverse

#The second function calculates the inverse of the special "vector" created with the above function. However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the value of the inverse in the cache via the setinverse function.

## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        #initialize variable
        m <- NULL
        
        
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        
        #get the contents of the matrix
        get <- function() { return(x) }
        
        #cache the contents of the inverse matrix
        setinverse <- function(inverse) { m <<- inverse  }
        
        #get the cached contents of the inverse matrix
        getinverse <- function() { return(m) }
        
        #list of functions
        list(
          set = set, 
          get = get,
          setinverse = setinverse,
          getinverse = getinverse
          )
        
}

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        #look for cached value and return if exist
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        
        #get matrix
        data <- x$get()
        
        #calculate the inverse
        m <- solve(data)
        
        #store the inverse
        x$setinverse(m)
        
        #return the inverse
        m
        
}

#See it in action with 3x3 matrix...

myMatrix <- matrix(c(1,2,3,4,1,2,3,4,5),3,3)

newMatrix <- makeCacheMatrix(myMatrix)

cacheSolve(newMatrix) #inverse returned after computation
