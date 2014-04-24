## makeCacheMatrix is a function that allows both to read from cache
# and to put into cache
# values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      # x is the cached variable that will store the matrix
      # i is the cached variable that will store inverse of the matrix
      
      i<-matrix(rep(NA,4),nrow(x),ncol(x)) # initiate i to NULL
      
      
      set <- function(y) {
            x <<- y
            i<-matrix(rep(NA,4),nrow(x),ncol(x)) 
            
      } 
      # set is a function that allows to cache matrix value into x 
      #and initiate inverse matrix value to NULL
      
      get <- function() x # get function simply gets the cached value of its argument (matrix)
      setinverse <- function(inverse) i <<- inverse 
      # setinverse is a function that allows to cache inverse matrix value into i
      
      
      getinverse <- function() i # get function simply gets the cached value of i
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      # add comment here!!! show function results?
}

## cacheSolve: Returns a matrix that is the inverse of 'x' - from cache when existing

## takes a matrix x, checks if inverse matrix has already been computed and cached
## if yes, gets the results from cache
## if no, computes the results and saves it into cache for next time

cacheSolve <- function(x) {
      
      # i <- getinverse() ## reads from cache 
      i <- x$getinverse() ## reads from cache 
      
      if(!is.null(i)) {
            message("getting cached data")
            return(i)
      } # if something exists in cache for the inverse matrix, display a message 
      # and return existing cache value (ends function)
      
      # if cache is empty (function has not been exited)
      data <- x$get() # get matrix
      i <- solve(data) # inverse matrix using "solve" function
      x$setinverse(i) # put matrix inverse result into cache
      i # returns computed value     
      
}



# tests 
x<-matrix(1:4,2,2)
makeCacheMatrix(x)

m1<-matrix(1:4,2,2)
m1
m2<-solve(m1)
m2
cacheSolve(m1)

# > cacheSolve(m1)
# Error in x$getinverse : $ operator is invalid for atomic vectors

