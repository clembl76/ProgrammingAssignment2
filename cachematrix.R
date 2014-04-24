## makeCacheMatrix is a function that allows both to read from cache
# and to put into cache
# values for a matrix and its inverse

makeCacheMatrix <- function(x = matrix()) {
      # x is the cached variable that will store the matrix
      # i is the cached variable that will store inverse of the matrix
      
      i <<- matrix(numeric(),nrow(x),ncol(x))  # initiate i to NULL matrix
      
      set <- function(y) {
            x <<- y
            i <<- matrix(numeric(),nrow(x),ncol(x)) 
      } 
      # set is a function that allows to cache matrix value into x 
      #and initiate inverse matrix value to NULL
      
      get <- function() x # get function simply gets the cached value of its argument (matrix)
      setinverse <- function(inverse) i <<- inverse 
      # setinverse is a function that allows to cache inverse matrix value into i
      
      getinverse <- function() i# get function simply gets the cached value of i
      
      list(set = set, get = get,
           setinverse = setinverse,
           getinverse = getinverse)
      #returns all 4 functions created for this x
}

## cacheSolve: Returns a matrix that is the inverse of 'x' - from cache when existing

## takes a matrix x, checks if inverse matrix has already been computed and cached
## if yes, gets the results from cache
## if no, computes the results and saves it into cache for next time

cacheSolve <- function(x) {
      
      i <- x$getinverse() ## reads from cache 
      
      if(!all(is.na(i))) { # what is the equivalent for a matrix?
            message("getting cached data")
            return(print(i))
      } # if something exists in cache for the inverse matrix, display a message 
      # and return existing cache value (ends function)
      
      # if cache is empty (function has not been exited)
      data <- x$get() # get matrix
      i <- solve(data) # inverse matrix using "solve" function
      x$setinverse(i) # put matrix inverse result into cache
      print(i) # returns computed value     
      
}



# test
# m1<-matrix(5:8,2)
m1<-matrix(1:4,2,2)
m2<-solve(m1)
a<-makeCacheMatrix(m1)
b<-cacheSolve(a)

