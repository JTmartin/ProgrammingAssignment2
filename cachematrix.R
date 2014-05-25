## cachematrix.R contains two functions (and some sub functions) 
## that together decrease the computational cost of getting the 
## inverse of a matrix.
## These functions together place in cache the inverse of a matrix and 
## verify if the matrix has changed a new inverse of the matrix will 
## be calculated

## makeCacheMatrix generates an empty matrix. Set populates the cached
## matrix and get returns values of the cached matrix.
## setinverse is called from cacheSolve to store an inverse matrix
## getinverse is called from cacheSolve to retrieve the cached inverse
## matrix

makeCacheMatrix <- function(x = matrix()) {
  
  inverseMatrix <- NULL
  set <- function(y) {
    x <<- y
    inverseMatrix <<- NULL
  }
  get <- function() x  
  
  setinverse <- function(loc_inverse) inverseMatrix <<- loc_inverse
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve checks if the inverse matrix is null and then verifys
## that the cached inverse matrix is the inverse of the cached matrix.
## This is accomplished by multiplying the cached inverse matrix and 
## cached matrix comparing the result to an identity matrix. A logical 
## matrix is generated and summed. If the row sums (which are 1 for true)
## equal the number of elements in each row the cached inverse matrix is 
## retrieved else a new inverse matrix is calculated and cached

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseMatrix<-x$getinverse()
  
  
  if(!is.null(inverseMatrix)){
    ## store the identity matrix
    identMatrix<-x$get()%*%inverseMatrix
    
    ## testMatrix is a logical matrix created by comparing
    ## individual elements of the identMatrix with a generated
    ## identity matrix using the diag function
    testMatrix<-identMatrix==diag(nrow(x$get()))
    
    ## test is a vector of dimension = to the number of rows
    ## elements of the test vector will = number of columns 
    ## if every element in testMatrix is true
    test<-rowSums(testMatrix)
    if(test[1]==ncol(x$get())&&test[2]==ncol(x$get())){
      message("getting cached data")
      return(inverseMatrix)
    }
    
  }
  
  ## code run if inverseMatrix is NULL or cached matrix has not changed
  data <- x$get()
  inverseMatrix <- solve(data)
  x$setinverse(inverseMatrix)
  inverseMatrix
  
}
