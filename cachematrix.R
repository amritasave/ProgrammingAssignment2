

#This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

          inverseMat <- NULL	#setting the value for variable inverseMat to NULL.
	                        #setMatrix function sets the new value of the matrix to be inversed 
	                        #and also sets NULL to inverseMat variable.
	  
	  setMatrix <- function(y) 
	  {
                x <<- y
                inverseMat <<- NULL	
          }

	  #getMatrix function returns the value of the matrix which needs to be inversed.

	  getMatrix <- function()
  	  { 
		    x 
	  }

	  #setInverseMatrix function sets the inversed value of the matrix in the cache.

          setInverseMatrix <- function(inverseVar)  
	  {	    
		    inverseMat <<- inverseVar
	  }

	  #getInverseMatrix function returns cached value of the matrix.
 
          getInverseMatrix <- function()  
	  {	
		    inverseMat
          }

	  #creating a list with the setMatrix , getMatrix ,setInverseMatrix ,getInverseMatrix functions.
        
	  list( setMatrix = setMatrix , getMatrix = getMatrix,
             setInverseMatrix = setInverseMatrix,
             getInverseMatrix = getInverseMatrix)


}


#This function computes the inverse of the special "matrix" returned 
#by makeCacheMatrix above. 
#If the inverse has already been calculated 
#(and the matrix has not changed),
#then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
         inverseMat <- x$getInverseMatrix()	#Calling getInverseMatrix to retrive cached value.
        if(!is.null(inverseMat)) 
	  {
	  	message("Getting cached data")  #Printing the cached value as inverseMat is not null.
                return(inverseMat)
          }
	  
          data <- x$getMatrix() 		#Getting the matrix value to be inversed.
	  inverseMat <- solve(data)   		#Return the inverse of the matrix.
	  x$setInverseMatrix(inverseMat)    	#Set inverse value in cache.
          inverseMat
}
