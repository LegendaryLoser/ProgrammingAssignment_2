#This file is created to calculate the inverse of a matrix via caching it so that there is no need
#to repeatedly calculate the inverse matrix

#This function will create the cache of the matrix. The functions within the functions will be named
#The function will have the capability to modify the set matrix in the parent function through the
#child set functions
#The function will also have the capability to let other functions extract the the original matrix and
#the inverse matrix through the get functions defined as a part of the parent makeCacheMatrix function

makeCacheMatrix <- function(x = matrix()) 
{ inverseMatrix <- NULL
  setMatrix <- function(y)
  {
    x <<-y
    inverseMatrix <<- NULL
  }
  setInverse <- function(solve)
  {
    inverseMatrix <<- solve
  }
  getMatrix <- function()
  {
    x
  }
  getInverse <- function()
  {
    inverseMatrix
  }
  list(setMatrix=setMatrix,setInverse=setInverse,getInverse=getInverse,getMatrix=getMatrix)
}


#This Function will be instrumental in extracting the cached inverse matrix stored in the makeCacheMatrix
#function. However if the value in the said function is NULL then this function will calculate the
#inverse matrix from scratch and cache it in the make cache function

cacheSolve <- function(x, ...) 
{
  inverseMatrix <- x$getInverse()
  if(!is.null(inverseMatrix))
  {
    message("getting cached inverse matrix")
    return(inverseMatrix)
  }
  else
  {
    Data <- x$getMatrix()
    inverseMatrix <- solve(Data)
    x$setInverse(inverseMatrix)
    inverseMatrix
  }
        ## Returns a matrix that is the inverse of 'x' and if for the first time updates the cache
}
