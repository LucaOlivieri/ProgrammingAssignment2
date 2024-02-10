## The function creates a "magic" Matrix Object
## The Object S3 conatains a matrix and his inverse (x_inv)

makeCacheMatrix <- function(x = matrix()) {
  
  x_inv <- NULL
  
  ##Getter & Setter Method for the "magical" matrix
  
  #set a "magic" matrix 
  setmatrix <-function(y){
    x<<- y
    x_inv <<- NULL
  }
  
  #Return the original matrix 
  getmatrix <-function() x
  
  #Set the inverse matrix calculated with the cacheSolve
  setinvmatrix<-function(inv_matrix) x_inv <<- inv_matrix
  
  #get the inverse matrix in the magical matrix 
  getinvmatrix<-function() x_inv 
  
  #Return a list with all function usefull for "comunicate" with the "magical" matrix
  list(getmatrix=getmatrix,setmatrix=setmatrix,setinvmatrix=setinvmatrix,getinvmatrix=getinvmatrix)

}


## The function can store an inverse matrix in a "magical" matrix object (x is a "magical" object)


cacheSolve <- function(x, ...) {

        ##get a possible value of an inverse matrix 
        x_inv<-x$getinvmatrix()
        
        ##Test if the inverse matrix is already cached
        if(!is.null(x_inv)) {
          
            #The inverse matrix is calculated
            message("getting chached data")
            
            return(x_inv)
        }

        #inverse matrix process
        
        #get the original matrix
        x_tmp<-x$getmatrix()        
        
        #save the inverse matrix object
        x$setinvmatrix(solve(x_tmp))
        
        #return the inverse matrix
        solve(x_tmp)
        
}
