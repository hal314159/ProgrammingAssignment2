## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
##  superassignment finds NEW, OLD, CACHEDINVERSE below which act as caches

makeCacheMatrix <- function(x = matrix()) {

NEWMATRIX<-OLDMATRIX<-CACHEDINVERSE<-NULL
  #
  # cache the new matrix

    NEWMATRIX<-x
 # 
  getNewMatrix<-function() NEWMATRIX
  getOldMatrix<-function() OLDMATRIX
  setOldMatrix<-function(v) OLDMATRIX<<-v
  getCachedInverse<-function() CACHEDINVERSE
  setCachedInverse<-function(v) CACHEDINVERSE<<-v

  list(getNewMatrix=getNewMatrix, getOldMatrix=getOldMatrix, setOldMatrix=setOldMatrix,
       getCachedInverse=getCachedInverse, setCachedInverse=setCachedInverse)

}

## Write a short comment describing this function
## (1) get newi, old and cached matrix from the parent frame
## (2) if oldmatrix == newmatrix then return cached inverse matrix
## (3) if oldmatrix != newmatrix ithen calculate inverse, cache it, cache new matrix as oldmatrix and return inverse matrix. 
## additional functions used: MATRIXisSAME (see below)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   newMatrix<-x$getNewMatrix()
   oldMatrix<-x$getOldMatrix()
   cachedInverse<-x$getCachedInverse()
   #------------------
  if(MATRIXisSAME(newMatrix, oldMatrix)){
    message("matrices are same, returning cached value")
    return(cachedInverse)  }
   #---------------------
   # not the same, (1) copy newMatix to oldMatrix and cache it,  (2) compute Inverse, (3) cache & return
   message("matrices are different, returning new inverse and caching it")
   x$setOldMatrix(newMatrix)
   cachedInverse<-solve(newMatrix,...)
   x$setCachedInverse(cachedInverse)
   return(cachedInverse)
}

## You will need this function I wrote to compare matrices
MATRIXisSAME <- function(x, y){
  is.matrix(x) && is.matrix(y) && dim(x) == dim(y) && all(x == y)
}
