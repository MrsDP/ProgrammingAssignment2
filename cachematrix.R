## The following function creates a a "matrix" object and 
## caches its inverse.
## A matrix has an inverse only if it is square (# of rows = # of columns)
## and is not singular(its determinant is not 0)
 

##The default value of mkcmat() is a matrix object. 

makeCacheMatrix <- function(m=matrix())
{
      
## initializing objects
      
   m_inv<-NULL 
   oldm<<-NULL
      
      
## defining functions
      
## to print matrix
   
   gm<-function()
   {
      m
   }
      
## to input a new matrix
      
   sm<-function(y)
   {
      m<<-y
   }

## to print inverse
      
   gi<-function()
   {
      m_inv
   }

## to store inverse
      
   si<-function(i)
   {
      m_inv<<-i
   }

## initializing 'matrix' vector
      
   list(get_mtrx = gm, set_mtrx = sm, get_inv = gi, set_inv=si)  
}    


## The following function returns a matrix that is the inverse of 'm'

cacheSolve <- function(m, ...) 
{
   
## accessing stored matrix   
   
   mtrx<-m$get_mtrx() 

## Checking if matrix is square
   
   if(nrow(mtrx)!= ncol(mtrx))
   {
      cat('\n Matrix not square. Cannot compute inverse.\n')
      return()
   }

## Checking if matrix is singular

   if(det(mtrx)==0)
   {
      cat('\n Singular matrix. Cannot compute inverse.\n')
      return()
   }

## checking if inverse has already been calculated
## or if the previously computed matrix and the current matrix 
## are the same
   
## accessing stored inverse
  
   m_inv<-m$get_inv()

## retrieving matrix stored earlier
   
   old_mat<-oldm

## Checking for pre-existing inverse 
## and if matrices are the same 
   
   if(!is.null(m$get_inv())&(identical(mtrx,old_mat)==TRUE))
   {
      
      cat("\n\n Inverse already exists.\n")
      cat("\n Retrieving cached value of inverse...\n\n")
      return(m_inv)
   }

## Computing inverse
   
   m_inv<-solve(mtrx)

## Storing inverse 
  
   m$set_inv(m_inv)

## Storing the current matrix
   
   oldm<<-mtrx

## Printing inverse

   cat("\n\n Computing Inverse...\n\n")
   m_inv

}
