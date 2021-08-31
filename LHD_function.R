#*****************Description***********************
#  % This function generates a Latin Hypercube Design.
#% VIFs are used to reduce multicollinearity.

#%*****************Input Variables*******************
#  % n = desired number of design points
#  % var = number of predictor variables


library(MASS)

LHDesign <- function(n,var){
  
  n<-n
  var<-var
  
  
  x<-matrix(0L,nrow=n,ncol=n)
  
  for (i in 1:n){
  x[i,]<-sample(1:n,n)
  
  }
  
  # %***************Vector Permutations*****************
  # % It first generates n random permutations of 1:n 
  # % then it randomizes their order.
  # % D is n x n
  # % D contains n row vectors with random permutations
  
  
  order<-sample(1:n,n)
  
  D<-matrix(0L,nrow=n,ncol=n)
  
  for(i in 1:n){
    
    D[i,]<-x[order[i],]
    
    
  }
  
  #LHD places the permutations in columns and with values from 0 - 1
  LHD=(t(D)-0.5)/n
  
  # The loop calculates the inverse correlation matrix, obtains
  # VIFs from diagonal, then it deletes the column corresponding
  # to the maximum VIF.
  # The loop ends the # of columns = # of variables.
  # Final VIFs are printed out to verify that they are less than 5.
  
  for (k in seq(n,(var+1),-1)){
    
    Invcorrm<-ginv(LHD)
    VIF<- diag(Invcorrm)
    pos<-which.max(VIF)
    LHD<-LHD[,-pos]
}
  Invcorrm<-ginv(LHD)
  final_vif<-diag(Invcorrm)
 LHD<-(LHD*n)+0.5
 output<-list()
 output[[1]]<-final_vif
 output[[2]]<-LHD
return(LHD)  
}