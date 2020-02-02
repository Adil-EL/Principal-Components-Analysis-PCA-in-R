metrics<-function(Y_pred,Y_test,p)
{
 
  SS_Residual<-sum((Y_pred-Y_test)**2)
  SS_Total<-sum((Y_test-mean(Y_test))**2)
  r_squared <-1 - (SS_Residual/SS_Total)
  adj_r_squared <-1 - (1-r_squared)*(length(Y_test)-1)/(length(Y_test)-p-1)
  
 return (adj_r_squared)
}

  