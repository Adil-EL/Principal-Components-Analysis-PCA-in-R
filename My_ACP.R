
My_acp<-function(X)
{
  Xdata<-X
  p<-ncol(X)
  n<-nrow(X)
  
  # centrer la matrice
  
  for(i in 1:p)
  {
    X[,i]<-X[,i]-mean(X[,i])
  }

  My_cov<-t(X)%*%X
  My_cov<-My_cov/(n-1)
  sd_vector<-GMCM:::colSds(X)
  sd_matrix<-sd_vector%*%t(sd_vector)
  
  
  My_cor<-My_cov/sd_matrix
  
  
 lambda<-eigen(My_cor)$values
  
  W<-eigen(My_cor)$vectors
  
  
  nbrcp<-sum(lambda>=1)
  
  
  barplot(lambda)
  title("cascade de valeurs propres")
  
  # règle de Kaiser-Guttman choix des valeurs propres >= 1 
  
  Inr_cum<-cumsum(lambda)/p
  barplot(Inr_cum)
  title("cascade des valeurs propres cumulées ")
  
  C<-Xdata%*%W # Les composantes principales
  Cp<-Xdata%*%W[,1:nbrcp]
  
  C_carre<-C*C
  Cp_carre<-Cp*Cp
  Qinformation<-rowSums(Cp_carre)/rowSums(C_carre)
  
  Gamma<-matrix(0,nrow=n,ncol=p)
  for (j in 1:p)
  {
    Gamma[,j]<-C_carre[,j]/lambda[j]
  }
  
  Gamma<-Gamma/n
  
  #6-dudi.pca(Xdata)
  
  R<-matrix(0,nrow=p,ncol=p)
  for(j in 1:p)
  {
    R[,j]<-sqrt(lambda[j])*W[,j] # les coefficient de corelation linéare 
  }
  
  # plot
  
  #plot(Cp[,1],Cp[,2])
  #plot(Cp[,1],Cp[,3])
  #plot(Cp[,2],Cp[,3])
  #print("le vecteur  qualité d'information")
  result<-list(nbrcp,My_cor,Qinformation)
  
  return(result)
}
