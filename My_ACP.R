

# 1- Upload the data into the working space plus some preprocessing.
  Xdata<-read.csv2('data_PDE19.csv')
  Xdata<-Xdata[,2:9]
  X<-Xdata


# 2- Get the data dimensions
  p<-ncol(X)
  n<-nrow(X)
  
# 3- normalize your data
  
  X<-scale(X, center = T, scale = T)
  
  
  
# 4- the correlation matrix 
  
  My_cor<-cor(X)
  
 
#The eigen values and vectors
  
 lambda<-eigen(My_cor)$values
 
  W<-eigen(My_cor)$vectors
  
  # the number of principal components
  nbrpc<-sum(lambda>=1) # there is many cretarias to determine them 
                        #for this case we chose the components for  wich 
                        #the eigen value is >= 1, the Kaiser-Guttman rule
  
  
  barplot(lambda)
  title("cascade de valeurs propres")
  
  # règle de Kaiser-Guttman choix des valeurs propres >= 1 
  
  Inr_cum<-cumsum(lambda)/p
  barplot(Inr_cum)
  title("cascade des valeurs propres cumulées ")
  
  C<-as.matrix(Xdata)%*%W # the projection of the variables' matrix on the new space
 
  
  Cp<-C[,1:nbrpc] # selecting the principale components
  
 
 # the Cp matrix  descibes the individuals using fewer variabales
 # and saving a maximum of variation.

  
# The code above is a right to the point procedure,to provide more details
# you can use the built in fuction below: 
# PCA from the FactoMiner package
  

#As a reminder the pca is used as a dimentiality reduction method,
#But it still have many limitations.
  

