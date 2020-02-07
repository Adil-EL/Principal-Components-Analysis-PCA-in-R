# -*- coding: utf-8 -*-
"""
Created on Thu Feb  6 08:40:18 2020

@author: Adil
"""
# Required packages
import numpy as np

def My_PCA(X):
    #  Save the original input Get the data dimensions
    Xorg=X
    n=X.shape[0]
    p=X.shape[1]
    
    #  Normalize the data
    X=X-np.mean(X,axis=0)
    X=X/np.std(X,axis=0)
    
    #  The correlation matrix 
    cormat=np.corrcoef(X)
    
    # The eigen values and vectors
    lmd=np.linalg.eig(cormat)[0]
    indices=np.argsort(lmd)[::-1]
    W=np.linalg.eig(cormat)[1]
    W=W[:,indices]
    
    
    # the projection of the variables' matrix on the new space
    X=np.matmul(W,Xorg)
    
    return X
    




  
 
  