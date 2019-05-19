
{
{
  ## ----setup, include=FALSE------------------------------------------------
  closeAllConnections()
  rm(list=ls())              # Clear variables
  graphics.off()             # Clear plot windows
  # General purpose packages (data handling, pretty plotting, ...)
  library(tidyverse)
  library(latex2exp) # Latex in ggplot2 labels
  cbPalette <- c(
    "#999999", "#E69F00", "#56B4E9", "#009E73",
    "#F0E442", "#0072B2", "#D55E00", "#CC79A7") # colour-blind friendly palette
  theme_set(theme_minimal()) # Set a less verbose standard theme
  
  # Packages for actual computation
  library(dimRed)
  library(plot3D)
  library(plotly)
  library(mvtnorm)
  library(fungible)
  
  # linear model
  require(foreign)
  require(MASS)
  library(rdetools)
}

# Data:
  
  ## ------ 2 planes different dim space Not Used------
  # Collapse the if, if not used
  if(0){
    nDim <- 2; nC1 <- 500; nC2 <- 500;
    muC1 <- rep(0,2*nDim); 
    sigmaC1 <- 10; 
    sigmaMatC1 <- sigmaC1*diag(2*nDim); 
    sigmaMatC1[1:nDim,1:nDim]<-matrix(0,nDim);
    
    muC2 <- rep(0,2*nDim); muC2[1] <- 0; 
    sigmaC2 <- 10;  
    sigmaMatC2 <- sigmaC2*diag(2*nDim);
    sigmaMatC2[(nDim+1):(2*nDim),(nDim+1):(2*nDim)]<-matrix(0,nDim);
    #set.seed(10)
    xC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
    xC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
    
    
    sigmaNoise<-5
    nNoise<-20
    outlierNoise1<-MASS::mvrnorm(n = nNoise, rep(0,2*nDim), sigmaNoise*sigmaMatC1, empirical = FALSE)
    outlierNoise2<-MASS::mvrnorm(n = nNoise, rep(0,2*nDim), sigmaNoise*sigmaMatC1, empirical = FALSE)
    
    xC1 <- cbind(rep(1,nC1),xC1)
    xC2 <- cbind(rep(2,nC2),xC2)
    
    xC1[1:nNoise,-1]<-xC1[1:nNoise,-1]+outlierNoise1
    xC1[1:nNoise,1]<--1
    
    xC2[1:nNoise,-1]<-xC2[1:nNoise,-1]+outlierNoise2
    xC2[1:nNoise,1]<--2
    
    
    
    X<-rbind(xC1,xC2)
    
    # Standardize!
    #X[,-1] <- scale(X[,-1])
    #X[,1]<-factor(X[,1])
  }
  
  ## ------ 3 planes different dim space Not Used ------
  if(0){
    nDim <- 4; 
    nClusters<-3;
    
    iCluster<-1
    nC1 <- 500;
    muC1 <- rep(0,nClusters*nDim); 
    sigmaC1 <- 10; 
    sigmaMatC1 <- matrix(0,nClusters*nDim,nClusters*nDim)
    diagonalIndices <-((iCluster-1)*nDim+1):(iCluster*nDim)
    sigmaMatC1[diagonalIndices,diagonalIndices]<-sigmaC1*matrix(1,nDim);
    
    iCluster<-2
    nC2 <- 500;
    muC2 <- rep(0,nClusters*nDim); #muC2[1] <- 0; 
    sigmaC2 <- 10;  
    sigmaMatC2 <- matrix(0,nClusters*nDim,nClusters*nDim);
    diagonalIndices <-((iCluster-1)*nDim+1):(iCluster*nDim)
    sigmaMatC2[diagonalIndices,diagonalIndices]<-sigmaC2*matrix(1,nDim);
    
    iCluster<-3
    nC3 <- 500;
    muC3 <- rep(0,nClusters*nDim); #muC2[1] <- 0; 
    sigmaC3 <- 10;  
    sigmaMatC3 <- matrix(0,nClusters*nDim,nClusters*nDim)
    diagonalIndices <-((iCluster-1)*nDim+1):(iCluster*nDim)
    sigmaMatC3[diagonalIndices,diagonalIndices]<-sigmaC3*matrix(1,nDim);
    
    xC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
    xC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
    xC3<-MASS::mvrnorm(n = nC3, muC3, sigmaMatC3, empirical = FALSE)
    
    
    sigmaNoise<-5
    nNoise<-20
    outlierNoise1<-MASS::mvrnorm(n = nNoise, rep(0,nClusters*nDim), sigmaNoise*sigmaMatC1, empirical = FALSE)
    outlierNoise2<-MASS::mvrnorm(n = nNoise, rep(0,nClusters*nDim), sigmaNoise*sigmaMatC2, empirical = FALSE)
    outlierNoise3<-MASS::mvrnorm(n = nNoise, rep(0,nClusters*nDim), sigmaNoise*sigmaMatC3, empirical = FALSE)
    
    xC1 <- cbind(rep(1,nC1),xC1)
    xC2 <- cbind(rep(2,nC2),xC2)
    xC3 <- cbind(rep(3,nC3),xC3)
    
    xC1[1:nNoise,-1]<-xC1[1:nNoise,-1]+outlierNoise1
    xC1[1:nNoise,1]<--1
    
    xC2[1:nNoise,-1]<-xC2[1:nNoise,-1]+outlierNoise2
    xC2[1:nNoise,1]<--2
    
    xC3[1:nNoise,-1]<-xC3[1:nNoise,-1]+outlierNoise3
    xC3[1:nNoise,1]<--3
    
    
    X<-rbind(xC1,xC2,xC3)
    
    # Standardize!
    X[,-1] <- scale(X[,-1])
    X[,1]<-factor(X[,1])
  }
  
  ## ------ swiss-role-setup, echo=FALSE, cache=TRUE Not Used----------------------------
  if(0){set.seed(2019294729)
    # Number of samples
    n <- 1000
    # Create swiss roll
    x <- matrix(runif(2 * n), nrow = 2)
    v <- 3 * pi / 2 * (0.1 + 2 * x[1,])
    X <- matrix(0, ncol = n, nrow = 3)
    X[2,] <- 20 * x[2,]
    X[1,] <- -cos(v) * v
    X[3,] <- sin(v) * v
    
    X<-t(X)
    X<-cbind(rep(1,n),X)
  }
  
  ## ------ 2 gaussian tops Not Used -------
  if(0){sigmaMatC1<-diag(1,2,2)
  sigmaMatC2<-diag(1,2,2)
  nC1<-500
  nC2<-500
  muC1<-c(0,0)
  muC2<-c(4,0)
  norm2dC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
  norm2dC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
  
  sigmaNoise<-5
  nNoise<-20
  outlierNoise1<-MASS::mvrnorm(n = nNoise, c(0,0), diag(sigmaNoise,2,2), empirical = FALSE)
  outlierNoise2<-MASS::mvrnorm(n = nNoise, c(0,0), diag(sigmaNoise,2,2), empirical = FALSE)
  
  #outlierNoise1 <- rnorm(n = nNoise, 0, sigmaNoise)
  #outlierNoise2 <- rnorm(n = nNoise, 0, sigmaNoise)
  
  xC1<-cbind(rep(1,nC1),norm2dC1,dmvnorm(norm2dC1, muC1, sigmaMatC1, log=FALSE))
  xC1[1:nNoise,-c(1,4)]<-xC1[1:nNoise,-c(1,4)]+outlierNoise1
  xC1[1:nNoise,1]<--1
  
  xC2<-cbind(rep(2,nC2),norm2dC2,dmvnorm(norm2dC2, muC2, sigmaMatC2, log=FALSE))
  xC2[1:nNoise,-c(1,4)]<-xC2[1:nNoise,-c(1,4)]+outlierNoise2
  xC2[1:nNoise,1]<--2
  
  X<-rbind(xC1,xC2)
  }
  
  ## ------ 6 gaussian tops Scale no Scale outliers -------
  if(0){
    sigmaMatC1<-diag(1,2,2)
    sigmaMatC2<-diag(1,2,2)
    sigmaMatC3<-diag(1,2,2)
    sigmaMatC4<-diag(1,2,2)
    sigmaMatC5<-diag(1,2,2)
    sigmaMatC6<-diag(1,2,2)
    nC1<-100
    nC2<-100
    nC3<-100
    nC4<-100
    nC5<-100
    nC6<-100
    
    dist<-4
    muC1<-c(0,0)
    muC2<-c(dist,0)
    muC3<-c(2*dist,0)
    muC4<-c(0,dist)
    muC5<-c(dist,dist)
    muC6<-c(2*dist,dist)
    
    norm2dC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
    norm2dC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
    norm2dC3<-MASS::mvrnorm(n = nC3, muC3, sigmaMatC3, empirical = FALSE)
    norm2dC4<-MASS::mvrnorm(n = nC4, muC4, sigmaMatC4, empirical = FALSE)
    norm2dC5<-MASS::mvrnorm(n = nC5, muC5, sigmaMatC5, empirical = FALSE)
    norm2dC6<-MASS::mvrnorm(n = nC6, muC6, sigmaMatC6, empirical = FALSE)
    
    #sigmaNoise<-0
    #nNoise<-floor((nC1+nC2+nC3)*0.02); print(nNoise)
    #outlierNoise1<-abs(MASS::mvrnorm(n = nNoise, c(0,0), diag(sigmaNoise,2,2), empirical = FALSE))
    #outlierNoise2<-abs(MASS::mvrnorm(n = nNoise, c(0,0), diag(sigmaNoise,2,2), empirical = FALSE))
    #outlierNoise3<-abs(MASS::mvrnorm(n = nNoise, c(0,0), diag(sigmaNoise,2,2), empirical = FALSE))
    
    xC1<-cbind(rep(1,nC1),norm2dC1,dmvnorm(norm2dC1, muC1, sigmaMatC1, log=FALSE))
    #xC1[1:nNoise,-c(1,4)]<-xC1[1:nNoise,-c(1,4)]+outlierNoise1
    #xC1[1:nNoise,1]<--1
    
    xC2<-cbind(rep(2,nC2),norm2dC2,dmvnorm(norm2dC2, muC2, sigmaMatC2, log=FALSE))
    #xC2[1:nNoise,-c(1,4)]<-xC2[1:nNoise,-c(1,4)]+outlierNoise2
    #xC2[1:nNoise,1]<--2
    
    xC3<-cbind(rep(3,nC3),norm2dC3,dmvnorm(norm2dC3, muC3, sigmaMatC3, log=FALSE))
    #xC3[1:nNoise,-c(1,4)]<-xC3[1:nNoise,-c(1,4)]+outlierNoise3
    #xC3[1:nNoise,1]<--3
    
    xC4<-cbind(rep(4,nC4),norm2dC4,dmvnorm(norm2dC4, muC4, sigmaMatC4, log=FALSE))
    xC5<-cbind(rep(5,nC5),norm2dC5,dmvnorm(norm2dC5, muC5, sigmaMatC5, log=FALSE))
    xC6<-cbind(rep(6,nC6),norm2dC6,dmvnorm(norm2dC6, muC6, sigmaMatC6, log=FALSE))
    
    X<-rbind(xC1,xC2,xC3,xC4,xC5,xC6)
    
    # Standardize!
    #X[,-1] <- scale(X[,-1],center = TRUE, scale = TRUE)
    #X[,1]<-factor(X[,1])
  }
  
  
  ## ------ 3 gaussian tops 1 Dir outliers  Not Used -------
  if(0){
    sigmaMatC1<-diag(1,2,2)
    sigmaMatC2<-diag(1,2,2)
    sigmaMatC3<-diag(1,2,2)
    nC1<-100
    nC2<-100
    nC3<-100
    dist<-4
    muC1<-c(0,0)
    muC2<-c(dist,0)
    muC3<-c(0,dist)
    norm2dC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
    norm2dC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
    norm2dC3<-MASS::mvrnorm(n = nC3, muC3, sigmaMatC3, empirical = FALSE)
    
    sigmaNoise<-10
    nNoise<-floor((nC1+nC2+nC3)*0.02); print(nNoise)
    sigmaNoiseMat<-diag(sigmaNoise,2,2); sigmaNoiseMat[2,2]<-0;
    outlierNoise<-abs(MASS::mvrnorm(n = nNoise, c(0,0), sigmaNoiseMat, empirical = FALSE))
    
    xC1<-cbind(rep(1,nC1),norm2dC1,dmvnorm(norm2dC1, muC1, sigmaMatC1, log=FALSE))
    
    xC2<-cbind(rep(2,nC2),norm2dC2,dmvnorm(norm2dC2, muC2, sigmaMatC2, log=FALSE))
    xC2[1:nNoise,-c(1,4)]<-xC2[1:nNoise,-c(1,4)]+outlierNoise
    xC2[1:nNoise,1]<--2
      
    xC3<-cbind(rep(3,nC3),norm2dC3,dmvnorm(norm2dC3, muC3, sigmaMatC3, log=FALSE))
    
    X<-rbind(xC1,xC2,xC3)
    
    # Standardize!
    #X[,-1] <- scale(X[,-1],center = TRUE, scale = TRUE)
    #X[,1]<-factor(X[,1])
  }
  
  ## ------ 4 gaussian tops 1 Dir outliers -------
  if(1){
    sigmaMatC1<-diag(1,2,2)
    sigmaMatC2<-diag(1,2,2)
    sigmaMatC3<-diag(1,2,2)
    sigmaMatC4<-diag(1,2,2)
    nC1<-100
    nC2<-100
    nC3<-100
    nC4<-100
    dist<-4
    muC1<-c(0,0)
    muC2<-c(dist,0)
    muC3<-c(0,dist)
    muC4<-c(dist,dist)
    norm2dC1<-MASS::mvrnorm(n = nC1, muC1, sigmaMatC1, empirical = FALSE)
    norm2dC2<-MASS::mvrnorm(n = nC2, muC2, sigmaMatC2, empirical = FALSE)
    norm2dC3<-MASS::mvrnorm(n = nC3, muC3, sigmaMatC3, empirical = FALSE)
    norm2dC4<-MASS::mvrnorm(n = nC4, muC4, sigmaMatC4, empirical = FALSE)
    
    sigmaNoise<-10
    nNoise<-floor((nC1+nC2+nC3)*0.05); print(nNoise)
    sigmaNoiseMat<-diag(sigmaNoise^2,2,2); sigmaNoiseMat[2,2]<-0;
    outlierNoise<-abs(MASS::mvrnorm(n = nNoise, c(0,0), sigmaNoiseMat, empirical = FALSE))
    
    xC1<-cbind(rep(1,nC1),norm2dC1,dmvnorm(norm2dC1, muC1, sigmaMatC1, log=FALSE))
    
    xC2<-cbind(rep(2,nC2),norm2dC2,dmvnorm(norm2dC2, muC2, sigmaMatC2, log=FALSE))
    xC2[1:nNoise,-c(1,4)]<-xC2[1:nNoise,-c(1,4)]+outlierNoise
    xC2[1:nNoise,1]<--2
    
    xC3<-cbind(rep(3,nC3),norm2dC3,dmvnorm(norm2dC3, muC3, sigmaMatC3, log=FALSE))
    xC4<-cbind(rep(4,nC3),norm2dC4,dmvnorm(norm2dC4, muC4, sigmaMatC4, log=FALSE))
    
    X<-rbind(xC1,xC2,xC3,xC4)
    
    # Standardize!
    #X[,-1] <- scale(X[,-1],center = TRUE, scale = TRUE)
    #X[,1]<-factor(X[,1])
  }

}



# ----------------------- T-SNE ------------------------- 
## ----tsne-simple-example, fig.width=4.5, fig.height=1.6, fig.align="center", echo=FALSE, results=FALSE, warning=FALSE, message=FALSE, cache=TRUE----
perp<-floor(dim(X)[1]*0.1); print(perp)
data_tsne <- dimRed::embed(X[,-1], "tSNE", perplexity = perp)@data@data
p1<-plot_ly(x=data_tsne[,1], y=data_tsne[,2], type="scatter", mode="markers",sizes = 10 ,color=X[,1])

#plot_ly(x=X[,2], y=X[,3], z=X[,4], type="scatter3d", mode="markers",size=10, color=X[,1])

p2<-plot_ly(x=X[,2], y=X[,3], type="scatter", mode="markers",sizes=10, color=X[,1])
subplot(p1,p2)

var(X[,2])
var(X[,3])

X[,-c(1,4)] <- scale(X[,-c(1,4)])

X[,-c(1)] <- scale(X[,-c(1)])






# ----- Kernel PCA ------

kPca <- kernlab::kpca(X[,-1], kernel = "rbfdot", kpar = list(sigma = 0.007),
                      features = 3, th = 1e-4, na.action = na.omit)

kPcaB<-kPca@pcv
kPcaR<-rotated(kPca)

p1<-plot_ly(x=kPcaR[,1], y=kPcaR[,2], type="scatter", mode="markers",sizes = 10 ,color=X[,1])
p2<-plot_ly(x=X[,2], y=X[,3], type="scatter", mode="markers",sizes=10, color=X[,1])
subplot(p1, p2)


var(X[,2])
var(X[,3])

X[,-1] <- scale(X[,-1])

plot_ly(x=kPcaR[,1], y=kPcaR[,2], z=kPcaR[,3], type="scatter3d", mode="markers",size=10, color=X[,1])



