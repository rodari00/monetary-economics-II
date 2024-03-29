#################################################
################# Libraries #####################
#################################################

library(foreign)
library(estimatr)
library(Matrix)
library(gtools)
rm(list=ls())
#################################################
################ Format data ####################
#################################################


fe <- 2 # flag for Airline specific fixed effect (=1 is within deviation, =2 is using dummies)
binary <- 0 # flag if =1, Market Presence is discretized

prior <- read.dta("C:/Users/feder/Dropbox/Github/industrial-organization-I/ps2/ReplicationCilibertoTamer/CilibertoTamerEconometrica.dta") # get data from CT
prior$A1 <- sapply(prior$market,function(x) substring(x,1,3)) # origine airport
prior$A2 <- sapply(prior$market,function(x) substring(x,4,6)) # destination airport
m <- nrow(prior) # number of markets
kx <- 10 # number of explanatory variables
v2k <- unique(permutations(n=6,r=6,v=c(1,0,0,0,0,0),set=F,repeats.allowed = T)) # list all possible matket combinations
l2k <- nrow(v2k) # number of possible market combinations
X <- vector("list", m) # each element will be a matrix
Y <- vector("list", m) # each element will be a vector
G <- vector("list", m) # each element will ba a n x n adjacency matrix
n <- rep(6,m) # number of firms for each market is 6

for (i in 1:m){ # for each market
  nt <- n[i] # size of market i
  Gt <- matrix(1,nt,nt) # initialize G
  Yt <- matrix(0,nt,1) # initialize Y
  Xt <- matrix(0,nt,kx) # initialize X
  Yt[,1] <- as.numeric(prior[i,c(which(colnames(prior)=="airlineAA"):which(colnames(prior)=="airlineWN"))]) # y for market i
  if (binary==1){
    Xt[,1] <- as.numeric(as.numeric(prior[i,c(which(colnames(prior)=="marketpresenceAA"):which(colnames(prior)=="marketpresenceWN"))])>=0.31) # market presence for market i
  } else {
    Xt[,1] <- as.numeric(prior[i,c(which(colnames(prior)=="marketpresenceAA"):which(colnames(prior)=="marketpresenceWN"))]) # market presence for market i
  }
  
  Xt[,2] <- as.numeric(prior[i,c(which(colnames(prior)=="mindistancefromhubAA"):which(colnames(prior)=="mindistancefromhubWN"))]) # distance from hub for market i
  # market variables for market i
  Xt[,3] <- prior[i,"wrightamendmDAL"]
  Xt[,4] <- prior[i,"dallasmarket"]
  Xt[,5] <- prior[i,"marketsize"]
  Xt[,6] <- prior[i,"marketdistance"]
  Xt[,7] <- prior[i,"mindistance"]
  Xt[,8] <- prior[i,"fromcenterdistance"]
  Xt[,9] <- prior[i,"percapitaincmarket"]
  Xt[,10] <- prior[i,"changeincmarket"]
  
  diag(Gt) <- 0 # zero on diagonal of G
  X[[i]] <- Xt # store X
  Y[[i]] <- Yt # store Y
  G[[i]] <- Gt # store G
}

mY <- matrix(0,6,1) # initialize average y (across markets)
mX <- matrix(0,6,kx)  # initialize average X (across markets)
mGY <- matrix(0,6,1)  # initialize average Gy (across markets)
mGX <- matrix(0,6,kx) # initialize average GX (across markets)

# compute averages for y,X,Gy,GX (across markets)
for (i in 1:m){
  mY <- mY + Y[[i]]/m
  mX <- mX + X[[i]]/m
  mGY <- mGY + G[[i]]%*%Y[[i]]/m
  mGX <- mGX + G[[i]]%*%X[[i]]/m
}


#################################################
################## Functions ####################
#################################################



bdf <- function(){
  ## generates a dataset for 2SLS estimation
  
  siz <- sum(n) # total number of firms
  bX <- matrix(1,siz,(kx+1)) # regressors are X + gy
  bZ <- matrix(1,siz,kx) # instruments are GX (excluding constant)
  bY <- matrix(1,siz,1) # explained var is Y
  clust <- matrix(1,siz,1) # market number
  for (school in 1:m){
    ## position of the first firm in market "school"
    if (school==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(school-1)])+1
    }
    p2 <- p1+n[school]-1 # position of the last firm in market i
    Yt <- Y[[school]] # get y
    Xt <- X[[school]] # get X
    Gt <- G[[school]] # get G
    nt <- n[i] # get market size=6
    GYt <- Gt%*%Yt # computes Gy
    GXt <- Gt%*%Xt # computes GX
    if (fe==1){ # if fe=1, substract averages
      Yt <- Yt - mY
      Xt <- Xt - mX
      GYt <- GYt - mGY
      GXt <- GXt - mGX
    }
    bX[p1:p2,] <- cbind(Xt,GYt) # binds explanatory vars
    bZ[p1:p2,] <- GXt # instruments
    bY[p1:p2,1] <- Yt # y
    clust[p1:p2,1] <- school # market number
  }
  return(as.data.frame(cbind(bY,bX,bZ,clust)))
}

predfit <- function(probfit){
  ## computes number of correctly predicted markets
  obj <- 0 # initialize
  for (i in 1:m){
    ## position of the first firm in market i
    if (i==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(i-1)])+1
    }
    nt <- n[i] # size of market i=6
    p2 <- p1 + nt -1 # position of the last firm in market i
    Pi <- pmin(pmax(probfit[p1:p2],0),1) # predicted probability for y=1
    fit <- rep(0,l2k) # initialize probas
    for (j in 1:l2k){ # for each possible market structure
      yt <- c(v2k[j,]) # get the market structure
      fit[j] <- prod( (Pi^yt)*(1-Pi)^(1-yt)  ) # predicted probability
    }
    j <- which.max(fit) # market structure with the highest predicted probability
    fit <- all(v2k[j,]==c(Y[[i]])) # is observation = predicted market structure
    obj <- obj + sum(as.numeric(fit))
  }
  return(obj)
}


predictprob <- function(para){
  # computes the predicted probabilities
  siz <- sum(n) # total number of individuals
  grad <- matrix(0,siz,1) # initialize
  mpara <- matrix(para[1:(length(para)-1)],(length(para)-1),1) # parameter values as matrix, parameters on exogenous variables, including constant at 1st position
  
  for (i in 1:m){ # for each group i
    nt <- n[i] # size of group i
    #position of the first individual in group i
    if (i==1){
      p1 <-1
    } else{
      p1 <- sum(n[1:(i-1)])+1
    }
    p2 <- p1+n[i]-1 # position of the last individual in group i
    Minv <- solve(diag(nt)-para[length(para)]*G[[i]]) # computes the inverse (I-betaG)^(-1)
    if (fe==0){
      if (!costonly){
        bZ <- Minv%*%cbind(matrix(1,nt,1),X[[i]]) # computes the probability of y=1
      }
      if (costonly){
        bZ <- Minv%*%cbind(matrix(1,nt,1),X[[i]][,-1]) # computes the probability of y=1
      }
    }
    if (fe==2){
      if (!costonly){
        bZ <- cbind(X[[i]],diag(nt))
      }
      if (costonly){
        bZ <- cbind(X[[i]][,-1],diag(nt))
      }
      bZ <- Minv%*%bZ # computes the probability of y=1
    }
    
    grad[p1:p2,1] <- bZ%*%mpara # computes predicted proba
  }
  return(c(grad))
}


#################################################
################# Execute code ##################
#################################################

dta <- bdf() # construct database
# name variables
cname <- c("MarketPres","DistHub","Wright","Dallas","Msize","Mdist","mindist","centerdist","percapinc","changeinc")
cname <- c("Y",cname,"GY",as.vector(sapply(cname,function(t) paste("z",t,sep=''))),"school")
colnames(dta) <- cname
dta$airline <- factor(rep(1:6,m))

## create formula
#instr <- cname[(kx+4)]
if (fe==1){
  out <- iv_robust(Y ~ 0 + MarketPres + DistHub + Wright + Dallas + Msize + Mdist + 
                     mindist + centerdist + percapinc + changeinc + GY | 0 + MarketPres +
                     DistHub + Wright + Dallas + Msize 	+ Mdist + mindist + centerdist + 
                     percapinc + changeinc + zMarketPres + zDistHub, data=dta,clusters = school,diagnostics = T)
} else if (fe==2) {
  out <- iv_robust(Y ~ 0 + MarketPres + DistHub + Wright + Dallas + Msize + Mdist + 
                     mindist + centerdist + percapinc + changeinc + airline + GY | 0 + MarketPres +
                     DistHub + Wright + Dallas + Msize 	+ Mdist + mindist + centerdist + 
                     percapinc + changeinc + airline + zMarketPres + zDistHub, data=dta,clusters = school,diagnostics = T)
} else {
  out <- iv_robust(Y ~ MarketPres + DistHub + Wright + Dallas + Msize + Mdist + 
                     mindist + centerdist + percapinc + changeinc + GY | MarketPres +
                     DistHub + Wright + Dallas + Msize 	+ Mdist + mindist + centerdist + 
                     percapinc + changeinc + zMarketPres + zDistHub, data=dta,clusters = school,diagnostics = T)
}

print(summary(out))

costonly <- FALSE

if (fe !=1 ){
  probfit <- predictprob(out$coefficients)
  
  pred <- predfit(probfit)/m # fraction of correctly predicted market structures
  print(pred)
  print(mean(probfit>=0 & probfit<=1))
}


if (fe==1){
  out <- iv_robust(Y ~ 0 + DistHub + Wright + Dallas + Msize + Mdist + 
                     mindist + centerdist + percapinc + changeinc + GY | 0 + 
                     DistHub + Wright + Dallas + Msize 	+ Mdist + mindist + centerdist + 
                     percapinc + changeinc + zDistHub, data=dta,clusters = school,diagnostics = T)
} else if (fe==2) {
  out <- iv_robust(Y ~ 0 + DistHub + Wright + Dallas + Msize + Mdist + 
                     mindist + centerdist + percapinc + changeinc + airline + GY | 0 + 
                     DistHub + Wright + Dallas + Msize 	+ Mdist + mindist + centerdist + 
                     percapinc + changeinc + airline + zDistHub, data=dta,clusters = school,diagnostics = T)
} else {
  out <- iv_robust(Y ~ DistHub + Wright + Dallas + Msize + Mdist + 
                     mindist + centerdist + percapinc + changeinc + GY | 
                     DistHub + Wright + Dallas + Msize 	+ Mdist + mindist + centerdist + 
                     percapinc + changeinc + zDistHub, data=dta,clusters = school,diagnostics = T)
}

print(summary(out))

costonly <- TRUE

if (fe !=1 ){
  probfit <- predictprob(out$coefficients)
  
  pred <- predfit(probfit)/m # fraction of correctly predicted market structures
  print(pred)
  print(mean(probfit>=0 & probfit<=1))
}