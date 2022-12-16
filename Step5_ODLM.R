## Hort Farm Ecosystem Resilience Project ###
# Code originally written by TJ Butts & GM Wilkinson November 2022

#============================================#
# STEP 5: Online Dynamic Linear Modeling 
#============================================#

# Functions for Online Estimation of AR(p) model using Dynamic Linear Model
# Copyright 2017 by Stephen R. Carpenter

# Try time-varying AR on Cascade data via online DLM method
# SRC 2017-07-01
# (c) Stephen R. Carpenter 2017-07-01

graphics.off()

#============================================# 
 # Chlorophyll - a Dynamic Linear Modeling #==========
#============================================#


## ============ Plot Margins ================= ##
# Window for checking plot 
windows(height = 4, width = 6) 

# Will create plot in whatever file path you set  
#pdf(file = "C:/Users/tjbut/Box Sync/Butts_Dissertation/Hort Chapter/Figures/Hort_Figure5.pdf", 
 #   height = 4, 
  #  width = 6)

# Set dimensions for figure array # 
par(mfrow =c(2,3),  mar = c(0.5,1,1,0.5), oma = c(4,4,.5,.5))
par(tcl = -0.25)
par(mgp = c(2, 0.6, 0)) 

# ========= PLOTTING COLORS ======== # 
low_col_B = rgb(74, 166, 81, max = 255, alpha = 180) #Pond B, Pond F
low_col_F = rgb(74, 166, 81, max = 255, alpha = 100) #Pond B, Pond F
low_col = rgb(74, 166, 81, max = 255, alpha = 255) #Pond B, Pond F
ref_col = rgb(155, 155, 155, max=255, alpha = 100) # Reference
black_col = rgb(0,0,0, max=255, alpha = 100) # Black
transparent = rgb(255,255,255, max=255, alpha = 0)

int_col_A = rgb(44, 127, 184, max = 255, alpha = 180) #Pond A, pond D
int_col_D = rgb(44, 127, 184, max = 255, alpha = 100) #Pond A, pond D
int_col = rgb(44, 127, 184, max = 255, alpha = 255) #Pond A, pond D

high_col_C = rgb(8, 29, 88, max = 255, alpha = 180) #Pond C, Pond E
high_col_E = rgb(8, 29, 88, max = 255, alpha = 100) #Pond C, Pond E
high_col = rgb(8, 29, 88, max = 255, alpha = 255) #Pond C, Pond E

# This script combines the shell function ODLMAR(nl,delta,x.full,T.full,title) with the 
#  online estimation function DLM(delta,n.gamma,d.gamma,mvec,Cpar,Yvec,Fmat)

## Run Functions ##============================
# Functions ----------------------------------------------------------------------------------

# ONLINE DYNAMIC LINEAR MODEL (DLM) ESTIMATION

DLM <- function(delta,n.gamma,d.gamma,mvec,Cpar,Yvec,Fmat) {
  
  # Online algorithm for Dynamic linear regression
  # Copyright 2016 by Stephen R. Carpenter
  
  # Description and definitions:
  
  # Observation equation is
  # Y_t = F_t'*theta_t + eta_t where
  # Y_t is the prediction
  # F_t is a vector of predictors at the beginning of the time step
  # theta_t is the parameter vector
  # eta_t is an individual observation error
  
  # System equation is:
  # theta_t = theta_t-1 + omega_t
  # where theta is defined above and omega_t is an individual process error
  
  # Inputs to the function are:
  # delta, the discount factor
  # n.gamma, the initial number of observations (usually 1)
  # d.gamma, the initial shape parameter for prediction errors
  #  (prior estimate of prediction variance = d.gamma / n.gamma)
  # mvec, the initial guess of regression coefficients
  # Cpar, the initial guess of the covariance matrix of regression coefficients
  # Yvec, the vector of the observed response variate
  # Fmat, the matrix of predictors
  
  # Outputs are:
  # predix, the one-step-ahead predictions of the response variate
  # varpredix, the prediction variance at start of time step before error is measured
  # pars, the updated parameter estimates using the most recent prediction error
  # parvar, the variances of the parameters
  # Svec, the update (after error is measured within a time step) of varpredix
  
  # Updating follows the equations on p. 176-179 of Carpenter 2003,
  # Regime Shifts in Lake Ecosystems: Pattern and Variation
  
  # Determine constants
  npar <- length(mvec)
  Nobs <- length(Yvec)
  S0 <- d.gamma/n.gamma
  
  # Set up vectors to hold results
  predix <- rep(0,Nobs)
  varpredix <- rep(0,Nobs)
  Svec = rep(0,Nobs)
  pars <- matrix(0,nrow=Nobs,ncol=npar)
  parvar = matrix(0,nrow=Nobs,ncol=npar)
  
  for(i in 1:Nobs)  {  #Start DLM loop
    # Generate predictions
    Fvec <- Fmat[i,] # vector of predictors
    predix[i] <- sum(Fvec*mvec)
    # Compute error and update estimates
    error <- Yvec[i]-predix[i]
    Rmat <- Cpar/delta
    varpredix[i] <- (t(Fvec) %*% Rmat %*% Fvec) + S0
    n.gamma <- (delta*n.gamma)+1
    d.gamma <- (delta*d.gamma)+(S0*error*error/varpredix[i])
    S1 <- d.gamma/n.gamma
    Svec[i] = S1  # save updated variance
    Avec <- (Rmat %*% Fvec)/varpredix[i]
    mvec <- mvec + (Avec*error)
    pars[i,] <- mvec
    Cpar <- (S1/S0)*(Rmat - (Avec %*% t(Avec))*varpredix[i])
    # Disallow negative variances on the diagonal
    for(idiag in 1:npar) {
      Cpar[idiag,idiag] <- max(0,Cpar[idiag,idiag])
    }
    parvar[i,] = diag(Cpar)
    S0 <- S1 # roll over S
  } # End DLM loop
  
  DLM.out <- list(predix,varpredix,pars,parvar,Svec)
  return(DLM.out)
} # END DLM FUNCTION

# ONLINE SHELL -----------------------------------------------------------------------

ODLMAR = function(nl,delta,x.full,T.full,title) {
  # Compute online DLM for AR models
  
  # Copyright 2016 by Stephen R. Carpenter
  
  # This function is a shell for the DLM() function
  
  # Inputs are:
  #  nl = number of lags in AR model
  #  delta = discount factor 0<delta<1; reasonable values are 0.9 to 0.99
  #     Rule of Thumb: df for each point estimate = 1/(1-delta)
  #  x.full is the time series to be analyzed
  #  T.full is the corresponding time steps
  #  title is a title for the plots
  
  # Outputs are a list containing:
  #  1 = matrix containing: time step, Y, yhat (one-step prediction), updated prediction variance
  #     Dimension is (nobs-nl)x4 where nobs is number of observations and nl is number of lags
  #  2 = (nobs-nl)x4 matrix containing eigenvalue, sd of eigenvalue, eigenvalue + sd, eigenvalue - sd
  #  3 = (nl+1)x(nobs-nl) matrix of AR parameter estimates; col 1 is intercept, col 2 is AR(1) coef, etc.
  #  4 = (nl+1)x(nobs-nl) matrix of AR parameter standard deviations
  
  # choose AR order
  p = nl+1 # allow for intercept
  
  # Number of observations
  nobs = length(x.full)
  
  # AR variates
  X.design = matrix(1,nr=(nl+1),nc=(nobs-nl)) # matrix to hold predictors
  for(i in 1:nl) {
    X.design[(i+1),] = x.full[i:(nobs-nl+i-1)]
  }
  Y = matrix(x.full[(1+nl):nobs],nr=1,nc=(nobs-nl)) # response
  
  # LS regression for initials
  invXX = solve(X.design%*%t(X.design))
  lm.par = invXX%*%X.design%*%t(Y)
  # Force initial parameters inside unit circle
  lm.inits=lm.par
  lm.inits[2:p] = ifelse(lm.par[2:p]^2 < 1,lm.par[2:p],0.9)
  # Other useful regression statistics
  lm.yhat = t(X.design)%*%lm.par
  lm.err = t(Y)-lm.yhat
  verr = var(lm.err)
  covpar = invXX*verr[1,1]
  
  # Other parameters for DLM
  n.gam = 1  # initial df
  d.gam = verr[1,1]  # based on the gamma distribution mean var = df*scale
  
  # Run DLM
  DLMrun = DLM(delta,n.gam,d.gam,lm.par,covpar,Y,t(X.design))
  yhat = DLMrun[[1]]
  vyhat = DLMrun[[2]]
  B.ests = t(DLMrun[[3]])
  B.sd = t( sqrt(DLMrun[[4]]) ) # parameter SD
  vupdate = DLMrun[[5]] #updated variance
  
  # Compute adjusted R^2
  Ry = cor(t(Y),yhat)
  R2 = Ry^2
  R2adj = 1 - ( (1-R2[1,1])*(nobs-1)/(nobs-p-1) )
  print('',quote=F)
  print(c('DLM R^2 = ',round(R2,3),' DLM adj R^2 = ',round(R2adj,3)),quote=F)
  # Compute AIC
  err1 = yhat-Y # errors
  sd1 = sqrt(vupdate)  # error variance estimate
  LL = dnorm(err1, mean=0, sd=sd1, log=T) # log likelihoods
  aic = 2*p - 2*sum(LL)
  print(c('AIC = ',round(aic,2)),quote=F)
  
  # compute eigenvalues
  lamda = rep(0,(nobs-nl))
  for(i in 1:(nobs-nl)) {
    armat = matrix(0,nr=nl,nc=nl)
    subdiag = rep(1,(nl-1))
    armat[row(armat) == col(armat)+1] = subdiag
    armat[1,] = B.ests[2:p,i]
    eigvals = eigen(armat,only.values=T)
    lamda[i] = max(Mod(eigvals$values))
  }
  
  # Bootstrap eigenvalue errors
  NT = nobs-nl  # number of time steps
  nboot = 100 # number of bootstrap iterations
  lamda.sd = rep(0,NT) # vector to hold time series of eigenvalue s.e.
  lamda.temp = rep(0,nboot)  # vector to hold temporary sample of eigenvalues
  # make a matrix with subdiagonal 1
  armat = matrix(0,nr=nl,nc=nl)
  subdiag = rep(1,(nl-1))
  armat[row(armat) == col(armat)+1] = subdiag
  for(i in 1:NT) {  # loop over time steps
    # make a matrix of normal random numbers with nboot rows and nl columns, each column a sample for an AR coef
    ARnorm = mapply(function(mu,sigma){rnorm(n=nboot,mean=mu,sd=sigma)},mu=B.ests[2:p,i],sigma=B.sd[2:p,i])
    for(j in 1:nboot)  {  # loop over bootstrap iterations
      armat[1,] = ARnorm[j,]
      eigvals = eigen(armat,only.values=T)
      lamda.temp[j] = max(Mod(eigvals$values))
    }  # end loop over bootstrap iterations
    lamda.sd[i] = sd(lamda.temp)
  }  # end loop over timesteps
  
  # Plot results
  T.ar = T.full[1:(nobs-nl)]
  
  # Plot 3: eigenvalue plus error
  lamda.plus = lamda+lamda.sd
  lamda.minus = lamda-lamda.sd
  yrange = range(lamda.plus,lamda.minus,0,1)
  plot(T.ar,lamda,type='l',lwd=2,col=color,ylim=c(0, 1.2),
       xlab='',ylab='', cex=1, cex.axis=1.2, xlim=c(165,245), col.axis = transparent, 
       yaxt = 'n')
  polygon(c(T.ar, rev(T.ar)), c(lamda.plus, rev(lamda.minus)), col=color1, border=NA)
  points(T.ar,lamda,type='l',lwd=3,col=color)
  # points(T.ar,lamda.plus,type='l',lwd=2,lty=2,col=color1)
  # points(T.ar,lamda.minus,type='l',lwd=2,lty=2,col=color)
  abline(h=1,lty=2)
  lines(c(176,176), c(-10,100), lwd=2, lty=3, col="gray20") #Pulse1
  lines(c(211,211), c(-10,100), lwd=2, lty=3, col="gray20") #Pulse2
  box()
  #lines(c(223,223), c(-10,100), lwd=2, lty=2, col='gray20')
  #col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
  #rect(185,-2,190,15, col=col, border=NA)
  
  # Create output list and return it
  Yyhat = matrix(c(T.ar,Y,yhat,vupdate),nr=(nobs-nl),nc=4)
  LamdaMat = matrix(c(lamda,lamda.sd,lamda.plus,lamda.minus),nr=(nobs-nl),nc=4)
  
  outlist = list(Yyhat,LamdaMat,B.ests,B.sd)
  return(outlist)
  
}  # End online DLM function

# end functions ----------------------------------------------------------------------------------------




# plotting DLMs ## ==============================

# Data # 
hort_sonde # Daily profile data 

# Separate to just chlorophyll-a measurements 
chl = hort_sonde %>%
  select(pond_id, doy, chla)
chl

# Chlorophyll-a by pond (easier to apply GAMs if separated) # 
algA = chl %>% #pulse, int
  filter(pond_id == "A") 

algB = chl %>% #pulse, low
  filter(pond_id == "B") 

algC = chl %>% #pulse, high
  filter(pond_id == "C") 

algD = chl %>% #ref, int
  filter(pond_id == "D") 

algE = chl %>% #ref, high
  filter(pond_id == "E") 

algF = chl %>% #ref, low
  filter(pond_id == "F") 

# Disturbed Ponds #===================== 

# Pulsed Low 
x.full = as.vector(algB$chla) 
T.full = as.vector(algB$doy)

#title = c('Low Coupling AR(2)', line = 1) #Title for the plot
color= low_col # this is the color for the eigenvalue line
color1=low_col_F #this is the color for the error polygon 
nobs = length(x.full)

# START PROTOTYPE SHELL
# USER MUST INPUT: nl; delta; x.full; T.full; title

nl = 2 # number of lags, AIC is lower on lag 2, but lower on lag 1 for other ponds 
delta = 0.9 # 0<delta<1; see advice in functions

ODL.out = ODLMAR(nl,delta,x.full,T.full,title)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2))
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 'Eigenvalues, Pulsed', cex = 11/12)
text(167, 1.2, 'A', font = 2)
mtext(side = 3, line = 0.1, 'Low Coupling', cex = 11/12)

Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]
B.sd = ODL.out[[4]]

#mtext('Low Coupling', side = 3, line = 1, cex = 1.25)

# Pulsed Intermediate 
x.full = as.vector(algA$chla) 
T.full = as.vector(algA$doy)

#title = c('Intermediate AR(1)', line = 1) #Title for the plot
color= int_col # this is the color for the eigenvalue line
color1=int_col_D #this is the color for the error polygon 
nobs = length(x.full)

# START PROTOTYPE SHELL
# USER MUST INPUT: nl; delta; x.full; T.full; title

nl = 1 # number of lags, AIC is lower on lag 2, but lower on lag 1 for other ponds 
delta = 0.9 # 0<delta<1; see advice in functions

ODL.out = ODLMAR(nl,delta,x.full,T.full,title)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2), labels = FALSE)
mtext(side = 3, line = 0.1, 'Intermediate', cex = 11/12)
text(167, 1.2, 'B', font = 2)
Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]
B.sd = ODL.out[[4]]

#mtext('Intermediate', side = 3, line = 1, cex = 1.25)

# Pulsed High 
x.full = as.vector(algC$chla) 
T.full = as.vector(algC$doy)

#title = c('High Coupling AR(1)', line = 1) #Title for the plot
color= high_col # this is the color for the eigenvalue line
color1= high_col_E #this is the color for the error polygon 
nobs = length(x.full)

# START PROTOTYPE SHELL
# USER MUST INPUT: nl; delta; x.full; T.full; title

nl = 1 # number of lags, AIC is lower on lag 2, but lower on lag 1 for other ponds 
delta = 0.9 # 0<delta<1; see advice in functions

ODL.out = ODLMAR(nl,delta,x.full,T.full,title)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2), labels = FALSE)
mtext(side = 3, line = 0.1, 'High Coupling', cex = 11/12)
text(167, 1.2, 'C', font = 2)

Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]
B.sd = ODL.out[[4]]

#mtext('High Coupling', side = 3, line = 1, cex = 1.25)

 ## Reference Ponds ##==================================
## Rerun shell to adjust the bottom half of the plot ## 

ODLMAR = function(nl,delta,x.full,T.full,title) {
  # Compute online DLM for AR models
  
  # Copyright 2016 by Stephen R. Carpenter
  
  # This function is a shell for the DLM() function
  
  # Inputs are:
  #  nl = number of lags in AR model
  #  delta = discount factor 0<delta<1; reasonable values are 0.9 to 0.99
  #     Rule of Thumb: df for each point estimate = 1/(1-delta)
  #  x.full is the time series to be analyzed
  #  T.full is the corresponding time steps
  #  title is a title for the plots
  
  # Outputs are a list containing:
  #  1 = matrix containing: time step, Y, yhat (one-step prediction), updated prediction variance
  #     Dimension is (nobs-nl)x4 where nobs is number of observations and nl is number of lags
  #  2 = (nobs-nl)x4 matrix containing eigenvalue, sd of eigenvalue, eigenvalue + sd, eigenvalue - sd
  #  3 = (nl+1)x(nobs-nl) matrix of AR parameter estimates; col 1 is intercept, col 2 is AR(1) coef, etc.
  #  4 = (nl+1)x(nobs-nl) matrix of AR parameter standard deviations
  
  # choose AR order
  p = nl+1 # allow for intercept
  
  # Number of observations
  nobs = length(x.full)
  
  # AR variates
  X.design = matrix(1,nr=(nl+1),nc=(nobs-nl)) # matrix to hold predictors
  for(i in 1:nl) {
    X.design[(i+1),] = x.full[i:(nobs-nl+i-1)]
  }
  Y = matrix(x.full[(1+nl):nobs],nr=1,nc=(nobs-nl)) # response
  
  # LS regression for initials
  invXX = solve(X.design%*%t(X.design))
  lm.par = invXX%*%X.design%*%t(Y)
  # Force initial parameters inside unit circle
  lm.inits=lm.par
  lm.inits[2:p] = ifelse(lm.par[2:p]^2 < 1,lm.par[2:p],0.9)
  # Other useful regression statistics
  lm.yhat = t(X.design)%*%lm.par
  lm.err = t(Y)-lm.yhat
  verr = var(lm.err)
  covpar = invXX*verr[1,1]
  
  # Other parameters for DLM
  n.gam = 1  # initial df
  d.gam = verr[1,1]  # based on the gamma distribution mean var = df*scale
  
  # Run DLM
  DLMrun = DLM(delta,n.gam,d.gam,lm.par,covpar,Y,t(X.design))
  yhat = DLMrun[[1]]
  vyhat = DLMrun[[2]]
  B.ests = t(DLMrun[[3]])
  B.sd = t( sqrt(DLMrun[[4]]) ) # parameter SD
  vupdate = DLMrun[[5]] #updated variance
  
  # Compute adjusted R^2
  Ry = cor(t(Y),yhat)
  R2 = Ry^2
  R2adj = 1 - ( (1-R2[1,1])*(nobs-1)/(nobs-p-1) )
  print('',quote=F)
  print(c('DLM R^2 = ',round(R2,3),' DLM adj R^2 = ',round(R2adj,3)),quote=F)
  # Compute AIC
  err1 = yhat-Y # errors
  sd1 = sqrt(vupdate)  # error variance estimate
  LL = dnorm(err1, mean=0, sd=sd1, log=T) # log likelihoods
  aic = 2*p - 2*sum(LL)
  print(c('AIC = ',round(aic,2)),quote=F)
  
  # compute eigenvalues
  lamda = rep(0,(nobs-nl))
  for(i in 1:(nobs-nl)) {
    armat = matrix(0,nr=nl,nc=nl)
    subdiag = rep(1,(nl-1))
    armat[row(armat) == col(armat)+1] = subdiag
    armat[1,] = B.ests[2:p,i]
    eigvals = eigen(armat,only.values=T)
    lamda[i] = max(Mod(eigvals$values))
  }
  
  # Bootstrap eigenvalue errors
  NT = nobs-nl  # number of time steps
  nboot = 100 # number of bootstrap iterations
  lamda.sd = rep(0,NT) # vector to hold time series of eigenvalue s.e.
  lamda.temp = rep(0,nboot)  # vector to hold temporary sample of eigenvalues
  # make a matrix with subdiagonal 1
  armat = matrix(0,nr=nl,nc=nl)
  subdiag = rep(1,(nl-1))
  armat[row(armat) == col(armat)+1] = subdiag
  for(i in 1:NT) {  # loop over time steps
    # make a matrix of normal random numbers with nboot rows and nl columns, each column a sample for an AR coef
    ARnorm = mapply(function(mu,sigma){rnorm(n=nboot,mean=mu,sd=sigma)},mu=B.ests[2:p,i],sigma=B.sd[2:p,i])
    for(j in 1:nboot)  {  # loop over bootstrap iterations
      armat[1,] = ARnorm[j,]
      eigvals = eigen(armat,only.values=T)
      lamda.temp[j] = max(Mod(eigvals$values))
    }  # end loop over bootstrap iterations
    lamda.sd[i] = sd(lamda.temp)
  }  # end loop over timesteps
  
  # Plot results
  T.ar = T.full[1:(nobs-nl)]
  
  # Plot 3: eigenvalue plus error
  lamda.plus = lamda+lamda.sd
  lamda.minus = lamda-lamda.sd
  yrange = range(lamda.plus,lamda.minus,0,1)
  plot(T.ar,lamda,type='l',lwd=2,col=color,ylim=c(0, 1.2),
       xlab='',ylab='', cex=1, cex.axis=1.2, xlim=c(165,245), 
       yaxt = 'n')
  polygon(c(T.ar, rev(T.ar)), c(lamda.plus, rev(lamda.minus)), col=color1, border=NA)
  points(T.ar,lamda,type='l',lwd=3,col=color)
  # points(T.ar,lamda.plus,type='l',lwd=2,lty=2,col=color1)
  # points(T.ar,lamda.minus,type='l',lwd=2,lty=2,col=color)
  abline(h=1,lty=2)
  lines(c(176,176), c(-10,100), lwd=2, lty=3, col="gray20") #Pulse1
  lines(c(211,211), c(-10,100), lwd=2, lty=3, col="gray20") #Pulse2
  box()
  #lines(c(223,223), c(-10,100), lwd=2, lty=2, col='gray20')
  #col=rgb(255,48,48, max=255, alpha=125, names= 'firebrick1')
  #rect(185,-2,190,15, col=col, border=NA)
  
  # Create output list and return it
  Yyhat = matrix(c(T.ar,Y,yhat,vupdate),nr=(nobs-nl),nc=4)
  LamdaMat = matrix(c(lamda,lamda.sd,lamda.plus,lamda.minus),nr=(nobs-nl),nc=4)
  
  outlist = list(Yyhat,LamdaMat,B.ests,B.sd)
  return(outlist)
  
}  # End online DLM function

# Reference Low 
x.full = as.vector(algF$chla) 
T.full = as.vector(algF$doy)

#title = c('Low Coupling AR(1)', line = 1) #Title for the plot
color= 'black' # this is the color for the eigenvalue line
color1= 'gray88' #this is the color for the error polygon 
nobs = length(x.full)

# START PROTOTYPE SHELL
# USER MUST INPUT: nl; delta; x.full; T.full; title

nl = 1 # number of lags, AIC is lower on lag 2, but lower on lag 1 for other ponds 
delta = 0.9 # 0<delta<1; see advice in functions

ODL.out = ODLMAR(nl,delta,x.full,T.full,title)
mtext(side = 2, line = 3.2, 
      expression('Chlorophyll-'~italic(a)), cex = 11/12)
mtext(side = 2, line = 2, 'Eigenvalues, Reference', cex = 11/12)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2))
text(167, 1.2, 'D', font = 2)


Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]
B.sd = ODL.out[[4]]

# Pulsed Intermediate 
x.full = as.vector(algD$chla) 
T.full = as.vector(algD$doy)

#title = c('Intermediate AR(1)', line = 1) #Title for the plot
color= 'black' # this is the color for the eigenvalue line
color1= 'gray88' #this is the color for the error polygon 
nobs = length(x.full)

# START PROTOTYPE SHELL
# USER MUST INPUT: nl; delta; x.full; T.full; title

nl = 1 # number of lags, AIC is lower on lag 2, but lower on lag 1 for other ponds 
delta = 0.9 # 0<delta<1; see advice in functions

ODL.out = ODLMAR(nl,delta,x.full,T.full,title)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2), labels = F)
text(167, 1.2, 'E', font = 2)

Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]
B.sd = ODL.out[[4]]

mtext('Day of Year, 2020', side = 1, line = 2, cex = 11/12)

# Pulsed High 
x.full = as.vector(algE$chla) 
T.full = as.vector(algE$doy)

#title = c('High Coupling AR(1)', line = 1) #Title for the plot
color= 'black' # this is the color for the eigenvalue line
color1='gray88' #this is the color for the error polygon 
nobs = length(x.full)

# START PROTOTYPE SHELL
# USER MUST INPUT: nl; delta; x.full; T.full; title

nl = 1 # number of lags, AIC is lower on lag 2, but lower on lag 1 for other ponds 
delta = 0.9 # 0<delta<1; see advice in functions

ODL.out = ODLMAR(nl,delta,x.full,T.full,title)
text(167, 1.2, 'F', font = 2)
axis(side = 2, at = c(0, 0.2, 0.4, 0.6, 0.8, 1, 1.2), labels = F)


Yyhat = ODL.out[[1]]
EigenVals = ODL.out[[2]]
B.ests = ODL.out[[3]]
B.sd = ODL.out[[4]]

#mtext('High Coupling', side = 3, line = 1, cex = 1.25)

# Print plot in specified file # 


#dev.off()

