
## The code is all about Modeling and then forecasting (by simulation) 
## from just the previous year data 
library(MASS)




data=read.csv(file.choose(),header=T) #Load the previous year premier league 
data
attach(data)
# Target / Response variable-----------------------------------------------------
y=c(FTHG,FTAG)  #goals scored by a particular team per game

# Predictive/explanatory variables-----------------------------------------------
x11=c(HS,rep(0,380))    #total no of shots by a particular team per game
x12=c(rep(0,380),AS)
x21=c(HST,rep(0,380))   #total no of shots on target by a particular team per game
x22=c(rep(0,380),AST)
x31=c(HF,rep(0,380))    #Fouls Committed by a particular team per game
x32=c(rep(0,380),AF)
x41=c(HC,rep(0,380))    #Corners taken by a particular team per game
x42=c(rep(0,380),AC)
x51=c(HY,rep(0,380))    #getting yellow cards
x52=c(rep(0,380),AY)
x61=c(HR,rep(0,380))    #getting red cards
x62=c(rep(0,380),AR)


# Modeling/fiting Poisson regression---------------------------------------------- 

fit=glm(y~x11+x12+x21+x22+x31+x32+
      x41+x42+x51+x52+x61+x62,family=poisson,data=data)
## Seeing the results of the model--------------------
summary(fit)
names(fit)
library(pscl)
pR2(fit)     #pseudo r square 0.3161629,0.3324922 
summary(stepAIC(fit)) # To see the estimated coefficients in the model

# Modeling/fiting Quasi Poisson regression-----------------------------------------
fit1=glm(y~x11+x12+x21+x22+x31+x32+x41+x42+x51+x52+x61+x62,
        family=quasipoisson(link = "log"), data=data)
summary(fit1)
names(fit1)
# Modeling/fiting negative binomial regression-------------------------------------
fit2=glm.nb(y~x11+x12+x21+x22+x31+x32+x41+x42+x51+x52+x61+x62,data=data)
summary(stepAIC(fit2))
summary(fit2)
pR2(fit2)
# Theta:  25427 
# Std. Err.:  128455
#pseudo r square 0.3136413 0.3299029


# Fitting Generalized linear mix models with multivariate 
# normal random effects, using Penalized Poisson Quasi-Likelihood------------------ 

fit3=glmmPQL(y~x11+x12+x21+x22+x31+x32+x41+x42+x51+x52+x61+x62, random = ~ 1,
                family = poisson, data = data)





# Prediction of the goal scoring data by using a poisson regression model----------------
# simulate 1 iteration for predicted full 
# priemerleague season using poisson regression mode

z1=0
i=0
for(i in 1:760)
{
# Here we implementing the estimated coefficients
#(that we get from the fited model) in the simulation 
    z1[i]=median(rpois(999,exp( -0.16044 +(-0.01628*x11[i])+(0*x12[i])+(0.21048*x21[i])
          +(0.21524*x22[i])+(-0.01994*x32[i])+(-0.05513*x41[i])+(-0.06700*x42[i])
         +(0*x51[i])+(0*x52[i])+(-0.31595*x62[i])))) 
}

z1
z11=z1[1:380]
z12=z1[381:760]
data.frame(HomeTeam,AwayTeam,y[1:380],y[381:760],z11,z12) # Checking the result of one simulation


# simulate 1000 iterations for predted full priemerleague
# seasons  using poisson regression model

z=matrix(0,760,1000)
i=0
for(j in 1:1000)
 {
  for(i in 1:760)
   {
       z[i,j]=median(rpois(999,exp( -0.16044 +(-0.01628*x11[i])+(0*x12[i])+(0.21048*x21[i])
          +(0.21524*x22[i])+(-0.01994*x32[i])+(-0.05513*x41[i])+(-0.06700*x42[i])
         +(0*x51[i])+(0*x52[i])+(-0.31595*x62[i]))))
   }
 }

#defining the mode function
getmode <- function(v)
 {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
 }

c1=0
k=0
for(k in 1:760)
  {
    c1[k]=getmode(z[k,])   #finding the mode of for each row of z
   }
c1
c11=c1[1:380]
c12=c1[381:760]

#finding correlation between observed and fitted values

cor(y,c1)


#simulate one full priemerleague season  using negative binomial regression model

nb1=0
i=0
for(i in 1:760)
  {
      nb1[i]=median(rnbinom(999,size= 25427,mu=exp( -0.16044 +(-0.01628*x11[i])+(0*x12[i])+(0.21048*x21[i])
          +(0.21524*x22[i])+(-0.01994*x32[i])+(-0.05513*x41[i])+(-0.06700*x42[i])
         +(0*x51[i])+(0*x52[i])+(-0.31595*x62[i]))))
  }

nb1
nb11=z1[1:380]
nb12=z1[381:760]
cor(y,nb1)


#simulate 1000 full priemerleague seasons  using negative binomial regression model

nb = matrix(0,760,1000)
i = 0
for(j in 1:1000)
 {
  for(i in 1:760)
  {
     nb[i,j]=median(rnbinom(999,size= 25427,mu=exp( -0.16044 +(-0.01628*x11[i])+(0*x12[i])+(0.21048*x21[i])
          +(0.21524*x22[i])+(-0.01994*x32[i])+(-0.05513*x41[i])+(-0.06700*x42[i])
         +(0*x51[i])+(0*x52[i])+(-0.31595*x62[i]))))
  }
 }

#defining the mode function
getmode <- function(v)
 {
   uniqv <- unique(v)
   uniqv[which.max(tabulate(match(v, uniqv)))]
 }

d1=0
k=0
for(k in 1:760)
{
d1[k]=getmode(nb[k,])   #finding the mode of for each row of z
}
d1
d11=d1[1:380]
d12=d1[381:760]


























