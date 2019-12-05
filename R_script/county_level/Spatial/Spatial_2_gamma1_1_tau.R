#This script contains Spatial 2 model with informative theta and tau~gamma(1,1) for tau priors sensitivity
library(R2OpenBUGS)
set.seed(-435374359)

load("~/Bayesian_final_report/Datasets/Final_data_Adult.RData")

model{
  
  for(i in 1:55) {
    b3[i] ~ dnorm(theta2,tau1[i])
    #b3[i] ~ dnorm(theta2,tau1)
    
    tau1[i]  <- pow(sigma.theta[i],-2)
    sigma.theta[i]~dunif(0,1000)
    theta_star[i] <- theta
    
    
  }
  #b1 are both with spatial correlation with mean theta and theta2 as mean
  
  b1[1:55] ~ spatial.exp(theta_star[], xcord[], ycord[], tau, phi, kappa) 
  
  for( k in 1:N ){
    y[k] ~ dpois(lambda[k])
    log(lambda[k]) <- b1[R[k]] + b3[R[k]]*TR[k]
    
    
  }
  theta ~ dnorm(5.984782,0.01571683) #informative theta from glm
  theta2 ~ dt(0.2050,14.34982,7)#informative theta from glm
  tau ~ dgamma(1, 1) #change to assess sensitivity
  sigsq <- pow(tau,-1)
  phi ~ dunif(0.05, 15)
  kappa ~ dunif(0.05,1.95)
}

model.file <- file.path("~/Bayesian_final_report/Model_script/county_level/Spatial/Spatial_2_gamma1_1.txt") 
sub2012 <- final_adult[final_adult$Year=="2012",]
sub2017 <- final_adult[final_adult$Year=="2017",]
sub2012$county_num <- as.numeric(as.factor(sub2012$County))
load("~/Bayesian_final_report/Datasets/Coordinates.RData")
county_ord <- levels(as.factor(sub2012$County))
rownames(coord_Adult) <- coord_Adult$Adult_county
#include the longitude and latitude as x and y coordinates

x_cood <- coord_Adult[county_ord,"long"]
y_cood <- coord_Adult[county_ord,"lat"]


y = c(sub2012$ED.visit,sub2017$ED.visit)
N <- length(y)
R <- rep(sub2012$county_num,2)
time <- c(rep(0,55),rep(1,55))
TR <- time
xcord <- x_cood
ycord <- y_cood

data <- list("y","N","R","time","TR","xcord","ycord")
inits <- function(){
  list(theta=rnorm(1,2,2), theta2=rnorm(1,0,2),tau=1,phi=1,sigma.theta=runif(55,0,10),kappa=1)
}
nonlinear.sim = bugs(data,inits,
                     model.file=model.file,
                     parameters=c("b1","b3","theta","theta2","phi","tau1","tau","sigsq"),
                     n.chains=3,n.iter=5000,
                     codaPkg=TRUE,debug=TRUE)


myModel.coda <- read.bugs(nonlinear.sim)
