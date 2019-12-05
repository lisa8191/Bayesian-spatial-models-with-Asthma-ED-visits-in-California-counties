#This script is for county level Spatial 1 model. In this model, we included spatial correlation 
#for both b1 and b3
library(R2OpenBUGS)
set.seed(-435374359)
load("~/Bayesian_final_report/Datasets/Final_data_Adult.RData")

model{
  
  for(i in 1:55) {
    #each theta
    theta_star2[i] <- theta2
    theta_star[i] <- theta
    
 
  }
  #b1 and b3 are both with spatial correlation with mean theta and theta2 as mean
  b1[1:55] ~ spatial.exp(theta_star[], xcord[], ycord[], tau, phi, kappa) 
  b3[1:55] ~ spatial.exp(theta_star2[], xcord[], ycord[], tau2, phi2, kappa2) 
  
  for( k in 1:N ){
    y[k] ~ dpois(lambda[k])
    log(lambda[k]) <- b1[R[k]] + b3[R[k]]*TR[k]
    
  }
  #flat priors
  theta ~ dnorm(0,0.000001)
  theta2 ~ dnorm(0,0.000001)
  tau ~ dgamma(0.001, 0.001) 
  phi ~ dunif(0.05, 50)
  kappa ~ dunif(0.05,1.95) 
  tau2 ~ dgamma(0.001, 0.001) 
  phi2 ~ dunif(0.05, 50)
  kappa2 ~ dunif(0.05,1.95) 
}

model.file <- file.path("~/Bayesian_final_report/Model_script/county_level/Spatial/Spatial_1.txt") 
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
  list(theta=rnorm(1,2,2), theta2=rnorm(1,0,2),tau=1,tau2=1,phi=1,phi2=1,kappa=1,kappa2=1)
}
nonlinear.sim = bugs(data,inits,
                     model.file=model.file,
                     parameters=c("b1","b3","theta","theta2","phi","phi2","tau","tau2"),
                     n.chains=3,n.iter=5000,
                     codaPkg=TRUE,debug=TRUE)

myModel.coda <- read.bugs(nonlinear.sim)
setwd("~/Bayesian_final_report/Model_file_saved/Coda/")
save(myModel.coda,file="Count_spatial_coda_both_spatial.RData")

