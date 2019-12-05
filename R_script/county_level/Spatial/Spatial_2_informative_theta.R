#This script contains Spatial 2 model with informative theta and tau~gamma(0.001,0.001) 

library(R2OpenBUGS)
set.seed(-435374359)

load("~/Bayesian_final_report/Datasets/Final_data_Adult.RData")

model{
  
  for(i in 1:55) {
    #b3[i] ~ dnorm(theta2,tau1[i])
    b3[i] ~ dnorm(theta2,tau1)
    
    #tau1[i]  <- pow(sigma.theta[i],-1)
    #sigma.theta[i]~dunif(0,100)
    theta_star[i] <- theta

 
  }
  b1[1:55] ~ spatial.exp(theta_star[], xcord[], ycord[], tau, phi, kappa) 

  for( k in 1:N ){
    y[k] ~ dpois(lambda[k])
    #log(lambda[k]) <- b1[R[k]] + b2*time[k] + b3[R[k]]*TR[k]
    log(lambda[k]) <- b1[R[k]] + b3[R[k]]*TR[k]
    
    
  }
  theta ~ dnorm(5.984782,0.01571683) #informative priors from glm
  theta2 ~ dt(0.2050,14.34982,7) #informative priors from glm
  tau ~ dgamma(0.001, 0.001) #flat
  sigsq <- pow(tau,-1)
  phi ~ dunif(0.05, 20)#flat
  kappa ~ dunif(0.05,1.95)#flat
}

model.file <- file.path("~/Bayesian_final_report/Model_script/county_level/Spatial/Spatial_2_informative_theta.txt") 
sub2012 <- final_adult[final_adult$Year=="2012",]
sub2017 <- final_adult[final_adult$Year=="2017",]

sub2012$county_num <- as.numeric(as.factor(sub2012$County))
load("~/Bayesian_final_report/Datasets/Coordinates.RData")
county_ord <- levels(as.factor(sub2012$County))
rownames(coord_Adult) <- coord_Adult$Adult_county

#incorporate x and y coordinaes
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
  list(theta=rnorm(1,2,2), theta2=rnorm(1,0,2),tau=1,phi=0.2,sigma.theta=runif(1,0,10))
}
nonlinear.sim = bugs(data,inits,
                     model.file=model.file,
                     parameters=c("b1","b3","theta","theta2","phi","tau1","tau"),
                     n.chains=3,n.iter=5000,
                     codaPkg=TRUE,debug=TRUE)
summ <- nonlinear.sim$summary
setwd("~/Bayesian_final_report/Model_file_saved/nonCoda/")
save(summ,file="Count_spatial_theta_inform_summary.RData")

myModel.coda <- read.bugs(nonlinear.sim)
setwd("~/Bayesian_final_report/Model_file_saved/Coda/")
save(myModel.coda,file="Count_spatial_theta_inform_coda.RData")