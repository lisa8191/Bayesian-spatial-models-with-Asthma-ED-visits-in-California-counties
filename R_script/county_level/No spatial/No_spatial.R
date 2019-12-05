#This one is for No spatial model for county level

library(R2OpenBUGS)
set.seed(-435374359)
load("~/Bayesian_final_report/Datasets/Final_data_Adult.RData")

model{
  
  for(i in 1:55) {
    b1[i] ~ dnorm(theta,tau[i])
    b3[i] ~ dnorm(theta2,tau2[i])
    tau[i]  <- pow(sigma.theta[i],-2)
    sigma.theta[i]~dunif(0,1000)
    tau2[i]  <- pow(sigma.theta2[i],-2)
    sigma.theta2[i]~dunif(0,1000)

  }
  for( k in 1:N ){
    y[k] ~ dpois(lambda[k])
    log(lambda[k]) <- b1[R[k]]  + b3[R[k]]*TR[k]
    
  }
  theta ~ dnorm(0,0.000001)
  theta2 ~ dnorm(0,0.000001)
 
}

model.file <- file.path("~/Bayesian_final_report/Model_script/county_level/No_spatial/No_spatial.txt") 
sub2012 <- final_adult[final_adult$Year=="2012",]
sub2017 <- final_adult[final_adult$Year=="2017",]
sub2012$county_num <- as.numeric(as.factor(sub2012$County))
#create dataset for bug input
y = c(sub2012$ED.visit,sub2017$ED.visit)
N <- length(y)
R <- rep(sub2012$county_num,2)
time <- c(rep(0,55),rep(1,55))
TR <- time




data <- list("y","N","R","time","TR")
inits <- function(){
  list(theta=rnorm(1,2,2), theta2=rnorm(1,0,2),sigma.theta=runif(55,0,10),sigma.theta2=runif(55,0,10))
}
nonlinear.sim = bugs(data,inits,
                     model.file=model.file,
                     parameters=c("b1","b3","theta","theta2"),
                     n.chains=3,n.iter=5000,
                     codaPkg=TRUE,debug=TRUE)
