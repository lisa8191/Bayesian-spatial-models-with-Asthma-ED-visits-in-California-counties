#This script is to do Spatial 2 model, zipcode level with informative priors selected from county level data
library(R2OpenBUGS)
set.seed(-435374359)


load("~/Bayesian_final_report/Datasets/Adult_with_Zip.RData")
sub2012 <- Adult_time[Adult_time$Year == "2012",]
sub2017 <- Adult_time[Adult_time$Year == "2017",]
rownames(sub2012) <- sub2012$ZIP
rownames(sub2017) <- sub2017$ZIP
new_sub2017 <- sub2017[rownames(sub2012),]
zip_num <- as.numeric(as.factor(sub2012$ZIP))
zip_order <- levels(as.factor(sub2012$ZIP))
county_num <- as.numeric(as.factor(sub2012$County))
county_order <- levels(as.factor(sub2012$County))



model{
  

  for(i in 1:55) {
    b3[i] ~ dnorm(theta2,tau1[i])
    #b3[i] ~ dnorm(theta2,tau1)
    
    tau1[i]  <- pow(sigma.theta[i],-2)
    sigma.theta[i]~dunif(0,1000)
    theta_star[i] <- theta
    
    
  }
  b1[1:55] ~ spatial.exp(theta_star[], xcord[], ycord[], tau, phi, kappa) 
  
  for( k in 1:N ){
    y[k] ~ dpois(lambda[k])
    log(lambda[k]) <-  b1[R[k]] + b3[R[k]]*TR[k]
    
    
  }
  theta ~ dnorm(4.210063,0.1142681) #informative prior from glm estimate
  theta2 ~ dt(0.01644351,14.34982,7)#informative prior from glm estimate
  tau ~ dgamma(0.001, 0.001)
  sigsq <- pow(tau,-1)
  phi ~ dunif(0.05, 15)
  kappa ~ dunif(0.05,1.95) 
  
  #b2 ~ dnorm(0,0.1)
}

load("~/Bayesian_final_report/Datasets/Coordinates.RData")
rownames(coord_Adult) <- coord_Adult$Adult_county
xcoord <- coord_Adult[county_order,"long"]
ycoord <- coord_Adult[county_order,"lat"]


y <- c(sub2012$Number_of_Asthma_ED_Visits,new_sub2017$Number_of_Asthma_ED_Visits)
N <- length(y)
R <- c(county_num,county_num)
z <- c(zip_num,zip_num)
time <- c(rep(0,1030),rep(1,1030))
xcord <- xcoord
ycord <- ycoord
TR <- time


data <- list("y","N","R","xcord","ycord","TR")
model.file <- file.path("~/Bayesian_final_report/Model_script/zipcode_level/zipcode_Spatial_2.txt") 
inits <- function(){
  list(theta=rnorm(1,2,2), theta2=rnorm(1,0,2),tau=1,phi=1,kappa=1)
}
nonlinear.sim = bugs(data,inits,
                     model.file=model.file,
                     parameters=c("b1","b3","theta","theta2","phi","sigsq"),
                     n.chains=3,n.iter=5000,
                     codaPkg=FALSE,debug=TRUE)

summ <- nonlinear.sim$summary
setwd("~/Bayesian_final_report/Model_file_saved/nonCoda/")
save(summ,file="zipcode_spatial_summary.RData")

myModel.coda <- read.bugs(nonlinear.sim)
setwd("~/Bayesian_final_report/Model_file_saved/Coda/")
save(myModel.coda,file="zipcode_spatial_coda.RData")
