#This script is to do Spatial 1 model, zipcode level with informative priors selected from county level data
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
    theta_star2[i] <- theta2
    theta_star[i] <- theta
    
    
  }
  b1[1:55] ~ spatial.exp(theta_star[], xcord[], ycord[], tau, phi, kappa) 
  b3[1:55] ~ spatial.exp(theta_star2[], xcord[], ycord[], tau2, phi2, kappa2) 
  
  for( k in 1:N ){
    y[k] ~ dpois(lambda[k])
    log(lambda[k]) <- b1[R[k]] + b3[R[k]]*TR[k]
    
  }
  theta ~ dnorm(4.210063,0.1142681)
  theta2 ~ dt(0.2050,14.34982,7)
  tau ~ dgamma(0.001, 0.001) 
  phi ~ dunif(0.05,15)
  kappa ~ dunif(0.05,1.95) 
  tau2 ~ dgamma(0.001, 0.001) 
  phi2 ~ dunif(0.05, 15)
  kappa2 ~ dunif(0.05,1.95) 
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
model.file <- file.path("~/Bayesian_final_report/Model_script/zipcode_level/zipcode_Spatial_1.txt") 
inits <- function(){
  list(theta=rnorm(1,2,2), theta2=rnorm(1,0,2),tau=1,tau2=1,phi=1,phi2=1,kappa=1,kappa2=1)
}

nonlinear.sim = bugs(data,inits,
                     model.file=model.file,
                     parameters=c("b1","b3","theta","theta2","phi","phi2"),
                     n.chains=3,n.iter=5000,
                     codaPkg=TRUE,debug=TRUE)

summ <- nonlinear.sim$summary
setwd("~/Bayesian_final_report/Model_file_saved/nonCoda/")
save(summ,file="zipcode_both_spatial_summary.RData")

myModel.coda <- read.bugs(nonlinear.sim)
setwd("~/Bayesian_final_report/Model_file_saved/Coda/")
save(myModel.coda,file="zipcode_both_spatial_coda.RData")
