library(readr)
library(tidyverse)
library(urbnmapr)
library(gridExtra)
library(ggplot2)
require(rms)
`%!in%` = Negate(`%in%`)
ED_data <- read_csv("D:/Lisa/Documents/Fall_2019/final_project/Dataset/asthma-ed-visit-rates-by-zip-code.csv")
#restrict to 2012 to 2017
ED_data2 <- ED_data[(ED_data$Year %in% c("2012","2013","2014","2015","2016","2017")),]
dd <- table(ED_data2$ZIP, ED_data2$Year)
bad <- c()
for(j in 1:nrow(dd)){
  len <- length(which(dd[j,] == 1))
  if(len >= 1){
    bad <- c(bad,rownames(dd)[j])
  }
}
ED_data2 <- ED_data2[ED_data2$ZIP %!in% bad,]


#select all the zip codes in the seven years
y1 <- as.character(as.matrix(ED_data2[ED_data2$Year == "2012","ZIP"]))
u1 <- unique(y1)
for(i in c("2013","2014","2015","2016","2017")){
  y <- as.character(as.matrix(ED_data2[ED_data2$Year == i,"ZIP"]))
  u <- unique(y)
  u1 <- intersect(u1,u)
  
}

final_data <- ED_data2[(ED_data2$Year %in% c("2012","2013","2014","2015","2016","2017")) &(ED_data2$ZIP %in% u1), ]

empty <- matrix(NA,nrow=1,ncol=ncol(final_data))


final_data <- data.frame(final_data)
d2 <- table(final_data$ZIP, final_data$Year)
only_two <- c()
for(j in 1:nrow(d2)){
  len <- length(which(d2[j,] == 2))
  if(len >= 1){
    only_two <- c(only_two,rownames(d2)[j])
  }
}

for(z in only_two){
  for(y in c("2012","2013","2014","2015","2016","2017")){
    
    dat <- final_data[(final_data$Year == y) & (final_data$ZIP == z),]
    what <- unique(dat$Age_Group)  
    
    if("Children (0-17)" %!in% what){
      added <- empty
      added[,c(1:2)] <- as.numeric(dat[1,c(1:2)])
      added[,6] <- as.character(dat[1,6])
      added[,8] <- as.character(dat[1,8])
      added[1,3] <- "Children (0-17)"
      added[1,4] <- as.numeric(dat[dat$Age_Group == "All Ages","Number_of_Asthma_ED_Visits"]) - as.numeric(dat[dat$Age_Group == "Adults (18+)","Number_of_Asthma_ED_Visits"])
      colnames(added) <- colnames(final_data)
      final_data <- rbind(final_data, added)
    }else if("Adults (18+)" %!in% what){
      
      added <- empty
      added[,c(1:2)] <- as.numeric(dat[1,c(1:2)])
      added[,6] <- as.character(dat[1,6])
      added[,8] <- as.character(dat[1,8])
      added[1,3] <- "Adults (18+)"
      added[1,4] <- as.numeric(dat[dat$Age_Group == "All Ages","Number_of_Asthma_ED_Visits"]) - as.numeric(dat[dat$Age_Group == "Children (0-17)","Number_of_Asthma_ED_Visits"])
      colnames(added) <- colnames(final_data)
      final_data <- rbind(final_data, added)
      
    }else{
      final_data <- final_data
    }
    
  }
  
}

d3 <- table(final_data$ZIP, final_data$Year) #all hae three numbers 


  colnames(final_data)[8] <- "county_fips"
final_data_red <- final_data[,c(1,2,3,4,6,8)]
theNA <- which(is.na(final_data_red$Number_of_Asthma_ED_Visits))

for(j in theNA){
  datt <- final_data_red[j,]
  year <- datt[,"Year"]
  Zip <- datt[,"ZIP"]
  if(datt$Age_Group == "Children (0-17)"){
    newdat_all <- final_data_red[which(final_data_red$Year == year & final_data_red$ZIP == Zip & final_data_red$Age_Group == "All Ages"),]
    newdat_old <- final_data_red[which(final_data_red$Year == year & final_data_red$ZIP == Zip & final_data_red$Age_Group == "Adults (18+)"),]
    
    final_data_red[j,"Number_of_Asthma_ED_Visits"] <- as.numeric(newdat_all$Number_of_Asthma_ED_Visits) - as.numeric(newdat_old$Number_of_Asthma_ED_Visits)
  }else if(datt$Age_Group == "Adults (18+)"){
    newdat_all <- final_data_red[which(final_data_red$Year == year & final_data_red$ZIP == Zip & final_data_red$Age_Group == "All Ages"),]
    newdat_young <- final_data_red[which(final_data_red$Year == year & final_data_red$ZIP == Zip & final_data_red$Age_Group == "Children (0-17)"),]
    
    final_data_red[j,"Number_of_Asthma_ED_Visits"] <- as.numeric(newdat_all$Number_of_Asthma_ED_Visits) - as.numeric(newdat_young$Number_of_Asthma_ED_Visits)
    
  }
  
}

final_rid <- unique(final_data_red[which(is.na(final_data_red$Number_of_Asthma_ED_Visits)),"ZIP"])

final_data_red_final <- final_data_red[final_data_red$ZIP %!in% final_rid,]
#final data


save(final_data_red_final,file="final_dataset.RData")

#separate by group and extract only 2012 and 2017

load("D:/Lisa/Documents/Fall_2019/final_project/Dataset/final_dataset.RData")
Adult <- final_data_red_final[final_data_red_final$Age_Group == "Adults (18+)",]
Child <- final_data_red_final[final_data_red_final$Age_Group == "Children (0-17)",]
Adult_time <- Adult[Adult$Year == "2012"|Adult$Year == "2017",]
Child_time <- Child[Child$Year == "2012"|Child$Year == "2017",]


setwd("D:/Lisa/Documents/Fall_2019/final_project/Dataset")
save(Adult_time,file="Adult_with_Zip.RData")
save(Child_time,file="Child_with_Zip.RData")


Adult2012 <- Adult[Adult$Year == "2012",]
Adult2017 <- Adult[Adult$Year == "2017",]

Child2012 <- Child[Child$Year == "2012",]
Child2017 <- Child[Child$Year == "2017",]

#add up to all counties for the adult
Adult_county <- unique(Adult$County)
catch <- rep(NA,length(Adult_county))
county_fips <- rep(NA,length(Adult_county))
data <- Adult2012
for(i in 1:length(Adult_county)){
  catch[i] <- sum(as.numeric(data[which(data$County == Adult_county[i]),"Number_of_Asthma_ED_Visits"]))
  county_fips[i] <- unique(data[which(data$County == Adult_county[i]),"county_fips"])
}

final_dat <- data.frame(ED.visit=catch)
final_dat$Year <- "2012"
final_dat$Age.Group <- "Adult"
final_dat$County <- Adult_county
final_dat$county_fips <- county_fips

catch <- rep(NA,length(Adult_county))
county_fips <- rep(NA,length(Adult_county))
data <- Adult2017
for(i in 1:length(Adult_county)){
  catch[i] <- sum(as.numeric(data[which(data$County == Adult_county[i]),"Number_of_Asthma_ED_Visits"]))
  county_fips[i] <- unique(data[which(data$County == Adult_county[i]),"county_fips"])
}

temp_dat <- data.frame(ED.visit=catch)
temp_dat$Year <- "2017"
temp_dat$Age.Group <- "Adult"
temp_dat$County <- Adult_county
temp_dat$county_fips <- county_fips

final_adult <- rbind(final_dat,temp_dat)
library(readr)
library(tidyverse)
library(urbnmapr)
library(gridExtra)
library(ggplot2)
require(rms)
final_adult_coord <- final_adult %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="California")


long <- rep(NA,length(Adult_county))
lat <- rep(NA,length(Adult_county))
data = final_adult_coord
for(i in 1:length(Adult_county)){
  long[i] <- mean(as.numeric(data[which(data$County == Adult_county[i]),"long"]))
  lat[i] <- mean(as.numeric(data[which(data$County == Adult_county[i]),"lat"]))
}

coord <- data.frame(cbind(Adult_county, long,lat))
coord$long <- as.numeric(as.character(long))
coord$lat <- as.numeric(as.character(lat))
coord_Adult <- coord


#For Children 


Child_county <- unique(Child$County)
catch <- rep(NA,length(Child_county))
county_fips <- rep(NA,length(Child_county))
data <- Child2012
for(i in 1:length(Child_county)){
  catch[i] <- sum(as.numeric(data[which(data$County == Child_county[i]),"Number_of_Asthma_ED_Visits"]))
  county_fips[i] <- unique(data[which(data$County == Child_county[i]),"county_fips"])
}

final_dat <- data.frame(ED.visit=catch)
final_dat$Year <- "2012"
final_dat$Age.Group <- "Child"
final_dat$County <- Child_county
final_dat$county_fips <- county_fips

catch <- rep(NA,length(Child_county))
county_fips <- rep(NA,length(Child_county))
data <- Child2017
for(i in 1:length(Child_county)){
  catch[i] <- sum(as.numeric(data[which(data$County == Child_county[i]),"Number_of_Asthma_ED_Visits"]))
  county_fips[i] <- unique(data[which(data$County == Child_county[i]),"county_fips"])
}

temp_dat <- data.frame(ED.visit=catch)
temp_dat$Year <- "2017"
temp_dat$Age.Group <- "Child"
temp_dat$County <- Child_county
temp_dat$county_fips <- county_fips

final_child <- rbind(final_dat,temp_dat)

#use this for plotting later
final_child_coord <- final_child  %>% 
  left_join(counties, by = "county_fips") %>% 
  filter(state_name =="California")


long <- rep(NA,length(Child_county))
lat <- rep(NA,length(Child_county))
data = final_child_coord
for(i in 1:length(Child_county)){
  long[i] <- mean(as.numeric(data[which(data$County == Child_county[i]),"long"]))
  lat[i] <- mean(as.numeric(data[which(data$County == Child_county[i]),"lat"]))
}

coord <- data.frame(cbind(Child_county, long,lat))
coord$long <- as.numeric(as.character(long))
coord$lat <- as.numeric(as.character(lat))
coord_Child <- coord
setwd("D:/Lisa/Documents/Fall_2019/final_project/Dataset")
save(coord_Adult,coord_Child,file="Coordinates.RData")

save(final_child,file="Final_data_Child.RData")
save(final_adult,file="Final_data_Adult.RData")


#obtain distance matrix
coord_temp <- coord_Adult
rownames(coord_temp) <- coord_temp$Adult_county
coord_temp <- coord_temp[,-1]
adult_dis <- as.matrix(dist(coord_temp))
coord_temp <- coord_Child
rownames(coord_temp) <- coord_temp$Child_county
coord_temp <- coord_temp[,-1]
child_dis <- as.matrix(dist(coord_temp))
save(adult_dis,child_dis,file="Distance_matrix.RData")
