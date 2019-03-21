# VideoData
rm(list=ls())
setwd("~/Jasmine uni/Imperial/Winter project/")
library(igraph)
library(dplyr)
library(plyr)
##HB=hidden badge, VB=visible badge
#Read in RFID-fitted cage data from every aviary:
total_interaction<- read.csv("~/Jasmine uni/Imperial/Winter project/total_interaction2.csv")
data<-total_interaction
str(data)
badge<-(read.csv("Badge1.csv")
##this is each individual's badge data      
Result_Dataset <- data.frame(NULL)
RData<-data.frame(NULL)
##every time you run line 14, the table empties, so only run when practicing

#Create for loop to calculate measures of centrality (degree, closeness, betweenness
# for each occasion (i) and every individual. el creates a matrix that extracts
#the two columns of individuals. newGraph pairs individuals from the same row to 
#show they interact. directed=FALSE means neither individual initiated the interaction.
#f creates another data frame containing functions that will calculate measures of centrality
#for each individual. rownames<-NULL is used to remove the additional individual name 
#attached to degree. rbind will add this new data frame (f) on the bottom of my initial
#table.

library(lme4)
kk<-seq(1:100)
for (k in kk){
  Result_Dataset <- data.frame(NULL)  

  for (j in unique(data$occasion)){
    a <- subset(data, occasion == j)
    av <- unique(a$aviary)
    occ <- j
    for (i in av){
      b<-data.frame(NULL)
      b <- subset(a, a$aviary == i)
      len<-length(b$id1)
      Posid1<-unique(b$id1)
      Posid2<-unique(b$id2)
      rid1<-sample(Posid1, size=len, replace=TRUE)
      rid2<-sample(Posid2, size=len, replace=TRUE)
      df<-data.frame(rid1,rid2)
      el<-as.matrix(df)
      # the above is the permutation
      net <- graph.data.frame(el, directed=FALSE)
      E(net)$weight <- 1
      net2 <- simplify(net, edge.attr.comb = list(weight="sum"))
      nG <- simplify(net2, edge.attr.comb = list(weight="sum"))
      NewDat <- NULL
      NewDat <- as.data.frame(degree(nG))
      NewDat$ID <- row.names(NewDat)  
      NewDat$ocassion <- paste(j,  sep = " ")
      NewDat$Av <- paste(i, sep="")
      NewDat$Degree <- degree(nG)
      NewDat$Strength <- strength(nG)
      NewDat$Closeness <- closeness(nG)
      NewDat$Betweenness <- betweenness(nG)
      NewDat$Hb<-badge$ahb[match(NewDat$ID,badge$ï..Transponder)]
      # the next two are variables on network - that means they generate the same value for each entry for that network. 
      NewDat$Rdensity<-c(rep(edge_density(nG),length(strength(nG))))
      NewDat$RNVertices<-c(rep(gorder(nG),length(strength(nG))))
      NewDat <- NewDat[,-1]
      #Saving each file under a different name. 
      assign(paste("Event",i, "Dataset", sep = ""), NewDat )
      #Compiling all the data.
      Result_Dataset <- rbind(Result_Dataset,NewDat)
    }
  }
  
##subset so only have males and occasion one as I am not interested in females or sampling occasion 2
  Maleshb<-subset(Result_Dataset, Result_Dataset$ocassion!="2")
 Males2hb<-subset(Maleshb, Maleshb$Hb!="NA")
  
##now to do the cor.test with new males data.frame, for badge vs sociality. This will be included in the loop
  
  RslopeD<-cor.test(Males2hb$Hb, Males2hb$Degree, alternative=c("greater"), method=c("pearson"))
  rD<-data.frame(NULL)
  rD<-as.numeric(RslopeD$estimate)
  
  RslopeC<-cor.test(Males2hb$Hb, Males2hb$Closeness, alternative=c("greater"), method=c("pearson"))
  rC<-data.frame(NULL)
  rC<-as.numeric(RslopeC$estimate)
  
  RslopeB<-cor.test(Males2hb$Hb, Males2hb$Betweenness, alternative=c("greater"), method=c("pearson"))
  rB<-data.frame(NULL)
  rB<-as.numeric(RslopeB$estimate)
  

##fills new table with permutation correlation data, (RSlope) for betweenness, closeness and 
##degree   
  
  TempR <- NULL
  TempR <- as.data.frame(rD[[1]])
  TempR$RslopeD<-rD
  TempR$RslopeB<-rB
  TempR$RslopeC<-rC
  
  TempR<-TempR[,-1]
  RData <-rbind(RData, TempR)
}
write.csv(RData, "SlopeSimulationHB.csv")
##here this is for HB. change code to VB when applicable

##run through whole code to get 1000 permutations


#get quantile of badge data (at 5% as my data has negative correlation)
degreeh<-quantile(RData$RslopeD, probs = 0.05)
degreeh
#so the slope at 5% =-0.1360786 for the random permutations of degree and HB

#sort permutation data into ranked order 
Ah<-sort(RData$RslopeD)
Ah
##My actual observed slope for degree = -0.1318404, which just comes outside the 
#5% quartile (degreeh)
##When comparing to the ranked data the observed slope fits in at rank 55, so p =0.055/0.06 
#and not signficant

#For hidden badge
closenessh<-quantile(RData$RslopeC, probs = 0.05)
closenessh

#so the slope at 5% =-0.1276615  for the random permutations of closeness and badge

hh<-sort(RData$RslopeC)
hh
##My actual observed slope for CLOSENESS = -0.0510324 which is inside the 
#5% quantile number (closenessh)
##When comparing to the ranked data the observed slope fits in at rank 210, so p =0.21 
#and not signficant

#For hidden badge
betweennessh<-quantile(RData$RslopeB, probs = 0.05)
betweennessh

#so the slope at 5% =-0.2401781 for the random permutations of closeness and badge

bb<-sort(RData$RslopeB)
bb
##My actual observed slope for degree = -0.1624953 which is inside the 
#5% quantile number (closenessh)
##When comparing to the ranked data the observed slope fits in at rank 163, so p =0.163 
#and not signficant

##for visible badge
degree<-quantile(RData$RslopeD, probs = 0.05)
degree
#so the slope at 5% = -0.038601 for the random permutations of degree and badge

A<-sort(RData$RslopeD)
A
##My actual observed slope for degree = -0.08744, which comes outside the 
#5% quantile number
##When ranking the permutation data, the observed slope comes in at 17. 
#(from lowest to highest)
##therefore, less than 1 in 50 chance that this happened by chance p<0.02


betweenness<-quantile(RData$RslopeB, probs = 0.05)
betweenness
##so slope at 5% for the random permutations of betweeness and badge= -0.227935
B<-sort(RData$RslopeB)
B
#My actual observed slope for betweenness =-0.0906, which greater then the 5% quantile 
#number of the random permutations and is therefore inside the range of data - not significant
##When ranking this permutation data, the observed slope comes in at 294.
#294 out of 1000 = 0.294 chance of this coming up up. So this is greater than 0.05
##not significant p=0.294

closeness<-quantile(RData$RslopeC, probs= 0.05)
closeness
##so slope at 5% for the random permutations of badge and closnesss is -0.1844579
C<-sort(RData$RslopeC)
C
##My actual observed slope for closeness is 0.00906, which is greater then the 5% quantile
##number of random permutations and is therefore inside the range of data -not significant
##When ranking the permutation data, the observed slope comes in at 410. Therefore, the p
#value is 410/100 = p=0.410


par(mfrow = c(1, 3))

#Plot permutation data 

##plot of random permuatation of badge vs Degree(1000)
plot(density(RData$RslopeD), xlim=c(-0.5,0.5), main="", xlab="Degree")
#plot the slope of my actual data
lines(x=c(-0.1318404,-0.1318404), y=c(0,7), col="red")

#plot mean of random permutation slope
#lines(x=c(0.1196,0.1196), y=c(0,7), col="blue", lty=2)



##plot of random permutation for badge vs closeness (1000)
plot(density(RData$RslopeC), xlim=c(-0.5,0.5), main="", xlab="Closeness")
#slope of observed data
lines(x=c(-0.0510324,-0.0510324), y=c(0,7), col="red")
##mean of RsclopeC
#lines(x=c(0.0228,0.0228), y=c(0,70), col="blue", lty=2)

##plot the random permutations for badge vs betweeness (1000)
plot(density(RData$RslopeB), xlim=c(-0.5,0.5), main="", xlab="Betweenness")
#slope of data

lines(x=c(-0.1624953,-0.1624953), y=c(0,7), col="red")
#mean of RslopeB data
#lines(x=c(-0.01968,-0.01968), y=c(0,7), col="blue", lty=2)
#lines(x=c(0.60,0.60), y=c(0,7.9), col="red", lty=2)
