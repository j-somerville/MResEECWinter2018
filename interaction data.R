rm(list=ls())
##Set wd
setwd("~/Jasmine uni/Imperial/Winter project")
i <- read.csv("InteractionResults.csv")
##Aviary_Out is the aviary that individuals came from at the time of measuring during first social network analysis (SNA)
##Aviary_In is the aviary that individuals came from at the time of measurement during second SNA
##Colour rings are the colour ring combinations that help identify each bird. Birds with just 'M' are birds born in 2018 with just a metal number ring.
##SNA (social network analysis) is the round in which the scores were taken from
##betweenness, closeness and degree are all sociality scores.birds with a score of zero entered the cage on their own.
##Socialty scores were measured as follows during the first SNA:
##In Aviary 1, from 17th December to 5th Jan 2019
##In Aviay 4, from 13th November to 19th November 2018
##In Aviary 7, from 13th November to 25th November 2018
##In Aviary 10, from 4th December to 14th January 2019
##In Aviary 13, from 25th November to 3rd December 2018
##In Aviary 16, from 29th November to 3rd December 2018
##In Aviary 19, from 6th December to 16th December 2018
##In Aviary 22, from 4th January to 14th January 2018
##Socialty scores were measured as follows during the second SNA:
##In Aviary 1, from 28th January to 4th December 2019
##In Aviay 4, from 11th February to 18th February 2019
##In Aviary 7, from 18th February to 25th February 2019
##In Aviary 10, from 4th February to 11th February 2019
##In Aviary 13, from 28th January to 4th February 2019
##In Aviary 16, from 4th Febryary to 11th February 2019
##In Aviary 19, from 11th February to 18th February 2019
##In Aviary 22, from 18th February to 25th February 2019
##Overall, social interactions were measured from 13th November to 14th January (2 months) during the first SNA and from 28th January to 25th February (1 month) during the second SNA. The second round of data was for Georgie Halford's Experiment.
##Transponder is the unique pit tag that each bird has, so when they entered the social network cage, their number would be scanned and recorded. This can change over time as birds can lose their transponders.
##vb = visible badge (mm) and hb is hidden badge (mm). Hidden badge takes into account the bases of black feathers from the badge that may be obsured by other lighter feathers.
##vb and hb were measured 3 times for each bird 
###avb= the average of the 3 vb measurements for each individual
##ahb = the average of the 3 hb measurements for each individual
##Badge size was measured over a 3 day period in October 2018

##Only males are present.
imales<-subset(i, i$sex!="F")
imales
##look at males only in round 1 of SNA data
imales1<-subset(imales, imales$round!="2")
imales1
length(imales1$sex)
##so there are 45 males here. I measured males 3 times each 45x3=135

hist(imales1$vb)
hist(imales1$hb)
#normally distributed
##Now look at badge size measurements in first round of sna data (imales1)
range(imales1$vb)
range(imales1$hb)
##Larger range in hidden badge then visible badge. Shows more likely to be variable when they moult?
var(imales1$vb)
var(imales1$hb)
##More variance in hidden badge. Hidden badge is normally less variable in other studies. May be due to inexperience in measuring hidden badge from observer(me). Quite difficult to determine hidden badge.
mean(imales1$vb)
mean(imales1$hb)
##Visible badge larger then hidden
hist(imales1$vb)
hist(imales1$hb)
plot(imales1$vb~imales1$hb)
#proportional to each other, (hidden badge and visible badge) which is expected
##interested in vb as this is what is on display
boxplot(imales1$vb~imales1$ï..Ring_ID)
##Badge varies between individuals, should be no pattern as ring id is a factor

##Now look at age of birds - does this effect badge size? Physiological effect of badge size?
boxplot(imales1$vb~imales1$age)
##looks like an increase in badge size with age. What about with average?
##subset imales1 so just avb numbers hvb
aimales1<-subset(imales1, imales1$ahb!="NA")
write.csv(aimales1, 'males_interaction1.csv')
table(aimales1$cohort)
##8 males from 2010. 1 male from 2012, 2017 and 2018. 14 from 2013. 21 from 2014. 
hist(aimales1$age)
##Most indivduals around 3-6 years old.
boxplot(imales1$vb~imales1$cohort, xlab="Age", ylab="Visible badge (mm)")
##badge seems larger with age. Potentially not a strong enough correlation. Birds age 9 have very varied badge size (From 30-45mm) Same with birds ages 5 and 6 (37-47cm)
m3<-anova(lm(vb~cohort, data=imales1))
m3
##no significant correlation with age and badge size - perhaps due to small statistical power as only 1 bird in some age

##What about age and sociality?
boxplot(imales1$age~imales1$degree)
boxplot(imales1$age~imales1$betweenness)
boxplot(imales1$age~imales1$closeness)
##Doesn't seem to be an affect of age and personality

m4<-anova(lm(degree~age, data=imales1))
m4
m5<-anova(lm(betweenness~age, data=imales1))
m5
m6<-anova(lm(closeness~age, data=imales1))
m6
##no effect of age on sociality

##What about aviary?
table(aimales1$Aviary_out)
##1 bird in aviary 1, 5 in 4, 6 in 7, 8 in 10, 6 in 13, 8 in 16, 6 in 19 and 6 in 22
##Only one bird in aviary 1. This bird was young and put into the females cage before being sexed.
.
#hist(imales1$Aviary_out)
#boxplot(imales1$Aviary_out~imales1$ï..Ring_ID)
##Here can see which bird is in which aviary. Around ring number 678 in Aviary 1
#model1<-anova(lm(imales1$vb~imales1$Aviary_out))
#model1
##badge size significantly differs different across aviary -  random effect

##Now look at sociality in sampling period 1.
mean(imales1$degree)
mean(imales1$betweenness)
mean(imales1$closeness)
##all quite low scores. What about range - do we see a big range of between individuals?
range(imales1$degree)
range(imales1$betweenness)
range(imales1$closeness)
##Not much range in closeness.Maybe better to concentrate on Degree and Betweeness? Although can't ignore closeness
var(imales1$degree)
var(imales1$closeness)
var(imales1$betweenness)
##Not much variation between individuals with closeness score. Potentially will not find correlation with Vb because of that. 
##Note these personality scores are ranked continious data. Zero inflated. 
hist(aimales1$betweenness)
hist(aimales1$closeness)
hist(aimales1$degree)
##All left skewed, zero inflated. Vb has no correlation with age. 
##therefore, does age need to be in model?

plot(aimales1$degree~aimales1$Aviary_out)
##range of sociality scores across aviaries
boxplot(imales1$degree~imales1$Aviary_out)
##potentially differences here. Interesting to note that the single male in aviary 1 has a degree score well over that of the average (0.98). Does presence of females have an effect on sociality of males? (for another project)
boxplot(imales1$betweenness~imales1$Aviary_out)
##Again,potentially differences between aviaries here (more zeros though)
boxplot(imales1$closeness~imales1$Aviary_out)
##Potentially differences again


##What about badge size and sociality?
boxplot(imales1$vb~imales1$degree)
boxplot(imales1$vb~imales1$closeness)
boxplot(imales1$vb~imales1$betweenness)
##Seems to be a slight negative correlation between visible badge and betweeness and degree.
##Want to test this in model later, plot first


##ggplot graphs
##generated from aimales1 dataset I saved earlier as male_interaction1
m <- read.csv("males_interaction1.csv")

require(ggplot2)

##Degree and visible badge size (avb)=visible badge size
plotD<-ggplot() + 
  geom_point(data=m, aes(x=avb, y=degree), color="red")+
  geom_smooth(data=m, method="lm", aes(x=avb, y=degree), color="red")+theme_bw()+
  theme(axis.text=element_text(size=12),panel.grid=element_blank())+
  labs(y="Degree score", x = "Badge size (mm)")
plotD                                                                                        

##Closeness and badge size
plotC<-ggplot() + 
  geom_point(data=m, aes(x=avb, y=closeness), color="red")+
  theme_bw()+
  theme(axis.text=element_text(size=12),panel.grid=element_blank())+
  labs(y="Closeness score", x = "Badge size (mm)")

plotC


##Betweenness and badge size

plotB<-ggplot() + 
  geom_point(data=m, aes(x=avb, y=betweenness), color="red")+
  theme_bw()+
  theme(axis.text=element_text(size=12),panel.grid=element_blank())+
  labs(y="Betweenness score", x = "Badge size (mm)")

plotB


##arrange in panel
require(gridExtra)
grid.arrange(plotD, plotC, plotB, ncol=3)

##Hidden badge size now
##Degree and hidden badge size (ahb)
plotDh<-ggplot() + 
  geom_point(data=m, aes(x=ahb, y=degree), color="red")+
  theme_bw()+
  theme(axis.text=element_text(size=12),panel.grid=element_blank())+
  labs(y="Degree score", x = "Badge size (mm)")
plotDh                                                                                        

##Closeness and badge size
plotCh<-ggplot() + 
  geom_point(data=m, aes(x=ahb, y=closeness), color="red")+
  theme_bw()+
  theme(axis.text=element_text(size=12),panel.grid=element_blank())+
  labs(y="Closeness score", x = "Badge size (mm)")

plotCh


##Betweenness and badge size

plotBh<-ggplot() + 
  geom_point(data=m, aes(x=ahb, y=betweenness), color="red")+
  theme_bw()+
  theme(axis.text=element_text(size=12),panel.grid=element_blank())+
  labs(y="Betweenness score", x = "Badge size (mm)")

plotBh

##arrange in panel
require(gridExtra)
grid.arrange(plotDh, plotCh, plotBh, ncol=3)


##Analysing badge vs sociality.
##as I am using random model to compare my observed data to,
##I want to test strength of correlation between badge and sociality.
##a simple model - pearson's correlation test - will run efficiently in random permutation model
##Use lm to check random effects and then find suitable model

##however, still check random factors and how they effect fixed effects using linear mixed model
require(nlme)
##degree vs vb
##Look at AIC and correlation to see if model varies with/without random affects of aviary and ring ID
meffectmodelD<-lme(degree~avb, data=aimales1,random=~1|Aviary_out/ï..Ring_ID)
summary(meffectmodelD)
meffectmodelD2<-lme(degree~avb, data=aimales1,random=~1|Aviary_out)
summary(meffectmodelD2)
meffectmodelD3<-lme(degree~avb, data=aimales1,random=~1|ï..Ring_ID)
summary(meffectmodelD3)
##model does not vary much in AIC and BIC values with and without aviary and ID, neither does correlation 
##between degree and vb

##closeness vs vb
meffectmodelC<-lme(closeness~avb, data=aimales1, random=~1|Aviary_out/ï..Ring_ID)
summary(meffectmodelC)
meffectmodelC2<-lme(closeness~avb, data=aimales1,random=~1|Aviary_out)
summary(meffectmodelC2)
meffectmodelC3<-lme(closeness~avb, data=aimales1,random=~1|ï..Ring_ID)
summary(meffectmodelC3)
meffectmodelC3<-lme(closeness~avb, data=aimales1, random=~1|Aviary_out/ï..Ring_ID)
summary(meffectmodelC3)
#not much variation in AIC or BIC values again, or correlation

##betweenness vs vb
meffectmodelB<-lme(betweenness~avb, data=aimales1, random=~1|Aviary_out/ï..Ring_ID)
summary(meffectmodelB)
meffectmodelB2<-lme(betweenness~avb, data=aimales1,random=~1|Aviary_out)
summary(meffectmodelB2)
meffectmodelB3<-lme(betweenness~avb, data=aimales1,random=~1|ï..Ring_ID)
summary(meffectmodelB3)
meffectmodelB3<-lme(betweenness~avb, data=aimales1, random=~1|Aviary_out/ï..Ring_ID)
summary(meffectmodelB3)
##again not much variation

##Now use pearson's correlation test for strength of correlation


##test for strength of correlation with vb
b<-cor.test(imales1$avb, imales1$betweenness, alternative = c("greater"), method=c("pearson"), conf.level = 0.95)
c<-cor.test(imales1$avb, imales1$closeness, alternative = c("greater"), method=c("pearson"), conf.level = 0.95)
d<-cor.test(imales1$avb, imales1$degree, alternative=c("greater"), method=c("pearson"), conf.level = 0.95)


#test for strength of correlation with hb
bh<-cor.test(imales1$ahb, imales1$betweenness, alternative = c("greater"), method=c("pearson"), conf.level = 0.95)
ch<-cor.test(imales1$ahb, imales1$closeness, alternative = c("greater"), method=c("pearson"), conf.level = 0.95)
dh<-cor.test(imales1$ahb, imales1$degree, alternative = c("greater"), method=c("pearson"), conf.level = 0.95)
##find the strength of correlation /estimate
bh
ch
dh

##use this to compare to randomised null model in SNA.simulation code
