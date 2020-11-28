#import the Montesinho_park Dataset from the folder via .csv file ie comma seperated value
Montesinhopark_forestfires <- read.csv("D:/Academic/Montesinhopark_forestfires.csv")  
View(Montesinhopark_forestfires)
hist(Montesinhopark_forestfires$X)
summary(Montesinhopark_forestfires)
names(Montesinhopark_forestfires)
attach(Montesinhopark_forestfires)
mean(X)
#another approach when data is detached and and we use a dollar sign
detach(Montesinhopark_forestfires)
mean(Montesinhopark_forestfires$FFMC)
class(FFMC)
summary(Montesinhopark_forestfires)
dim(Montesinhopark_forestfires)
Montesinhopark_forestfires$FFMC[5:20]#gives the value of FFMC from 5 to 20
Montesinhopark_forestfires$FFMC[5:20,]
mean(FFMC[day=="mon"])#value of FFMC of the day mon 's mean
x1<-Montesinhopark_forestfires[month=="mar",]#subset the data to avoid error //y can extract more of the code
x2<-Montesinhopark_forestfires[month=="feb",]
x3<-Montesinhopark_forestfires[FFMC>50,]
rain[100:200]
area[400:800]
temp<-rain>5#will return the rain units greater than 5
temp2<-as.numeric(rain>0)
r<-rain>0 & month=="mar"
md<-cbind(Montesinhopark_forestfires,temp2)
#barplot visual frequency table
 ?barplot()
table(FFMC)
count<-table(FFMC)
count
#for percentages divide the number with the total number of columns
 barplot(count)
 count<-table(FFMC)
  count
barplot(count)
percent<-table(FFMC)/1021
barplot(FFMC)
barplot(FFMC,xlab="FFMC",ylab = "%",las=1)#las means orientation of the tick mark variables
boxplot(FFMC)
boxplot(temp)
boxplot(ISI)
boxplot(DC)
boxplot(DMC)
quantile(FFMC,probs=c(0,0.25,0.50,0.75,1))
quantile(ISI,probs=c(0,0.25,0.50,0.75,1))
boxplot(ISI,main="BOXPLOT",ylab="ISI",las=1)
boxplot(FFMC~ISI)
boxplot(DC~ISI)
boxplot(DMC~ISI)
boxplot(ISI[area=="0"],ISI[rain=="0"])
boxplot(ISI~DMC,main="Litter vs Forest burnt",ylab="Expected forest burn(ISI)",las=1)
stem(ISI)
#examine the relationship between ISI & DMC/FFMC/DC
#produce a contingency table
table(ISI,FFMC)
barplot(table(ISI))
#relationships between ISI and FFMC
barplot(table(ISI,FFMC))
mosaic(table(ISI,FFMC))#not much of help
summary(ISI)
#scatterplot to examine the linear relationship
plot(ISI)
plot(ISI~DMC)
plot(ISI,FFMC,main="Scatterplot",ylab="FFMC",las=1)
#correlation
cor(ISI,FFMC)
cor(ISI,DC)
cor(ISI,DMC)
abline(ISI~DMC)
lines(smooth.spline(ISI,FFMC))
min(ISI)
max(ISI)
plot(ISI,FFMC, main="Scatterplot", cex=0.50)
plot(ISI,FFMC, main="Scatterplot", cex=0.50,cex.main=2,cex.lab=1.5)
plot(ISI,FFMC, main="Scatterplot", font.main=3)#italic
plot(ISI,FFMC, main="Scatterplot", font.main=4)#bold
plot(ISI,FFMC, main="Scatterplot", font.lab=2)
plot(ISI,FFMC, main="Scatterplot", font.lab=2,font.axis=3)
plot(ISI,FFMC, main="Scatterplot", pch=2)#triangular notations for the values
abline(lm(FFMC~ISI))#predict FFMC using ISI
#t-test analysis
t.test(ISI,mu=8.98,alternaties="less",level=0.95)
t.test(ISI,FFMC)
t.test(ISI,DMC)
var(ISI)
var(FFMC)
TAB=table(ISI,FFMC)
barplot(TAB,beside=T,legend=T)
chisq.test(TAB)
mod1<-lm(ISI~FFMC)
summary(mod1)
plot(mod1)#see all the plots you get and analyze them correctly
abline(mod1)
attributes(mod1)
coefficients(mod1)
confint(mod1)
anova(mod1)
cor(ISI,FFMC,method="pearson")
plot(cor(ISI,FFMC,method="pearson"),ylab="cor value")
#multiple linear regression in R
mod2<-lm(ISI~FFMC+DMC)
summary(mod2)
mod3<-lm(ISI~FFMC+DMC+DC)
summary(mod3)
mod4<-lm(ISI~rain+wind+RH+temp)
summary(mod4)
plot(mod3)
plot(mod4)
#interaction or effect modification
mod6<-lm(ISI~FFMC*DMC)
#how are the forest fires are distributed in the park
fit=glm(log(area+1)~FFMC+DMC+DC+ISI+temp+RH+wind+rain)
plot(fit)
summary(fit)
#what suggestions can we give on the basis of the analyzation of the relationships
plot(RH)
barplot(RH)
plot(RH,ISI)
barplot(ISI,RH)
#thus people should avoid visiting the park when relative humidity is lower
#Analyze the fanning effect
#1.work on the intervals
#draw the histograms
plot(wind,ISI)
#yes wind helps the spread of fire
plot(rain,ISI)
#when the rain is zero the fire has maximum spreading trends
plot(temp,ISI)
#when the temperature is maximum[10-30 degree celsius] the fire is maximum
plot(ISI,area)
#trends are observed with the initial spread of fire are out of nowhere
#perform the T-test in R
x1<-rnorm(ISI)
x2<-rnorm(FFMC)
t.test(x1,x2)
#used to determine whether the means of two groups are equal to each other. The assumption for the test is that both groups are sampled from normal distributions with equal variances. The null hypothesis is that the two means are equal, and the alternative is that they are not
plot(mean(x1),mean(x2))