
drinkdata<-read.csv('C:/Users/ellen/Documents/School/stats640/proj2/DrinkPilot.csv', header=TRUE)
head(drinkdata)
#group 1 = no etoh/no stress, 2 = no etoh / stress, 3 = etoh / no stress, 4 = etoh/stress
# now seperated by either etoh yes/no (12, 34) or stress yes/no (13,24)

grp24_base<-drinkdata[drinkdata$TxCode=='2' |drinkdata$TxCode=='4' ,3]
grp24_test<-drinkdata[drinkdata$TxCode=='2' |drinkdata$TxCode=='4' ,4]

n_grp24_test<-length(grp24_test)
n_grp24_base <-length(grp24_base)

# decided to test whether chronic stress exposure increases drinking in mice with 1hr access
#within subject 
meanbase<-mean(grp24_base)
meantest<-mean(grp24_test)
sdbase<-sd(grp24_base)
sdtest<-sd(grp24_test)
medianbase<-median(grp24_base)
mediantest<-median(grp24_test)
iqrbase<-IQR(grp24_base)
iqrtest<-IQR(grp24_test)

(mean(grp24_test-grp24_base))
(sd(grp24_test-grp24_base))
(median(grp24_test-grp24_base))
(IQR(grp24_test - grp24_base))

#a priori power

# test assumptions:
#independence


#normality 
library('nortest')
library('car')
lillie.test(grp24_base) #p=0.7004
lillie.test(grp24_test) #p=0.5241
# the data is  normal
library(e1071)
(kurtosis(grp24_base))
(kurtosis(grp24_test))

par(mfrow=c(2, 2),pty='s')
par(mfrow=c(1,1),pty='m')
qqnorm(grp24_base, main="Baseline Drinking Q-Q Plot", col = 'indianred2',lwd='4')
qqnorm(grp24_test,main="Test Drinking Q-Q Plot",col = 'indianred4',lwd='4')
hist(grp24_base, main="Baseline Drinking Histogram", xlab="Avg EtOH Consumption (g/kg)", ylab="Density", xlim=c(.5,4), ylim=c(0,5), breaks=c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4),col = 'indianred2')
hist(grp24_test, main="Test Drinking Histogram", xlab="Avg EtOH Consumption (g/kg)", ylab="Density", xlim=c(.5,4), ylim=c(0,5),  breaks=c(0.5,0.75,1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5,3.75,4),col = 'indianred4')
par(mfrow=c(1,1),pty='m')
plot(grp24_test,grp24_base)
#HOV
y<-rep.int(0,n_grp24_base)
yy <- rep.int(1,n_grp24_test)
x <- data.frame(d=c(grp24_base, grp24_test), g=as.factor(c(y,yy)))
head(x)
leveneTest(x$d, x$g, center='mean')
#we can assume homogeneity of variance 

par(mfrow=c(1,1))
boxplot(grp24_base,grp24_test,ylab="EtOH consumption (g/kg)",xlab="Treatment", names=(c("Pre-stress Baseline","Post-stress Test")), col=c('indianred2','indianred4'))

sub<-(grp24_test-grp24_base)
sub
t.test(sub, alternative="greater")
t.test(grp24_test , grp24_base,paired = TRUE, alternative="greater")


cv<-abs(qt(0.025,9,lower.tail = FALSE))
t<-(mean(sub)-0)/(sd(sub)/sqrt(10))

uppercv<- mean(sub)+(cv * (sd(sub)/sqrt(10)))
lowercv<- mean(sub)-(cv * (sd(sub)/sqrt(10)))

#effect size 
varbase<-var(grp24_base)
vartest<-var(grp24_test)
spooled<-sqrt((.5*varbase)+(.5*vartest))
spooled
(dhat <- ( mean(grp24_test) - mean(grp24_base) ) / spooled)
(dzhat<-( mean(grp24_test) - mean(grp24_base) ) / sd(grp24_test - grp24_base))


par(mfrow=c(1,1))
boxplot((grp24_test-grp24_base),ylab="EtOH consumption (g/kg)",xlab="Post-stress - Pre-stress  ", col=c('cyan4'))
hist((grp24_test-grp24_base), main="Difference in Drinking Histogram", xlab="Avg EtOH Consumption (g/kg)", ylab="Density", xlim=c(-2,2), ylim=c(0,5),breaks=c(-1,-.5,0,.5,1,1.5,2) ,col = 'cyan4')


