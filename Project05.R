data<- read.csv('C:/Users/ellen/Documents/School/stats641/proj2/data.csv', header=TRUE)
head(data)

cov(data$PI, data$DRINK)
cor(data$PI, data$DRINK)
cor.test(data$PI, data$DRINK)
plot(data$PI, data$DRINK, xlab='Performance Index', ylab='Average EtOH consumption (g/kg)')

lm.1 <- lm(data$DRINK~ data$PI)
b1 <- cor(data$PI, data$DRINK)*sd(data$DRINK)/sd(data$PI); b1
b0 <- mean(data$DRINK) - b1*mean(data$PI); b0


# Residual plots -  y-hat v resid 
plot(lm.1$fitted, lm.1$residual) # Residual plot
abline( b0,  b1, col = "red")

#########regression
plot(data$PI, data$DRINK, xlab='Performance Index', ylab='Average EtOH consumption (g/kg)')
abline(lm.1, col='red')

lm.1


###ASSUMPTIONS
# linearity
library(lmtest)
harvtest(DRINK ~ PI, data = data)
#p-value = 0.004825
# Linearity is violated. 
# Multiple regression with a quadratic term could be used. Variable transformation can be used if needed.
#violation of linearity - fail to reject null tht the data are linear
# what we could do - transform so that data is linear (for example sqrt)

#linearity plot
par(mfrow=c(1, 2), pty='s')
drink_sq <- (data$DRINK-mean(data$DRINK))^2
plot(data$PI, drink_sq) # Violates linearity
lm.sq <- lm(drink_sq ~ data$PI)
abline(lm.sq)
plot(lm.sq$fitted, lm.sq$residual) # Residual plot
abline(0, 0)
par(mfrow=c(1,1))

# equal variance - Homoscedasticity
lm <- lm(DRINK ~ PI, data = data)
library(car)
ncvTest(lm)
#p = 0.62855
# Equal variance is not violated.

# Homoscedasticity plot
par(mfrow=c(1, 2), pty='s')
drink_diff <- data$DRINK + sd(data$DRINK)*rnorm(19)*data$PI
plot(data$PI, drink_diff) #
lm.diff <- lm(drink_diff ~ data$PI)
abline(lm.diff)
plot(lm.diff$fitted, lm.diff$residual) # Residual plot
abline(0, 0)
par(mfrow=c(1,1))

# normality
library(nortest)
ad.test(lm$residuals)
#p-value = 0.8115
# Normality is not violated.
qqPlot(lm.1, main="QQ Plot") 

# Normality of residuals - plot
hist(lm.1$residuals)
qqnorm(lm.1$residuals)
hist(lm.sq$residuals)
qqnorm(lm.sq$residuals)


# independence
durbinWatsonTest(lm)
# p = 0.396
#independance is not violated

######OUTLIERS
###########################ALL#############
# Studentized residuals 
par(mfrow=c(1, 2), pty='s')
sResids_ext = rstudent(lm.1) # External (I think) studentized residuals
plot(sResids_ext)
abline(0, 0)

# Comparing studentized residuals to normal distribution
hist(sResids_ext, freq=FALSE, main="Distribution of Studentized Residuals", ylim=c(0, .6))
xfit<-seq(min(sResids_ext), max(sResids_ext), length=50)
yfit<-dnorm(xfit)
lines(xfit, yfit) 
par(mfrow=c(1,1))

# Outlier test based on studentized residuals, Bonferroni corrected
outlierTest(lm.1) 
#no residual outlier 

# Marking the outlier
plot(data$DRINK ~ data$PI)
abline(b0,b1)
points(data$DRINK[5], col='red', cex=2)
qqPlot(lm.1, main="QQ Plot")  # qqplot for studentized residuals

# Leverage - predictor outlier
lev <- hatvalues(lm.1)
plot(lev, xlab='Data point index', ylab='Leverage')

# Hoaglin & Welsch (1978) cutoff
2*(1 + 1)/19 # Cutoff 2(p+1)/N
lev_outliers <- which(lev > 2*(1 + 1)/length(lev))
lev_outliers # No outliers here

cutoff <- 4/((nrow(data)-length(lm.1$coefficients)-1)) # 4/(n-p-1)
cutoff
plot(lm.1, which=4, cook.levels=cutoff)
abline(cutoff,0)
#row 3,5,15 are influential points
#row 44 is both an outlier and an influential point and may be distorting our data. If it can be explained by something like experimenter/equiptment error it can be disregaured. If not we may want to normalize the data

cooks_dist <- cooks.distance(lm.1)
plot(cooks_dist, ylab='Cooks distance')
cooks_dist[cooks_dist > cutoff]
#influencePlot(lm.1, id=list(method="identify"), main="Influence Plot", sub="Circle size is proportial to Cook's Distance" )


##########partial correlation
library(ppcor)
pcor.test(data$PI, data$DRINK, data$MAX)
par(mfrow=c(1, 3), pty='s')
plot(data$PI, data$DRINK, pch=20, cex=1.5)
plot(data$MAX, data$PI, pch=20, cex=1.5)
plot(data$MAX, data$DRINK, pch=20, cex=1.5)


##############
summary(lm.1)
#inference for intercept 
# is intercept (2.901429)  different than 0
N<-19
SE_b0 <- 0.5139  * sqrt(1/N + mean(data$PI)^2/sum((data$PI - mean(data$PI))^2)) 
SE_b0
2.901429/SE_b0
2*pt(2.901429/SE_b0, 17 , lower.tail=FALSE)
confint(lm.1)
#reject the null hypothesis that intercept is different than 0

#inference for slope
# is slope (4.560) different from 0
SE_b1 <- 0.5139 / sqrt(sum((data$PI - mean(data$PI))^2))
SE_b1
0.002862312/SE_b1
2*pt(0.002862312/SE_b1, 48, lower.tail=FALSE)
confint(lm.1)
#fail to reject the null hypothesis that slope is different than 0

summary(lm.1)
###########independent slopes
f_PI<- data$PI[1:10]
m_PI<-data$PI[11:19]
f_DRINK<- data$DRINK[1:10]
m_DRINK<-data$DRINK[11:19]
cor(f_PI,f_DRINK)
cor(m_PI,m_DRINK)

b1f <- cor(f_PI, f_DRINK)*sd(f_DRINK)/sd(f_PI); b1f
b1m <- cor(m_PI, m_DRINK)*sd(m_DRINK)/sd(m_PI); b1m
SE_f <- sd(f_PI)/sqrt(10)
SE_m <- sd(m_PI)/sqrt(9)
ssx_f<-sum((f_PI- mean(f_PI))^2);ssx_f
ssx_m<-sum((m_PI - mean(m_PI))^2);ssx_m
s2.yx <- (8 * SE_f^2 + 7 * SE_m^2) / (8 + 7)
s2.yx
s.yx <- sqrt(s2.yx)
s.yx
t <- (b1f - b1m) / ( s.yx * sqrt(1/ssx_f + 1/ssx_m) )
t
2*pt(t, 15, lower.tail=FALSE)

N_f<-10
N_m<-9
se<-sqrt(((ssx_f)*(10/19))+((ssx_m)*(9/19)))
t<-(b1f-b1m)/(se*sqrt((1/ssx_f)+(1/ssx_m)))
2*pt(t, 15, lower.tail=FALSE)


b01 <- mean(f_DRINK) - b1f*mean(f_DRINK); b01
b02 <- mean(m_DRINK) - b1f*mean(m_DRINK); b02

lm.11 <- lm(f_DRINK~ f_PI)
summary(lm.11) #0.3655 , 8df, 
f_r2<- 0.1644
lm.12 <- lm(m_DRINK~ m_PI)
summary(lm.12) #0.4428  , 7df.
m_r2<- 0.03661

syx<-sqrt((1/0.3655)+(1/0.4428))
se2<-(((1-f_r2)*ssx)+((1-m_r2)*ssy))/(10+9-4)
se<-sqrt(se2)
t<-(b11-b12)/(se*sqrt((1/ssx)+(1/ssy)))
2*pt(t, 15, lower.tail=FALSE)

par(mfrow=c(1, 1), pty='s')
plot(c(f_PI,m_PI),c(f_DRINK,m_DRINK), cex=1.5, pch=21, bg=c("lightpink","lightblue"),xlab='Performance Index', ylab='Average EtOH consumption (g/kg)', xlim=c(150,275), ylim=c(0,4))
abline( lm.11, col = "pink")
abline( lm.12,  b1m, col = "blue")
legend('topleft', legend=c("Female", "Male"),
       col=c("lightpink", "lightblue"), pch=20, cex=1.5)




