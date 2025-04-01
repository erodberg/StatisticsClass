
#####
data<-read.csv("C:/Users/ellen/Documents/School/stats641/proj3/data.csv")
summary(lm(DRINK~PI+sexID, data=data))
lm.proj<-(lm(DRINK~PI+sexID, data=data))
#plot(lm.proj)

data.f<-data[data$sexID==1, c('DRINK', 'PI', 'sexID')]
data.m<-data[data$sexID==0, c('DRINK', 'PI', 'sexID')]
plot(data.f)
plot(data.m)

# Men
plot(data[data$sexID==0, 'PI'], data[data$sexID==0, 'DRINK'], col='blue',  xlim=c(150,280), ylim=c(1.2,3.0))
lm.m_age <- lm(DRINK~PI, data[data$sex==0,])
abline(lm.m_age, col='blue')
summary(lm.m_age) 

# Women
plot(data[data$sexID==1, 'PI'], data[data$sexID==1, 'DRINK'], col='red',  xlim=c(150,280), ylim=c(1.2,3.0))
lm.w_age <- lm(DRINK~PI, data[data$sex==1,])
abline(lm.w_age, col='red')
summary(lm.w_age)

plot(c(data.f$PI,data.m$PI), c(data.f$DRINK, data.m$DRINK), 
          col = c("hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3"), 
          xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 18,
          xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.w_age, col='hotpink3', lwd = 2)
abline(lm.m_age, col='skyblue3',lwd = 2)
abline(x, lwd=2)

boxplot(DRINK~sexID, data=data, col=c("skyblue3","hotpink3"))
summary(lm.proj)



plot(c(data$PI), c(data$DRINK),
     col = c("hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3"), 
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 16,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
x<-(lm(DRINK~PI, data=data))
abline(x, lwd=2)
summary(x)

scatterplot3d(c(data.f$PI,data.m$PI), c(data.f$DRINK, data.m$DRINK), 
     col = c("hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","hotpink3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3","skyblue3"), 
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 18,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.proj)

library("scatterplot3d")
s3d<- scatterplot3d(data$PI, data$DRINK, data$sexID,
                     pch = 16)
s3d$plane3d(lm.proj) 

abline(lm.proj)


s3d <- scatterplot3d(trees, type = "h", color = "blue",
                     angle=55, pch = 16)
# Add regression plane
my.lm <- lm(trees$Volume ~ trees$Girth + trees$Height)
s3d$plane3d(my.lm)
# Add supplementary points
s3d$points3d(seq(10, 20, 2), seq(85, 60, -5), seq(60, 10, -10),
             col = "red", type = "h", pch = 8)
################################################################################################
lm.int <-lm(DRINK~PI*sexID, data=data)
summary(lm.int)
par(mfrow=c(1,3))  
avPlots(lm.int)
avPlot(lm.proj)
#########################
#partial f test
# Compare the model using sex and pi to the model using only pi
lm.pi <- lm(DRINK~PI, data=data)
# Compare the model using sex and pi to the model using only sex
lm.sex <- lm(DRINK~sexID, data=data)


anova(lm.proj, lm.pi) #addition of the extra parameters (sex) improves the fit of the model, bc sig)
anova(lm.proj, lm.sex)
anova(lm.proj, lm.int)


plot(c(data$PI), c(data$DRINK),
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 16,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.pi, lwd=2)

plot(c(data$PI), c(data$DRINK),
     xlim=c(0,280), ylim=c(1.2,3.0), cex=2, pch = 16,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.sex, lwd=2)

plot(c(data$PI), c(data$DRINK),
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 16,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.int, lwd=2)

#########################
summary(lm.proj, direction='both')
#########################

confint(lm.proj)

################################################################################################
cor(data.f$DRINK,data.f$PI)
cor.test(data.f$DRINK,data.f$PI)
cor(data.m$DRINK,data.m$PI)
cor.test(data.m$DRINK,data.m$PI)
cor(data$DRINK, data$PI)
cor.test(data$PI, data$DRINK)

x<-(lm(DRINK~PI, data=data))

par(mfrow=c(1,3))  
plot(c(data.f$PI), c(data.f$DRINK), 
     col = c("hotpink3"), 
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 18,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.w_age, col='hotpink3', lwd = 2)

plot(c(data.m$PI), c( data.m$DRINK), 
     col = c("skyblue3"), 
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 18,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(lm.m_age, col='skyblue3',lwd = 2)

plot(c(data$PI), c(data$DRINK), 
     col = c("black"), 
     xlim=c(150,280), ylim=c(1.2,3.0), cex=2, pch = 18,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")
abline(x, lwd=2)

#########
# Predicting conditional mean -------------------
x.f <- data.frame(cbind(PI=300, sexID=1))
x.f.c<-predict(lm.proj, x.f, interval="confidence")  
x.f.p<-predict(lm.proj, x.f, interval="prediction") 

y.f <- data.frame(cbind(PI=100, sexID=1))
y.f.c<-predict(lm.proj, y.f, interval="confidence")  
y.f.p<-predict(lm.proj, y.f, interval="prediction")

x.m <- data.frame(cbind(PI=300, sexID=0))
x.m.c<-predict(lm.proj, x.m, interval="confidence")  
x.m.p<-predict(lm.proj, x.m, interval="prediction") 

y.m <- data.frame(cbind(PI=100, sexID=0))
y.m.c<-predict(lm.proj, y.m, interval="confidence")  
y.m.p<-predict(lm.proj, y.m, interval="prediction")

c.bar<-c(2.937247 ,2.159783, 1.828023,1.050559  )
c.low<-c(2.204177, 1.637993 ,0.6614499 ,-0.355604 )
c.up<-c( 3.670317,2.681574,2.994596,2.456723)

p.bar<- c(2.937247 ,2.159783 , 1.828023 , 1.050559 )
p.low<- c(1.830364 ,  1.179955 , 0.3966983 , -0.5819519 )
p.up<- c(4.04413, 3.139612, 3.259347, 2.683071 )

plot(c(299,301,99,101),c(2.937247 ,2.159783, 1.828023,1.050559  ), col=c("red","darkblue","pink","lightblue"), cex=2, pch = 18, xlab=" PI",ylab="Confidence Ethanol Consumption", ylim=c(-1,4.55) )
arrows(x0=c(299,301,99,101),y0=c(2.937247 ,2.159783, 1.828023,1.050559  ),x1=c(299,301,99,101),
       y1=c(2.204177, 1.637993 ,0.6614499 ,-0.355604 ),length = 0.01)
arrows(x0=c(299,301,99,101),y0=c(2.937247 ,2.159783, 1.828023,1.050559  ),x1=c(299,301,99,101),
       y1=c( 3.670317,2.681574,2.994596,2.456723),length = 0.01)

c.bar-c.low
c.up-c.bar
p.bar-p.low
p.up-p.bar

plot(c(299,301,99,101),p.bar, col=c("red","darkblue","pink","lightblue"), cex=2, pch = 18, xlab=" PI",ylab="Predicted Ethanol Consumption", ylim=c(-1,4.55) )
arrows(x0=c(299,301,99,101),y0=p.bar,x1=c(299,301,99,101),
       y1=p.low,length = 0.01)
arrows(x0=c(299,301,99,101),y0=p.bar,x1=c(299,301,99,101),
       y1=p.up,length = 0.01)

###############################################################################################
# Trend analysis


data$PI2 <- data$PI^2
data$PI3 <- data$PI^3
data$PI4 <- data$PI^4

anova(lm(DRINK ~ PI, data=data))
SST<-0.0954 +4.4890 
dferr<-17        
MSerr<-0.264059               

lm.trend <- lm(DRINK ~ PI, data=data)
summary(lm.trend)
R2 <- 0.0208
SS <- R2*SST
df <- 1
MS <- SS/df
F <- MS/MSerr
(pf(F, df, dferr, lower=FALSE))
anova(lm.trend)
# p 0.5558163
#not linear

lm.trend <- lm(DRINK ~ PI + PI2, data=data)
summary(lm.trend)
R2.2 <- 0.2999
SS.2 <- (R2.2-R2)*SST
df <- 1
MS.2 <- SS.2/df
F.2 <- MS.2/MSerr
(pf(F.2, df, dferr, lower=FALSE))
#p 0.04182113
#yes quadratic
anova(lm.trend)

lm.trend <- lm(DRINK ~ PI + PI2 + PI3, data=data)
summary(lm.trend)
R2.3 <- 0.3054
SS.3 <- (R2.3-R2.2)*SST
df <- 1
MS.3 <- SS.3/df
F.3 <- MS.3/MSerr
(pf(F.3, df, dferr, lower=FALSE))
#p 0.7610691
#no cubic 
anova(lm.trend)

lm.trend <- lm(DRINK ~ PI, data=data)
plot(data$PI, data$DRINK)
abline(lm.trend)

lm.trend <- lm(DRINK ~ PI + PI2, data=data)
plot(data$PI2, data$DRINK)
abline(lm.trend)

lm.trend <- lm(DRINK ~ PI + PI2 + PI3, data=data)
plot(data$PI3, data$DRINK)
abline(lm.trend)





#############################3
# Checking Assumptions -------------------

#normality and homoscedasticity assumptions
par(mfrow=c(2,2))  
plot(lm.proj)

# Homoscedasticity -----------
library(car)
ncvTest(lm.proj)
par(mfrow=c(1,2))
plot(lm.proj, which = 3)
spreadLevelPlot(lm.proj)

# plot studentized residuals vs. fitted values
par(mfrow=c(1,1),pty= "s")  
plot(resid(lm.proj))
abline(h=0)
spreadLevelPlot(lm.proj)

# Normality --------------
library(nortest)
# Anderson-Darling test.
ad.test(residuals(lm.proj)) # On residuals
ad.test(data$DRINK) # On data
hist(residuals(lm.proj), ylim=c(0,5))
qqPlot(lm.proj)
plot(lm.proj, which = 2)

# Linearity -------------

# component + residual plot
# Attempts to show the relationship between a given independent variable and 
#  the response variable given that other independent variables are also in the model
# Partial residual plot.
crPlots(lm.proj)
plot(lm.proj, which = 1)

# The Harvey-Collier Test of Linearity
library(lmtest)
harvtest(lm.w_age)
harvtest(lm.m_age)


# Outliers ------------
# Bonferonni p-value for most extreme obs
outlierTest(lm.proj)
par(mfrow=c(1,1))  
plot(lm.proj, which=1)

# Leverage plots
lev <- hat(model.matrix(lm.proj))
plot(lev, xlab='Data point index', ylab='Leverage')
which(lev > (2*(2 + 1)/19))
data[5,]
#lev= potential to change slope, data point 5 may be a levereage point 

plot(c(data$PI), c(data$DRINK), 
     col = c("black","black","black","black","red","black","black","black","black","black","black","black","black","black","black","black","black","black","black" ), 
     xlim=c(150,280), ylim=c(1.2,3.0), cex=1.5, pch = 16,
     xlab="Performance Index", ylab="Avg EtOH Consumption (g/kg)")


# Influential Observations --------- high lev and far from reg line 
# Added variable plots
# In applied statistics, a partial regression plot attempts to show the effect of adding an additional variable to the model (given that one or more independent variables are already in the model). Partial regression plots are also referred to as added variable plots, adjusted variable plots, and individual coefficient plots.
# More useful for multivariate regression
avPlots(lm.proj) # or
leveragePlots(lm.proj) 

# Cook's D plot
cook <-  cooks.distance(lm.proj)
plot(cook, ylab="Cooks distances")

# Cook's distance
cutoff <- 4/(nrow(data)-2-1)# 4/(n-p-1)
cutoff
plot(lm.proj, which=4, cook.levels=cutoff)


# Test for Autocorrelated Errors ---------------
durbinWatsonTest(lm.proj)




#  Can we predict better with fewer variables?
# In backward elimination, the process starts
#   with the full set of predictors, dropping the one corresponding
#   to the smallest contribution, and continuing in that way.
# R's step function uses the AIC criterion; tc is the first
#   variable dropped from the model followed by height and then weight. 
# In viewing the results for the full and reduced models, there are
#   several noteworthy changes: (1) The reduced model has only two
#   of the original five variables. (2) bmi now makes a significant
#   contribution. (3) Estimates for the intercept and the regression
#   coefficient for bmi are clearly changed. (4) Despite dropping three 
#   predictors, the adjusted R2 values are very similar. 

# We can also do forward and both forward and backward versions
summary(step(lm.proj, direction='both'))
summary(step(lm.int, direction='both'))

##############################
#vvvvvvvvvvvvvvvvvvvvvvvvvvvvv

