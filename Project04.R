
data<- read.csv('C:/Users/ellen/Documents/School/stats641/proj1/data.csv', header=TRUE)
head(data)

grandmean <- mean(data$ALC)
grandmean

n<-19
a<-4
b<-3

(df_Tot <- a*b*n -1) #227
(df_S <- a*n -1) #75
(df_A <- a-1) #3
(df_SA <- a*(n-1)) #72
(df_B <- b-1 ) #2
(df_AB <- (a-1)*(b-1)) #6
(df_SAB <- a*(n-1)*(b-1)) #144

SS_Total <- sum((data$ALC - grandmean)^2) #10.6164
# Subject SS
SS_S <- b*sum((tapply(data$ALC, list(data$SID), mean) - grandmean)^2)
SS_S # 4.627692
# B SS
SS_B <- n*a*sum((tapply(data$ALC, list(data$WK), mean) - grandmean)^2)
SS_B # 1.938563
# A SS
SS_A <- n*b*sum((tapply(data$ALC, list(data$TXT), mean) - grandmean)^2)
SS_A # 4.210205
#SA SS
SS_SA <- SS_S - SS_A 
SS_SA # 0.4174874
#SAB SS
SS_AB <- n*sum((tapply(data$ALC, list(data$WK,data$TXT), mean) - grandmean)^2) - SS_A - SS_B 
SS_AB #1.888291
#WS SS
SS_Ws <- SS_Total - SS_S 
SS_Ws #5.988705
#BSA SS
SS_BSA <- SS_Ws - SS_B - SS_AB 
SS_BSA #2.16185


(MS_A <- SS_A/df_A) #1.403402
(MS_B <- SS_B/df_B) #0.9692817
(MS_S <- SS_S/df_S) #0.06170257
(MS_SA <- SS_SA/df_SA) #0.06170257
(MS_AB <- SS_AB/df_AB) #0.3147152
(MS_BSA <- SS_BSA/df_SAB) #0.01501285

F1_A <- MS_A / (MS_AB + MS_SA - MS_BSA) #4.593774
F1_B <- MS_B/MS_BSA #64.56347
F1_AB <- MS_AB/MS_BSA #20.96306

pf(F1_B, df_B, df_SAB, lower.tail = FALSE)
pf(F1_AB,df_AB, df_SAB, lower.tail = FALSE)

data$SID <- as.factor(data$SID )
data$TXT  <- as.factor(data$TXT)
data$WK <- as.factor(data$WK)

library(ez)
aov.1 <- aov(ALC ~ (WK*TXT) + Error(SID / WK), data=data)
summary(aov.1)

aov.ez <- ezANOVA(data=data, 
                  dv=ALC, 
                  wid=SID, 
                  within=WK, 
                  between =TXT,
                  type=3)
print(aov.ez)


# We assume independence of the between subject factors.

shapiro.test(data[data$WK==1 & data$TXT ==1, 'ALC']) #0.3425
shapiro.test(data[data$WK==1 & data$TXT ==2, 'ALC']) #0.9829
shapiro.test(data[data$WK==1 & data$TXT ==3, 'ALC']) #0.3771
shapiro.test(data[data$WK==1 & data$TXT ==4, 'ALC']) #0.1571
shapiro.test(data[data$WK==2 & data$TXT ==1, 'ALC']) #0.2601
shapiro.test(data[data$WK==2 & data$TXT ==2, 'ALC']) #0.8386
shapiro.test(data[data$WK==2 & data$TXT ==3, 'ALC']) #0.8499
shapiro.test(data[data$WK==2 & data$TXT ==4, 'ALC']) #0.7527
shapiro.test(data[data$WK==3 & data$TXT ==1, 'ALC']) #0.7135
shapiro.test(data[data$WK==3 & data$TXT ==2, 'ALC']) #0.7631
shapiro.test(data[data$WK==3 & data$TXT ==3, 'ALC']) #0.3613
shapiro.test(data[data$WK==3 & data$TXT ==4, 'ALC']) #0.5714

# Normality is not violated.

par(mfrow=c(4,3),pty='m')
hist(data[data$WK==1 & data$TXT ==1, 'ALC'], main="Air/NS Week 1", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='steelblue1')
hist(data[data$WK==2 & data$TXT ==1, 'ALC'], main="Air/NS Week 2", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='steelblue3')
hist(data[data$WK==3 & data$TXT ==1, 'ALC'], main="Air/NS Week 3", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='steelblue4')
hist(data[data$WK==1 & data$TXT ==2, 'ALC'], main="Air/FSS Week 1", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='plum2')
hist(data[data$WK==2 & data$TXT ==2, 'ALC'], main="Air/FSS Week 2", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='plum3')
hist(data[data$WK==3 & data$TXT ==2, 'ALC'], main="Air/FSS Week 3", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='plum4')
hist(data[data$WK==1 & data$TXT ==3, 'ALC'], main="CIE/NS Week 1", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='orange2')
hist(data[data$WK==2 & data$TXT ==3, 'ALC'], main="CIE/NS Week 2", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='orange3')
hist(data[data$WK==3 & data$TXT ==3, 'ALC'], main="CIE/NS Week 3", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='orange4')
hist(data[data$WK==1 & data$TXT ==4, 'ALC'], main="CIE/FSS Week 1", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='brown1')
hist(data[data$WK==2 & data$TXT ==4, 'ALC'], main="CIE/FSS Week 2", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='brown3')
hist(data[data$WK==3 & data$TXT ==4, 'ALC'], main="CIE/FSS Week 3", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5),ylim=c(0,3), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='brown4')

# Sphericity
aov.ez$`Mauchly's Test for Sphericity`
#Effect         W         p p<.05
#3     WK 0.8043245 0.2177805      
#4 TXT:WK 0.8043245 0.2177805    
# Sphericity is not violated.

##############
par(mfrow=c(1,1),pty='m')
boxplot(ALC ~ WK, ylab='Avg EtOH Conspumtion(g/kg)', xlab='Week', names=c('1', '2','3'),col=c('lightgrey','darkgrey','grey20'), data=data)
boxplot(ALC ~ TXT, ylab='Avg EtOH Conspumtion(g/kg)', xlab='Treatment',names=c('Air/NS', 'Air/FSS','CIE/NS','CIE/FSS'),col=c('steelblue3','plum3','orange3','brown3'), data=data)
boxplot(ALC ~ WK*TXT, ylab='Avg EtOH Conspumtion(g/kg)', xlab='Treatment by Week ',col=c('steelblue1','steelblue3','steelblue4','plum2','plum3','plum4','orange2','orange3','orange4','brown1','brown3','brown4'), data=data)

x_bar <- tapply(data$ALC, list(data$WK, data$TXT), FUN=mean)
plot(x_bar[1,], ty='b', ylim=c(1,3), col='lightgrey', ylab='self esteem', xlab='Treatment', xaxt='n')
points(x_bar[2,], ty='b', ylim=c(1, 3), col='darkgrey')
points(x_bar[3,], ty='b', ylim=c(1, 3), col='grey20')
axis(side=1, at=1:4)
legend(x='topleft', legend=c('baseline', 'therapy'), pch=15, col=c('red', 'blue'), bty='n')
