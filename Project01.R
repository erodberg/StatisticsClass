LCdata <- read.csv('C:/Users/ellen/Documents/School/stats640/TrainingData_Project01.csv', header=TRUE) 
f_app_s1_corr <-LCdata[LCdata$sex=='f'&LCdata$program=='app'&LCdata$session=='1', c("percorr")] ##returns percent corr in females in Appetitive 
f_aver_s1_corr<-LCdata[LCdata$sex=='f'&LCdata$program=='aver'&LCdata$session=='1', c("percorr")] ##returns percent corr in females in aversive
m_app_s1_corr <-LCdata[LCdata$sex=='m'&LCdata$program=='app'&LCdata$session=='1', c("percorr")] ##returns percent corr in males in Appetitive
m_aver_s1_corr<-LCdata[LCdata$sex=='m'&LCdata$program=='aver'&LCdata$session=='1', c("percorr")] ##returns percent corr in males in aversive

totalmeans<-c((mean(f_app_s1_corr)),(mean(f_aver_s1_corr)),(mean(m_app_s1_corr)),(mean(m_aver_s1_corr)))
totalsd<-c((sd(f_app_s1_corr)),(sd(f_aver_s1_corr)),(sd(m_app_s1_corr)),(sd(m_aver_s1_corr)))
corrmed<-c((median(f_app_s1_corr)),(median(f_aver_s1_corr)),(median(m_app_s1_corr)),(median(m_aver_s1_corr)))
corrIQR<-c((IQR(f_app_s1_corr)),(IQR(f_aver_s1_corr)),(IQR(m_app_s1_corr)),(IQR(m_aver_s1_corr)))

par(mfrow=c(1,1))
totalplot<- barplot(totalmeans, ylim =c(0,1.2), main="Mean proportion correct 1st training session", ylab="Proportion correct (mean)", xlab="Sex and Trial Type", names.arg=(c("Female Appetitive","Female Aversive","Male Appetitive","Male Aversive")), col=c("lightpink3","pink","darkcyan","cyan3"))
segments(totalplot,totalmeans-totalsd,totalplot,totalmeans+totalsd)

boxplot(f_app_s1_corr,f_aver_s1_corr,m_app_s1_corr,m_aver_s1_corr,main="Proportion correct 1st training session",ylab="Proportion  correct",xlab="Sex and Trial Type", names=(c("Female Appetitive","Female Aversive","Male Appetitive","Male Aversive")), col=c("lightpink3","pink","darkcyan","cyan3"))

RTdata <- read.csv('C:/Users/ellen/Documents/School/stats640/RT_Project01.csv', header=TRUE) 
f_app_s1_RT<-RTdata[RTdata$sex=='f'&RTdata$program=='app'&RTdata$session=='1', c('RT')] ##returns s1 RT in females in Appetitive 
f_aver_s1_RT<-RTdata[RTdata$sex=='f'&RTdata$program=='aver'&RTdata$session=='1', c('RT')] ##returns s1 RT in females in aversive
m_app_s1_RT<-RTdata[RTdata$sex=='m'&RTdata$program=='app'&RTdata$session=='1', c('RT')] ##returns s1 RT in males in Appetitive
m_aver_s1_RT<-RTdata[RTdata$sex=='m'&RTdata$program=='aver'&RTdata$session=='1',c('RT')] ##returns s1 RT in males in aversive


##calculate the median and IQR of reaction times per session
med_f_app_s1<-median(f_app_s1_RT)
med_f_aver_s1<-median(f_aver_s1_RT)
med_m_app_s1<-median(m_app_s1_RT)
med_m_aver_s1<-median(m_aver_s1_RT)
totalmed<-c(med_f_app_s1,med_f_aver_s1,med_m_app_s1,med_m_aver_s1)

IQR_f_app_s1<-IQR(f_app_s1_RT)
IQR_f_aver_s1<-IQR(f_aver_s1_RT)
IQR_m_app_s1<-IQR(m_app_s1_RT)
IQR_m_aver_s1<-IQR(m_aver_s1_RT)
totalIQR<-c(IQR_f_app_s1,IQR_f_aver_s1,IQR_m_app_s1,IQR_m_aver_s1)

meanRT <- c(mean(f_app_s1_RT), mean(f_aver_s1_RT),mean(m_app_s1_RT),mean(m_aver_s1_RT))
sdRT <- c(sd(f_app_s1_RT), sd(f_aver_s1_RT),sd(m_app_s1_RT),sd(m_aver_s1_RT))

par(mfrow=c(1,1))
totalplotRT<- barplot(totalmed, ylim =c(0,30),  ylab="Reaction times (median)", xlab="Sex and Trial Type", names.arg=(c("Female Appetitive","Female Aversive","Male Appetitive","Male Aversive")), col=c("lightpink3","pink","darkcyan","cyan3"))
segments(totalplotRT,totalmed-totalIQR,totalplotRT,totalmed+totalIQR)

boxplot(f_app_s1_RT,f_aver_s1_RT,m_app_s1_RT,m_aver_s1_RT,ylab="Reaction times",xlab="Sex and Trial Type", names=(c("Female Appetitive","Female Aversive","Male Appetitive","Male Aversive")), col=c("lightpink3","pink","darkcyan","cyan3"))


par(mfrow=c(1,2))
##plot histograms of total reaction times appetitive
hist(f_app_s1_RT, ylim = c(0,150), xlim=c(0,20), col=c("lightpink3"),ylab="Density",xlab="Reaction times - female appetitive session")
hist(m_app_s1_RT, ylim = c(0,150), xlim=c(0,20),col=c("darkcyan"),ylab="Density",xlab="Reaction times - male appetitive session")


par(mfrow=c(1,2))
##plot histograms of total reaction times aversive
hist(f_aver_s1_RT, ylim = c(0,30), xlim=c(0,50),col=c("pink"),ylab="Density",xlab="Reaction times - female aversive session"  )
hist(m_aver_s1_RT, ylim = c(0,30), xlim=c(0,50),col=c("cyan3"),ylab="Density",xlab="Reaction times - male aversive session" )


##zscored animal 06 and 012 
app_corr_006<- LCdata[LCdata$program=='app'&LCdata$session=='1'&LCdata$ï..ID=='6', c("percorr")] ##returns percent corr for EMR20006- Appetitive 
aver_corr_006<- LCdata[LCdata$program=='aver'&LCdata$session=='1'&LCdata$ï..ID=='6', c("percorr")] ##returns percent corr for EMR20006- aversive
app_corr_012<- LCdata[LCdata$program=='app'&LCdata$session=='1'&LCdata$ï..ID=='12', c("percorr")] ##returns percent corr for EMR20006- Appetitive 
aver_corr_012<- LCdata[LCdata$program=='aver'&LCdata$session=='1'&LCdata$ï..ID=='12', c("percorr")] ##returns percent corr for EMR20006- aversive 


z_app_06<- ((app_corr_006-(mean(f_app_s1_corr)))/ (sd(f_app_s1_corr)))
z_aver_06<- ((aver_corr_006-(mean(f_aver_s1_corr)))/ (sd(f_aver_s1_corr)))
z_app_12<- ((app_corr_012-(mean(m_app_s1_corr)))/ (sd(m_app_s1_corr)))
z_aver_12<- ((aver_corr_012-(mean(m_aver_s1_corr)))/ (sd(m_aver_s1_corr)))


library(e1071)
kurtosis(f_app_s1_RT)
kurtosis(f_aver_s1_RT)
kurtosis(m_app_s1_RT)
kurtosis(m_aver_s1_RT)

skewness(f_app_s1_RT)
skewness(f_aver_s1_RT)
skewness(m_app_s1_RT)
skewness(m_aver_s1_RT)

