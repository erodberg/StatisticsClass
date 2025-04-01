library('tidyr')

drink<- read.csv('C:/Users/ellen/Documents/School/stats640/Project3/DrinkData_v2.csv', header=TRUE)
head(drink)

drink$overall<-as.factor(drink$overall)
drink$Tx<-as.factor(drink$Tx)
drink$stress<-as.factor(drink$stress)
drink$alc<-as.factor(drink$alc)
drink$stage<-as.factor(drink$stage)

txt1_base <-drink[drink$stress=='0' &drink$alc=='0' & drink$stage=="base",]
txt2_base <-drink[drink$stress=='1' &drink$alc=='0' & drink$stage=="base",]
txt3_base <-drink[drink$stress=='0' &drink$alc=='1' & drink$stage=="base",]
txt1_test <-drink[drink$stress=='0' &drink$alc=='0' & drink$stage=="test",]
txt2_test <-drink[drink$stress=='1' &drink$alc=='0' & drink$stage=="test",]
txt3_test <-drink[drink$stress=='0' &drink$alc=='1' & drink$stage=="test",]

shapiro.test(txt1_base$drink)
shapiro.test(txt2_base$drink)
shapiro.test(txt3_base$drink)
shapiro.test(txt1_test$drink)
shapiro.test(txt2_test$drink)
shapiro.test(txt3_test$drink)
#reject the null hypothesis that the data is not normal

par(mfrow=c(2,3),pty='m')
hist(txt1_base$drink, main="Air/NS Baseline", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='darkolivegreen2')
hist(txt2_base$drink,  main="Air/FSS Baseline", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='deepskyblue2')
hist(txt3_base$drink,  main="CIE/NS Baseline", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='darkorange2')
hist(txt1_test$drink,  main="Air/NS Test", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='darkolivegreen4')
hist(txt2_test$drink,  main="Air/FSS Test", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='deepskyblue4')
hist(txt3_test$drink,  main="CIE/NS Test", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='darkorange4')


par(mfrow=c(1,1),pty='m')
boxplot(drink$drink~drink$stage*drink$Tx, 
        col=c('darkolivegreen1','darkolivegreen4','deepskyblue1','deepskyblue4','darkorange1','darkorange4' ),
        xlab='Drinking Data by treatment group and stage',
        ylab="Average EtOH Consumption (g/kg)")
boxplot(drink$drink~drink$Tx*drink$stage,  
        col=c('darkolivegreen1','deepskyblue1','darkorange1','darkolivegreen4','deepskyblue4','darkorange4' ),
        xlab='Drinking Data by treatment group and stage',
        ylab="Average EtOH Consumption (g/kg)")
boxplot(drink$drink~drink$Tx, 
        col=c('darkolivegreen3','deepskyblue3','darkorange3'),
        xlab='Drinking Data by treatment group',
        ylab="Average EtOH Consumption (g/kg)")
boxplot(drink$drink~drink$stage,  
        col=c('gray80',"gray20"),
        xlab='Drinking Data by  stage',
        ylab="Average EtOH Consumption (g/kg)")

# independence  no the baseline and test drinking are performed in the same animal, within subjects 

library(e1071)
library('car')
#we can assume homogeneity of variance 
leveneTest(drink$drink~drink$Tx*drink$stage, center='mean')
#we can assume homogeneity of variance 


summary(aov(drink$drink~drink$Tx*drink$stage))


#contrasts
# All comparisons to control, i.e., the first condition
# Using multcomp
library(multcomp)
aov.btTxT <- aov(drink ~ overall- 1, data = drink)
summary(aov.btTxT)

# Rows are contrasts
# Columns are the coefficients for each level of the factor
K <- rbind("Control  test - Control base" = c(-1, 0, 0, 1,0,0),
           "Stress test - Stress base" = c(0,-1,0,0,1,0),
           "EtOH test - EtOH base" = c(0,0,-1,0,0,1),
           "Stress test - Control test" = c(0,0,0,-1,1,0),
           "EtOH test - Control test" = c(0,0,0,-1,0,1),
           "Stress + EtOH test - Control test " = c(0,0,0,-1,.5,.5)
           
           )
K

# Planned contrasts ----------------------------

# You can use the mcp (multiple comparison) function to specify contrasts
summary(glht(aov.btTxT, linfct = mcp(overall = K)))
glht.aov<- summary(glht(aov.btTxT, linfct = K))
plot(glht.aov)

############


#get SS and MS from ANOVA table
SSa<-0.687  
SSb<-0.356  
SSab<-0.840  
SSe<-6.047  
MSa<-0.3437   
MSb<-0.3562   
MSab<-0.4199   
MSe<-0.1440                 
dfA<-2
dfB<-1
dfAB<-1

etaA <- SSa/(SSa+SSe)
etaB<- SSb/(SSb+SSe)
etaAB<-SSab/(SSab+SSe)

#Omega squared
#(SS_effect - df_effect*MS_error)/(SS_effect + (N - df_effect)*MS_error)
omegaA<- (SSa-dfA*MSe)/(SSa+(48-dfA)*MSe)
omegaB<-(SSb-dfB*MSe)/(SSb+(48-dfB)*MSe)
omegaAB<-(SSab-dfAB*MSe)/(SSab+(48-dfAB)*MSe)







#SCRAPS
#grand_var<-var(drink$drink)
#A= treatment, B= stage 
#n<-8
#b<-2
#a<-3
#txt1_mean<-mean(c(txt1_base$drink,txt1_test$drink))
#txt2_mean<-mean(c(txt2_base$drink,txt2_test$drink))
#txt3_mean<-mean(c(txt3_base$drink,txt3_test$drink))
#stg1_mean<-mean(c(txt1_base$drink,txt2_base$drink,txt3_base$drink))
#stg2_mean<-mean(c(txt1_test$drink,txt2_test$drink,txt3_test$drink))#
#txt1_var<-var(c(txt1_base$drink,txt1_test$drink))
#txt2_var<-var(c(txt2_base$drink,txt2_test$drink))
#txt3_var<-var(c(txt3_base$drink,txt3_test$drink))
#stg1_var<-var(c(txt1_base$drink,txt2_base$drink,txt3_base$drink))
#stg2_var<-var(c(txt1_test$drink,txt2_test$drink,txt3_test$drink))#
#txt4_test <-drink[drink$stress=='1' &drink$alc=='1' & drink$stage=="test",]
#txt4_base <-drink[drink$stress=='1' &drink$alc=='1' & drink$stage=="base",]
#shapiro.test(txt4_test$drink)#shapiro.test(txt4_base$drink)
#hist(txt4_test$drink,  main="CIE/FSS Test", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5),col='firebrick4')
#hist(txt4_base$drink,  main="CIE/FSS Baseline", xlab="Avg EtOH Conspumtion(g/kg)", ylab="Density", xlim=c(1,3.5), breaks=c(1,1.25,1.5,1.75,2,2.25,2.5,2.75,3,3.25,3.5), col='firebrick2')
#boxplot(drink$drink~drink$alc, col=c('gray70',"gray30"))
#boxplot(drink$drink~drink$stress, col=c('gray70',"gray30"))
#boxplot(drink$drink~drink$stress*drink$stage)
#boxplot(drink$drink~drink$alc*drink$stage)
#boxplot(drink$drink~drink$alc*drink$stress)
#boxplot(drink$drink~drink$stage*drink$alc*drink$stress, col=c('darkolivegreen2','darkolivegreen4','deepskyblue2','deepskyblue4','darkorange2','darkorange4','firebrick2','firebrick4' ),
#        xlab='Drinking Data by treatment group and stage',
#        ylab="Average EtOH Consumption (g/kg)",
#        names=(c('Air/NS Base','Air/NS Test','Air/FSS Base','Air/FSS Test','CIE/NS Base','CIE/NS Test','CIE/FSS Base','CIE/FSS Test')),
#        )
# names=(c('Air/NS Base','Air/NS Test','Air/FSS Base','Air/FSS Test','CIE/NS Base','CIE/NS Test','CIE/FSS Base','CIE/FSS Test')),
#par(mfrow=c(2,2),pty='m')
#leveneTest(drink$drink~drink$stress*drink$alc*drink$stage, center='mean')
#summary(aov(drink$drink~drink$stress*drink$alc*drink$stage))
