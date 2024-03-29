####################################################### IMPORT DATA
library(readxl)
Long <- read_excel("H:/_2022_drexel_Classes/Stat_and_R/Diet_study_V2.xlsx")
View(Long)


####################################################### DEPENDENT-SAMPLE TESTS
library(dplyr)
Male<-filter(Long, Gender=="Male" & (Time == "Baseline" | Time == "1 month"))
library(tidyr)
Dep<-select(Male,ID,Time, Triglyceride)
View(Dep)

############################################# Check normality
#################################### Graphical
library(ggplot2)
Dep$Time<-factor(Dep$Time,levels=c("Baseline","1 month"))
(g<-ggplot(Dep,aes(x=Time, y=Triglyceride))+stat_boxplot(geom = "errorbar")+
    geom_boxplot()+labs(x = "", y = "Triglyceride (mg/dL)"))
windows(20,10)
(g1<-g+ scale_y_continuous(breaks=seq(90,190,10))+
    theme(text = element_text(size=15)))

#################################### Numerical
install.packages("psych")
library(psych)
Norm3<-Dep %>% group_by(Time) %>% summarise("Sample size"=n(),Mean = round(mean(Triglyceride),1), 
                                            Median = round(median(Triglyceride),1), Skewness=round(skew(Triglyceride,type=2),2),
                                            "Normally distributed"=ifelse(
                                              shapiro.test(Triglyceride)$p.value>0.05,
                                              paste0("Yes (p = ",round(shapiro.test(Triglyceride)$p.value,4),")"),
                                              paste0("No (p = ",round(shapiro.test(Triglyceride)$p.value,4),")")))
t(Norm3)

############################################# Paired t-test
(res3.1<-t.test(Dep$Triglyceride~Dep$Time,conf.level = 0.95,paired=T))

############################################# Effect size
library(rstatix)
(effect3.1<-cohens_d(Dep,Triglyceride~Time,paired=T))
(eff3.1<-round(effect3.1$effsize,2))
(effect3.1_class<-ifelse(abs(eff3.1)<0.2,paste0("Negligible (d = ",eff3.1,")"),
                         ifelse(abs(eff3.1)<0.5,paste0("Small (d = ",eff3.1,")"),
                                ifelse(abs(eff3.1)<0.8,paste0("Moderate (d = ",eff3.1,")"),
                                       paste0("Large (d = ",eff3.1,")")))))

############################################# Sample presentation of test results
Labels3.1<-c("Mean difference","95% CI (lower)","95% CI (upper)","p-value","Effect size")
Stats3.1<-c(round(res3.1$estimate,1),round(res3.1$conf.int[1],2),
            round(res3.1$conf.int[2],2),round(res3.1$p.value,3),effect3.1_class)
(Output3.1<-data.frame(Labels3.1,Stats3.1))

############################################# Power Analysis
# https://cran.r-project.org/web/packages/pwr/vignettes/pwr-vignette.html
#################################### Part I
library(pwr)
(res<-pwr.t.test(d=effect3.1$effsize,sig.level=0.05,power=0.8,type="paired",alternative="two.sided"))
ceiling(res$n)

#################################### Part II
# plot 01
power<-c()
a<-0.6
b<-0.95
inc<-0.01
n<-(b-a)/inc

size<-c()

for (i in 1:(n+1))
{
  power[i]<-a+(i-1)*inc
  res<-pwr.t.test(d=effect3.1$effsize,sig.level=0.05,power=power[i],type="paired",alternative="two.sided")
  size[i]<-res$n
}
(result1<-data.frame(power,size))
View(result1)

(plot01<-ggplot(result1, aes(x=power, y=size)) + geom_point(color="red",size=2)+geom_line(color="red",size=1))
windows(20,10)
plot01+labs(x = expression("Power (1-"*beta*")"), y = "Total sample size")+
  coord_cartesian(ylim=c(12,30))+scale_y_continuous(breaks=seq(12,30,2))+
  scale_x_continuous(breaks=seq(0.6,0.95,0.05))+theme(text = element_text(size=15))

#################################### Part III
# plot 02
power<-c()
a<-0.6
b<-0.95
inc<-0.01
n<-(b-a)/inc

eff<-c()
c<-0.65
d<-0.71
incr<-0.03
m<-(d-c)/incr

size<-matrix(,n+1,m+2)
dim(size)

for (j in 1:(m+1))
{
  eff[j]<-c+(j-1)*incr
  
  for (i in 1:(n+1))
  {
    power[i]<-a+(i-1)*inc
    res<-pwr.t.test(d=eff[j],sig.level=0.05,power=power[i],type="paired",alternative="two.sided")
    size[i,j]<-res$n
  }
}
(result2<-data.frame(power,size))
names(result2)<-c("Power","0.65","0.68","0.71")
df<-gather(result2,"Effect size","Size",2:4)

(plot02<-ggplot(df, aes(x=Power, y=Size,fill=`Effect size`)) + geom_point(aes(color=`Effect size`),size=2)+
    geom_line(aes(color=`Effect size`),size=1))
windows(20,10)
plot02+labs(x = expression("Power (1-"*beta*")"), y = "Total sample size")+
  coord_cartesian(ylim=c(11,33))+scale_y_continuous(breaks=seq(11,33,2))+
  scale_x_continuous(breaks=seq(0.6,0.95,0.05))+theme(text = element_text(size=15))