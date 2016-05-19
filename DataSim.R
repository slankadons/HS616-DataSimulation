chemo<-data.frame()
selum<-data.frame()


#Creating columns C=Chemo, S=Selumetinib
ageC<-rnorm(n = 51,mean = 62,sd=10)
ageS<-rnorm(n=50,mean=62,sd=10)

M<-rep("M",times=31)
Fe<-rep("F",times=20)
SexC<-c(M,Fe)
M<-rep("M",times=26)
Fe<-rep("F",times=24)
SexS<-c(M,Fe)
sample(SexC)
sample(SexS)

PerfStatC<-sample(c(0,1),51,replace=TRUE)
PerfStatS<-sample(c(0,1),50,replace=TRUE)

M1ab<-rep("M1a/b",times=3)
M1c<-rep("M1c",times=48)
CancerStagePercentC<-c(M1ab,M1c)
sample(CancerStagePercentC)

M1ab<-rep("M1a/b",times=2)
M1c<-rep("M1c",times=50)
CancerStagePercentS<-c(M1ab,M1c)
sample(CancerStagePercentS)

SystemicTherapiesC<-sample(c(0,1,2),51,replace=TRUE)
SysteicTherapiesS<-sample(c(0,1,2),50,replace=TRUE)

LiverTherapiesC<-sample(c(0,1,2),51,replace=TRUE)
LiverTherapiesS<-sample(c(0,1,2),50,replace=TRUE)

GNAQC<-rbinom(51,1,0.4)
GNA11C<-rbinom(51,1,0.5)
WildC<-rbinom(51,1,.14)

GNAQS<-rbinom(50,1,0.4)
GNA11S<-rbinom(50,1,0.4)
WildS<-rbinom(50,1,0.18)
  

AnemiaC<- rbinom(51,1,0.16)
LeukopeniaC<- rbinom(51,1,0.18)
LymphopeniaC<-rbinom(51,1,0.08)
NeutropeniaC<-rbinom(51,1,0.08)
ThrombocytopeniaC<-rbinom(51,8,0.16)
  

AnemiaS<-rbinom(50,1,0.16)
LeukopeniaS<-rbinom(50,1,0.18)
LymphopeniaS<-rbinom(50,1,0.08)
NeutropeniaS<-rbinom(50,1,0.08)
ThrombocytopeniaS<- rbinom(50,1,0.16)



##Nausea and Vomiting related
#Age and Anorexia related

logistic <- function(t) 1 / (1 + exp(-t))


FatigueC<-rbinom(51,1,0.44)
FatigueS<-rbinom(50,1,0.6)

NauseaC<-rbinom(51,1,0.4)
NauseaS<-rbinom(50,1,0.6)

VomitingC<-rep(0,51)
#Insert a 1 in all the index values where Nausea is a 1, with a probability of getting a 1 as 0.6
VomitingC[NauseaC==1]<-rbinom(count(VomitingC[NauseaC==1]),1,0.6)

VomitingS<-rep(0,50)
VomitingS[NauseaS==1]<-rbinom(count(VomitingS[NauseaS==1]),1,0.6)

ConstipationC<-rbinom(51,1,0.3)
ConstipationS<-rbinom(50,1,0.06)

#Chemo
n <- 51
beta0 <- -1.6
beta1 <- 0.03


pi_x <- exp(beta0 + beta1 * ageC) / (1 + exp(beta0 + beta1 * ageC))
AnorexiaC <- rbinom(n=length(ageC), size=1, prob=pi_x)

#selemutinib

n <- 50
beta0 <- -1.6
beta1 <- 0.03


pi_x <- exp(beta0 + beta1 * ageS) / (1 + exp(beta0 + beta1 * ageS))
AnorexiaS <- rbinom(n=length(ageS), size=1, prob=pi_x)

chemo<-data.frame()
selum<-data.frame()

          
