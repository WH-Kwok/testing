# Ethiopia poultry_Model

rm(list = ls())
library(readxl)
poultryparameters <- read_excel("poultryparameters.xlsx")
View(poultryparameters)

##M <- 72     # tstep*M = life span of chicken
M <- as.numeric(poultryparameters[1,3])
tstep <- as.numeric(poultryparameters[2,3])
Noyears <- as.numeric(poultryparameters[4,3])

Notsteps <- Noyears*12   # No. of time steps to run  Months=Notsteps*tstep

CF <- as.numeric(poultryparameters[9,3])
CM <- as.numeric(poultryparameters[10,3])
PF <- as.numeric(poultryparameters[11,3])
PM <- as.numeric(poultryparameters[12,3])
VAXEFF <- as.numeric(poultryparameters[13,3])
EthDoll <- as.numeric(poultryparameters[44,3])
#M1=as.numeric(M)

Male  <- c(1:M)*0
beta <-c(1:M)*0
alfa<- c(1:M)*0
Egg  <- c(1:M)*0              # Egg = Eggs laid by each age group
SF <- c(1:M)*0                # SF = proportion of chickens sold at each age
MFage <- c(1:M)*0
MMaleage <- c(1:M)*0
DF <- c(1:M)*0                #DF = No. of female chickens that die each age
Fe <- c(1:M)*0
Hatch   <- c(1:M)*0
Eat_egg <- c(1:M)*0
DMale    <- c(1:M)*0
time_Real  <- 1:Notsteps
SMale   <- c(1:M)*0
Noyears <- 10
k1   <- c(1:M)*0          # infected rate
k2   <- c(1:M)*0          # recovery rate
k3  <- c(1:M)*0           # death rate
FeS  <- c(1:M)*0          # Susceptible Female
FeI  <- c(1:M)*0          # Infected Female
FeR  <- c(1:M)*0          # Recovered (Removed) Female
MaleS   <- c(1:M)*0       # Susceptible Male
MaleI   <- c(1:M)*0       # Infected Male
MaleR   <- c(1:M)*0       # Recovered (Removed) Male
VAX <- c(1:M)*0           # Vaccination 
VAXEFF  <- c(1:M)*0       # Vaccination effect
wgtNoFchicks  <- c(1:M)*0
wgtNoFsubadult   <-c(1:M)*0
wgtNoFadult  <- c(1:M)*0
wgtNoMalechicks  <- c(1:M)*0
wgtNoMalesubadult  <- c(1:M)*0
wgtNoMaleadult  <- c(1:M)*0
NoFems   <- c(1:Notsteps)*0
NoFchicks   <- c(1:Notsteps)*0
NoMalechicks   <- c(1:Notsteps)*0
NoFsubadult   <- c(1:Notsteps)*0
NoMalesubadult  <- c(1:Notsteps)*0
NoFadult  <- c(1:Notsteps)*0
NoMaleadult   <-c(1:Notsteps)*0
NoSFems   <- c(1:Notsteps)*0
NoMale   <- c(1:Notsteps)*0
NoSMale  <- c(1:Notsteps)*0
infected <- c(1:Notsteps)*0
Rvalue <- c(1:Notsteps)*0
neweggs <- c(1:Notsteps)*0

# calculating Gross Margin
Costs   <- c(1:Notsteps)*0
Profit  <-c(1:Notsteps)*0
GMargin   <- c(1:Notsteps)*0
GMyear <-1:Noyears


Fe[1:M] <- as.numeric(poultryparameters[15,3])
Male[1:M] <- as.numeric(poultryparameters[16,3])
alfa[1:M] <- as.numeric(poultryparameters[17,3])
beta[1:M]<- as.numeric(poultryparameters[18,3])
Egg[1:M] <- as.numeric(poultryparameters[19,3])
DF[1:M] <- as.numeric(poultryparameters[20,3])
SF[1:M] <- as.numeric(poultryparameters[21,3])
Hatch[1:M] <- as.numeric(poultryparameters[22,3])
Eat_egg[1:M] <- as.numeric(poultryparameters[23,3])
DMale[1:M] <- as.numeric(poultryparameters[24,3])
SMale[1:M] <- as.numeric(poultryparameters[25,3])
k1[1:M] <- as.numeric(poultryparameters[26,3])
k2[1:M] <- as.numeric(poultryparameters[27,3])
k3[1:M] <- as.numeric(poultryparameters[28,3])
FeS[1:M] <- as.numeric(poultryparameters[29,3])
FeI[1:M] <- as.numeric(poultryparameters[30,3])
FeR[1:M] <- as.numeric(poultryparameters[31,3])
MaleS[1:M] <- as.numeric(poultryparameters[32,3])
MaleI[1:M] <- as.numeric(poultryparameters[33,3])
MaleR[1:M] <- as.numeric(poultryparameters[34,3])
VAX[1:M] <- as.numeric(poultryparameters[35,3])
WFChicks <- as.numeric(poultryparameters[37,3])
WFsubadult <- as.numeric(poultryparameters[38,3])
WFadult <- as.numeric(poultryparameters[39,3])
WMaleChicks <- as.numeric(poultryparameters[40,3])
WMalesubadult <- as.numeric(poultryparameters[41,3])
WMaleadult <- as.numeric(poultryparameters[42,3])


# assuming that the vaccination effectiveness is the same for all age
 
 VAXEFF<-0.4    #vaccination effectiveness


Age <- 1:M


MFage[12:M] <- 8
MFage[1:11] <- 100000

MMaleage[12:M] <- 3
MMaleage[1:11] <- 100000

k1[1:M] <- 0.5e-5     # infected rate
k2[1:M]<-  5e-3        # recovery rate
k3[1:M] <- 7e-4        #death rate
Fe[1:M] <- 3
beta[1:M]<- 0.1


# DF = No. of female chickens that die each age
DF[1:M] <- 0

#Eat_egg[1:M] <- 0


FeS[1:20]<- 10
FeI[10:12]<- 3

MaleS[1:20]<- 5
MaleI[10:12]<- 1

for (i in 1:Notsteps){
  time_Real[i] <- i*tstep

# Eggs laid at rate alpha - only from ages 6 months onward - stop laying 18 months before "death"
  item <- M-18
  Egg[1:M] <- 0
  Egg[6:item] <- alfa[6:item]*Fe[6:item]

# No of eggs hatched - rate of hatching is beta
  Hatch[1:M] <- 0
  Hatch[6:item] <- beta[6:item]*Egg[6:item]

  ## No. of eggs sold or eaten (1-beta)
  
  Eat_egg <- 0
  Eat_egg <- sum(Egg)-sum(Hatch)   ##(1-beta[6:item])*Egg[6:item]
  Fe[1] <- sum(Hatch)/2

 # neweggs[i] <- Fe[1]

  Male[1] <- sum(Hatch)/2

 # Half females die or are sold if aged M-1
   DF[M-2] <- Fe[M-3]/2

   #half males die or sold if aged M-1
   DMale[M-2] <- Male[M-3]/2

 # Remainder die if aged M
  DF[M-1] <- Fe[M-1]
 # Fe[2:M] <- Fe[1:M-1]-DF[1:M-1]

  DMale[M-1] <- Male[M-1]
 # Male[2:M] <- Male[1:M-1]-DMale[1:M-1]
  
 
  # Vaccination part
  
  #if there is outbreak,it will vaccinate, otherwise it does not do any thing.
  
  # we are assuming we vaccinate all of the chicken that haven't got the disease, 
  # but only 60% effective and still 40% of the original number got the disease.
  
    if (sum(infected)>= 0.5*sum(FeS+MaleS))
    {
      VAX<- 1- VAXEFF
      }
     else{
      VAX<-1
     }
  
  #SIR compartment- Female
  
  #FeS[2:M]<-FeS[1:M-1]-k1[1:M-1]*sum(FeS)*sum(FeI)
  
  FeI[2:M]<-FeI[1:M-1]+k1[1:M-1]*VAX*sum(FeS+MaleS)*sum(FeI+MaleI)-k2[1:M-1]*sum(FeI)-k3[1:M-1]*sum(FeI)
  
  FeR[2:M]<-FeR[1:M-1]+k2[1:M-1]*sum(FeI)

  Fe[2:M]<-Fe[1:M-1]-k3[1:M-1]*sum(FeI)-DF[1:M-1]

  # total population OF Female
  FeS[2:M]<-Fe[2:M]-FeI[2:M]-FeR[2:M]
  
  
  # SIR Male

  MaleI[2:M]<-MaleI[1:M-1]+k1[1:M-1]*VAX*sum(MaleS+FeS)*sum(MaleI+FeI)-k2[1:M-1]*sum(MaleI)-k3[1:M-1]*sum(MaleI)
  
  MaleR[2:M]<-MaleR[1:M-1]+k2[1:M-1]*sum(MaleI)
  
  Male[2:M]<-Male[1:M-1]-k3[1:M-1]*sum(MaleI)-DMale[1:M-1]

  # total population of Male
  MaleS[2:M]<-Male[2:M]-MaleI[2:M]-MaleR[2:M]

  infected[i]<-sum(FeI)+sum(MaleI)
  Rvalue[i]=k1[1:M-1]*sum(MaleS+FeS)/(k2[1:M-1]+k3[1:M])
  
  # Female market
  
  if (i %% 3 == 0){
  for (k in 1:M){
  if (Fe[k] >= MFage[k]){
    SF[k] <- Fe[k]-MFage[k]
    Fe[k] <- MFage[k]
  } else {
    SF[k]==0
  }}}

 # Male market
  
if (i %% 3 == 0){
  for (k in 1:M){
    if (Male[k] >= MMaleage[k]){
      SMale[k] <- Male[k]-MMaleage[k]
      Male[k] <- MMaleage[k]
    } else {
      SMale[k]==0
    }}}

  # Check to prevent negative number of chickens
  
  for (k in 1:M){
    if (FeR[k] < 0) FeR[k] <- 0 }


  for (k in 1:M){
    if (FeI[k] < 0) FeI[k] <- 0 }


   for (k in 1:M){
     if (Fe[k] < 0) Fe[k] <- 0 }

   for (k in 1:M){
     if (Male[k] < 0) Male[k] <- 0 }

  for (k in 1:M){
    if (MaleR[k] < 0) MaleS[k] <- 0 }

  for (k in 1:M){
    if (MaleI[k] < 0) MaleI[k] <- 0 }

  # No. of females and males at each age group
  NoFems[i] <- sum(Fe)
  NoFchicks[i] <- sum(Fe[1:2])
  NoMalechicks[i] <- sum(Male[1:2])
  NoFsubadult[i] <- sum(Fe[3:6])
  NoMalesubadult[i] <- sum(Male[3:6])
  NoFadult[i] <- sum(Fe[7:M])
  NoMaleadult[i] <- sum(Male[7:M])
  NoSFems[i] <- sum(SF)
  NoMale[i] <- sum(Male)
  NoSMale[i] <- sum(SMale)

  #weight at different age group

  wgtNoFchicks[i] <- WFChicks*NoFchicks[i]
  wgtNoFsubadult[i] <- WFsubadult*NoFsubadult[i]
  wgtNoFadult[i] <- WFadult*NoFadult[i]
  wgtNoMalechicks[i] <- WMaleChicks*NoMalechicks[i]
  wgtNoMalesubadult[i] <- WMalesubadult*NoMalesubadult[i]
  wgtNoMaleadult[i] <- WMaleadult*NoMaleadult[i]

  # calculating Gross Margin
  Costs[i] <- sum(Fe*CF+Male*CM)
  Profit[i] <- sum(SF*PF+SMale*PM)
  GMargin[i] <- (Profit[i]-Costs[i])*EthDoll

# Update the number of each age - also  FeI, FeS, FeR, FeV (and males)
 Fe[2:M]<-Fe[1:M-1]
 FeS[2:M]<-FeS[1:M-1]
 FeI[2:M]<-FeI[1:M-1]
 FeR[2:M]<-FeR[1:M-1]
 Male[2:M]<-Male[1:M-1]
 MaleS[2:M]<-MaleS[1:M-1]
 MaleI[2:M]<-MaleI[1:M-1]
 MaleR[2:M]<-MaleR[1:M-1]
 Fe[1]<-0
 FeS[1]<-0
 FeI[1]<-0
 FeR[1]<-0
 Male[1]<-0
 MaleS[1]<-0
 MaleI[1]<-0
 MaleR[1]<-0
}

##  hold off
##  if (mod(i,12) <- 0){
##    fprintf() 'NoFemes.dat' year, F
##  }if){
##}){
##figure(2)
##xlabel('Time')
##ylabel('No. Eggs & No. Chickens')

#print -dpdf 'filename.pdf'
#Eggs_per_year <- sum(neweggs)/Notsteps*4

for (k in 1:Noyears){
strt <- (k-1)*12+1
fin <- (k-1)*12+12
GMyear[k] <- sum(GMargin[strt:fin])
}

par(mfrow=c(2,1))
plot(time_Real/12,NoFems,xlab='Time',ylab='No. Females',type='l')   ##,'b--','linewidth',2,time/12,NoFems,'r','linewidth',2,time/12,NoMale,'g','linewidth',2)
#plot(time_Real/12,NoMale,xlab='Time',ylab='No. Males',type='l')  ##
#plot(time_Real/12,neweggs,xlab='Time',ylab='No. Eggs',type='l')
plot(time_Real/12,infected,xlab='Time',ylab='Infected',type='l')
#plot(GMyear/1000,xlab='Year',ylab='Gross Margin (in 1000s)',type='b',col='blue')


