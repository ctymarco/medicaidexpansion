setwd("J:/4274/Medicaid idea/R Medicaid")

library(readxl)
Medicaid <- read_excel("Medicaid2.xlsx")

Medicaid<- as.data.frame(Medicaid)
Medicaid<- cbind(Medicaid[,1:3],Medicaid[,5:26])

length(which(Medicaid[,2]==1)) # treat.1 = 1, treated between 2014 and 2015 (26)
length(which(Medicaid[,3]==1)) # treat.2 = 1, treated between 2015 and 2016 (3)

length(which(Medicaid[,2]+Medicaid[,3]==0)) # number of states that were not treated at all. (20)

length(Medicaid[,2]) # total of 49 states

#Y1 is Medicaid[,11]; Prevalence of diabetes
#Y2 is Medicaid[,12]; % of adults with reported poor health

#Robust SEs.
install.packages("sandwich")
library(sandwich)
install.packages("lmtest")
library(lmtest)

install.packages("stargazer");library(stargazer)




DID.D<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             
             Y1 + pov.perc + rGDP.cap + age + UE + disability + insured + hosps + 
             exercise + co2 + HDI + hos.days + SSI.perc + fat + HS.perc + uni.perc
           , data = Medicaid)

summary(DID.D)


########################################################################################


DID.D1<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             
             Y1 + pov.perc + rGDP.cap + age + UE + disability + insured + hosps + 
             exercise + hos.days + HS.perc + uni.perc
           , data = Medicaid)

summary(DID.D1)


########################################################################################

DID.D2<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              Y1 + age + disability + insured +  
              exercise + hos.days + uni.perc
            , data = Medicaid)

summary(DID.D2)


########################################################################################


DID.D<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             
             Y1 + pov.perc + rGDP.cap + age + UE + disability + insured + hosps + 
             exercise + co2 + HDI + hos.days + SSI.perc + fat + HS.perc + uni.perc
           , data = Medicaid)

summary(DID.D)

########################################################################################

DID.41<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             
            Y1 + age + disability + co2 + HDI
              
           , data = Medicaid)

summary(DID.41)

########################################################################################

DID.42<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              Y1 + age + disability + co2 + HDI +
              exercise 
            
            , data = Medicaid)

summary(DID.42)

########################################################################################

DID.43<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              Y1 + age + disability + co2 + HDI +
              exercise +
              pov.perc + insured + SSI.perc
            
            , data = Medicaid)

summary(DID.43)


########################################################################################

DID.44<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              Y1 + age + disability +  
              exercise +
              pov.perc + insured + SSI.perc + 
              hos.days
            
            , data = Medicaid)

summary(DID.44)


########################################################################################

DID.45<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              Y1 + age + disability +  
              exercise +
              pov.perc + insured + SSI.perc + 
              + hos.days + fat + crime + HS.perc
            
            , data = Medicaid)

summary(DID.45)


install.packages("sandwich")
library(sandwich)

Rse41<- sqrt(diag(vcovHC(DID.41, type="HC1"))) 
Rse42<- sqrt(diag(vcovHC(DID.42, type="HC1"))) 
Rse43<- sqrt(diag(vcovHC(DID.43, type="HC1"))) 
Rse44<- sqrt(diag(vcovHC(DID.44, type="HC1"))) 
Rse45<- sqrt(diag(vcovHC(DID.45, type="HC1"))) 


stargazer(DID.41,DID.42,DID.43,DID.44,DID.45
          , df = FALSE
          , column.sep.width = "0.1pt"
          #, type = "text"
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse41,Rse42,Rse43,Rse44,Rse45))

