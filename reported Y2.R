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

#####################################################################

DID.2<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              pov.perc + rGDP.cap + age + UE + disability + insured + hosps + 
              exercise + co2 + HDI + hos.days + SSI.perc + fat + HS.perc + uni.perc
            , data = Medicaid)

summary(DID.2)

#####################################################################

DID.21<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             rGDP.cap + pov.perc + co2 + HDI
           , data = Medicaid)

summary(DID.21)

#####################################################################

DID.22<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              pov.perc + 
              age + UE + hosps + HS.perc + uni.perc
            , data = Medicaid)

summary(DID.22)

#####################################################################

DID.23<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              pov.perc + HS.perc +
              age + UE + disability
              
            , data = Medicaid)

summary(DID.23)

#####################################################################

DID.24<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
                
              disability + age + HS.perc +
               
              SSI.perc + exercise + UE
            
            , data = Medicaid)

summary(DID.24)



#####################################################################

DID.25<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             age + UE + disability  + exercise + 
             crime + exercise + HS.perc
           , data = Medicaid)

summary(DID.25)

#####################################################################

DID.26<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              age + UE + disability + exercise + 
              SSI.perc 
            , data = Medicaid)

summary(DID.26)

#####################################################################

DID.27<- lm(Y2 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              age + UE + disability + exercise + 
              HS.perc 
            , data = Medicaid)

summary(DID.27)

install.packages("sandwich")
library(sandwich)

Rse21<- sqrt(diag(vcovHC(DID.21, type="HC1"))) 
Rse22<- sqrt(diag(vcovHC(DID.22, type="HC1"))) 
Rse23<- sqrt(diag(vcovHC(DID.23, type="HC1"))) 
Rse24<- sqrt(diag(vcovHC(DID.24, type="HC1"))) 
Rse25<- sqrt(diag(vcovHC(DID.25, type="HC1"))) 
Rse26<- sqrt(diag(vcovHC(DID.26, type="HC1"))) 
Rse27<- sqrt(diag(vcovHC(DID.27, type="HC1")))

Rse28<- sqrt(diag(vcovHC(DID.28, type="HC1")))
Rse29<- sqrt(diag(vcovHC(DID.29, type="HC1")))

stargazer(DID.21,DID.22,DID.23,DID.24,DID.25,DID.26
          , df = FALSE
          , column.sep.width = "0.1pt"
          , type="text"
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse21,Rse22,Rse23,Rse24,Rse25,Rse26))



stargazer(DID.21,DID.22,DID.23,DID.24,DID.25,DID.27
          , df = FALSE
          , column.sep.width = "0.1pt"
          
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse21,Rse22,Rse23,Rse24,Rse25,Rse27))










stargazer(DID.21,DID.22,DID.23,DID.24,DID.25,DID.26
          , df = FALSE
          , column.sep.width = "0.1pt"
          , type="text"
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse21,Rse22,Rse23,Rse24,Rse25,Rse26))

#different significantces with robust SE
stargazer(DID.21,DID.22,DID.23,DID.24,DID.25,DID.26
          , df = FALSE
          , column.sep.width = "0.1pt"
          , type="text"
          , single.row = FALSE
          , font.size = "small"
          )
