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



DID.DD<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
             treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
             
             pov.perc + rGDP.cap + age + UE + disability + insured + hosps + 
             exercise + co2 + HDI + hos.days + SSI.perc + fat + HS.perc + uni.perc
           , data = Medicaid)

summary(DID.DD)


########################################################################################

DID.DD<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              pov.perc + rGDP.cap + age + UE + disability + insured + hosps + 
              exercise + co2 + HDI + hos.days + SSI.perc + fat + HS.perc + uni.perc
            , data = Medicaid)

summary(DID.DD)

########################################################################################

DID.D51<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              
              age + disability + insured 
             
            , data = Medicaid)

summary(DID.D51)

########################################################################################

DID.D52<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
               treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
               
               age + disability + insured +
               UE + co2 + HDI
             
             , data = Medicaid)

summary(DID.D52)

########################################################################################

DID.D53<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
               treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
               
               age + disability + insured +
               fat + SSI.perc
             
             , data = Medicaid)

summary(DID.D53)

########################################################################################

DID.D53<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
               treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
               
               age + disability + insured +
               fat + SSI.perc +
               crime + exercise
             
             , data = Medicaid)

summary(DID.D53)

########################################################################################

DID.D54<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
               treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
               
               age + disability + insured +
               fat + 
               crime + exercise +
               hosps + hos.days
             
             , data = Medicaid)

summary(DID.D54)

########################################################################################

DID.D55<- lm(death ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
               treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
               
               age + disability + insured +
               fat + 
               exercise +
               hos.days
             
             , data = Medicaid)

summary(DID.D55)

install.packages("sandwich")
library(sandwich)

RseD51<- sqrt(diag(vcovHC(DID.D51, type="HC1"))) 
RseD52<- sqrt(diag(vcovHC(DID.D52, type="HC1"))) 
RseD53<- sqrt(diag(vcovHC(DID.D53, type="HC1"))) 
RseD54<- sqrt(diag(vcovHC(DID.D54, type="HC1"))) 
RseD55<- sqrt(diag(vcovHC(DID.D55, type="HC1"))) 

stargazer(DID.D51,DID.D52,DID.D53,DID.D54,DID.D55
          , df = FALSE
          , column.sep.width = "0.1pt"
          #, type = "text"
          , single.row = FALSE
          , font.size = "small"
          , se = list(RseD51,RseD52,RseD53,RseD54,RseD55))

