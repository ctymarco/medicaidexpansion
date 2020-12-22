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

######################################################

DID.11<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
               treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
               fat + age
             , data = Medicaid)

summary(DID.11)

#intercept ***
#fat ***
#age ***

#Multiple R-squared:  0.5903,	Adjusted R-squared:  0.5611 
#F-statistic: 20.17 on 13 and 182 DF,  p-value: < 2.2e-16

######################################################


DID.12<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability
            , data = Medicaid)

summary(DID.12)

#intercept ***
#fat ***
#age ***
#disability ***

#Multiple R-squared:  0.7697,	Adjusted R-squared:  0.7519 
#F-statistic: 43.22 on 14 and 181 DF,  p-value: < 2.2e-16

######################################################

DID.13<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability + 
              rGDP.cap
            , data = Medicaid)

summary(DID.13)


#fat ***
#age ***
#disability ***
#rGDP.cap **

#Multiple R-squared:   0.78,	Adjusted R-squared:  0.7616 
#F-statistic: 42.53 on 15 and 180 DF,  p-value: < 2.2e-16

######################################################

DID.15<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability + 
              rGDP.cap +
              UE
            , data = Medicaid)

summary(DID.15)

#Intercept *
#fat ***
#age ***
#disability ***
#rGDP.cap *
#UE ***

#Multiple R-squared:  0.7989,	Adjusted R-squared:  0.781 
#F-statistic: 44.46 on 16 and 179 DF,  p-value: < 2.2e-16


######################################################

DID.16<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability + 
              rGDP.cap +
              
              hosps
            , data = Medicaid)

summary(DID.16)

#fat ***
#age ***
#disability ***
#rGDP.cap ***
#hosps ***

#Multiple R-squared:  0.8264,	Adjusted R-squared:  0.8109 
#F-statistic: 53.26 on 16 and 179 DF,  p-value: < 2.2e-16

######################################################

DID.17<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability + 
              rGDP.cap +
              
              hosps +
              hos.days
            , data = Medicaid)

summary(DID.17)

#fat ***
#age ***
#disability ***
#rGDP.cap *
#hosps ***
#hos.days ***

#Multiple R-squared:  0.8565,	Adjusted R-squared:  0.8428 
#F-statistic: 62.48 on 17 and 178 DF,  p-value: < 2.2e-16

######################################################

DID.18<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability + 
              
              
              hosps +
              hos.days +
              exercise
            , data = Medicaid)

summary(DID.18)

#intercept *
#fat ***
#age ***
#disability ***

#hosps ***
#hos.days ***
#exercise **

#Multiple R-squared:  0.8579,	Adjusted R-squared:  0.8443 
#F-statistic:  63.2 on 17 and 178 DF,  p-value: < 2.2e-16

######################################################

DID.19<- lm(Y1 ~ D.14 + D.15 + D.16 + treat.1 + D.14*treat.1 + D.15*treat.1 + D.16*treat.1 +
              treat.2 + D.14*treat.2 + D.15*treat.2 + D.16*treat.2 + 
              fat + age +
              disability + 
              
              
              hosps +
              hos.days +
              exercise +
              insured
            , data = Medicaid)

summary(DID.19)

#intercept *
#fat ***
#age ***
#disability ***

#hosps ***
#hos.days ***
#exercise **
#insured **

#Multiple R-squared:  0.8653,	Adjusted R-squared:  0.8516 
#F-statistic: 63.18 on 18 and 177 DF,  p-value: < 2.2e-16

install.packages("stargazer")
library(stargazer)


install.packages("sandwich")
library(sandwich)

Rse11<- sqrt(diag(vcovHC(DID.11, type="HC1"))) 
Rse12<- sqrt(diag(vcovHC(DID.12, type="HC1"))) 
Rse13<- sqrt(diag(vcovHC(DID.13, type="HC1"))) 
#Rse14<- sqrt(diag(vcovHC(DID.14, type="HC1"))) 
Rse15<- sqrt(diag(vcovHC(DID.15, type="HC1"))) 
Rse16<- sqrt(diag(vcovHC(DID.16, type="HC1"))) 
Rse17<- sqrt(diag(vcovHC(DID.17, type="HC1")))
Rse18<- sqrt(diag(vcovHC(DID.18, type="HC1")))
Rse19<- sqrt(diag(vcovHC(DID.19, type="HC1")))

stargazer(DID.11,DID.12,DID.13,DID.15,DID.16,DID.17,DID.18,DID.19, type = "html", out="Y1.html"
          , df = FALSE
          , column.sep.width = "0.1pt"
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse11,Rse12,Rse13,Rse15,Rse16,Rse17,Rse18,Rse19))


stargazer(DID.11,DID.12,DID.13,DID.15,DID.16,DID.17,DID.18,DID.19
          , df = FALSE
          , column.sep.width = "0.1pt"
          
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse11,Rse12,Rse13,Rse15,Rse16,Rse17,Rse18,Rse19))

stargazer(DID.11,DID.12,DID.16,DID.17,DID.18,DID.19
          , df = FALSE
          , column.sep.width = "0.1pt"
          
          , single.row = FALSE
          , font.size = "small"
          , se = list(Rse11,Rse12,Rse16,Rse17,Rse18,Rse19))



