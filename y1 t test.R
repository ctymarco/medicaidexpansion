setwd("J:/4274/Medicaid idea/R Medicaid")

library(readxl)
Medicaid <- read_excel("Medicaid2.xlsx")

Medicaid<- as.data.frame(Medicaid)
Medicaid<- cbind(Medicaid[,1:3],Medicaid[,5:26])

control<- Medicaid[(Medicaid$treat.1+Medicaid$treat.2==0),]

treated1<- Medicaid[(Medicaid$treat.1==1),]

treated2<- Medicaid[(Medicaid$treat.2==1),]

#Getting Y1 vector for 2013 control

Y1.13.control<- (control$D.14+control$D.15+control$D.16==0)*control$Y1

Y1.13.control<- as.data.frame(Y1.13.control)

?apply

row_sub = apply(Y1.13.control, 1, function(row) all(row !=0 ))

#Y1 vector
Y1.13.control<- Y1.13.control[row_sub,]

#Getting Y1 vector for 2014 control

Y1.14.control<- (control$D.14==1)*control$Y1

Y1.14.control<- as.data.frame(Y1.14.control)

row_sub = apply(Y1.14.control, 1, function(row) all(row !=0 ))

#Y1 vector for 2014
Y1.14.control<- Y1.14.control[row_sub,]

Y1.13.treated1<- (treated1$D.14+treated1$D.15+treated1$D.16==0)*treated1$Y1

Y1.13.treated1<- as.data.frame(Y1.13.treated1)

?apply

row_sub = apply(Y1.13.treated1, 1, function(row) all(row !=0 ))

#Y1 vector
Y1.13.treated1<- Y1.13.treated1[row_sub,]

#Getting Y1 vector for 2014 treated1

Y1.14.treated1<- (treated1$D.14==1)*treated1$Y1

Y1.14.treated1<- as.data.frame(Y1.14.treated1)

row_sub = apply(Y1.14.treated1, 1, function(row) all(row !=0 ))

#Y1 vector for 2014
Y1.14.treated1<- Y1.14.treated1[row_sub,]








##############################

Y1.control.diff<- Y1.14.control-Y1.13.control
Y1.treated1.diff<- Y1.14.treated1-Y1.13.treated1

t.test(Y1.control.diff,Y1.treated1.diff ,alternative = "two.sided", 0, paired = F, conf.level = .95)
