?biserial.cor

install.packages("ltm")
library(ltm)

biserial.cor(Medicaid$death, Medicaid$treat.1, level = 1)
biserial.cor(Medicaid$death, Medicaid$treat.2, level = 1)

biserial.cor(Medicaid$death, Medicaid$D.14*Medicaid$treat.1, level = 1)
biserial.cor(Medicaid$death, Medicaid$D.14*Medicaid$treat.2, level = 1)

biserial.cor(Medicaid$death, Medicaid$D.15*Medicaid$treat.1, level = 1)
biserial.cor(Medicaid$death, Medicaid$D.15*Medicaid$treat.2, level = 1)

biserial.cor(Medicaid$death, Medicaid$D.16*Medicaid$treat.1, level = 1)
biserial.cor(Medicaid$death, Medicaid$D.16*Medicaid$treat.2, level = 1)
