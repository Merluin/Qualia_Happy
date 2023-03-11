#################################################
# 
# Experiment:     Qualia_soma
# Programmer:     Thomas Quettier
# Date:           30/03/2021
# Description:    power analysis
#
#################################################

###Parameters ----
effect<- .478 # medium effect from Quettier 2021
directedeffect = "greater"

###Test ----
pwr.t.test( d = effect, n = 22, sig.level = 0.05 , type = "paired", alternative = directedeffect)