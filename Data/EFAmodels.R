############## EFA Models ###############

## Load relevant packages
library(psych)
library(polycor)
library(nFactors)
library(data.table)

## Import first random split half of data (S1)
S1 <- read.csv("HBSC Bullying Scale_S1_EFA.csv", 
               stringsAsFactors=FALSE)

## Create data table of only the bullying items in S1
S1.efa <- data.table(
        S1[,c("CASEID","METRO3","Q1","Q3B","Q4","Q6_COMP","AFFLUENT",
              "Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
              "Q66G","Q66H","Q66I","Q66J","Q66K",
              "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
              "Q68G","Q68H","Q68I","Q68J","Q68K")])
# Give columns meaningful names
setnames(x = S1.efa, 
         old = c("CASEID","METRO3","Q1","Q3B","Q4","Q6_COMP","AFFLUENT",
                 "Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
                 "Q66G","Q66H","Q66I","Q66J","Q66K",
                 "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
                 "Q68G","Q68H","Q68I","Q68J","Q68K"), 
         new = c("ID","broadResidence","sex","age","gradeLevel","race","famAffluence",
                 "vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial", 
                 "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut",
                 "pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                 "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut"))


## Get descriptives for all items
S1.ItemDesc <- data.table(describe(S1.efa))
# View data frame
View(S1.ItemDesc)
# Export item descriptives to .csv file in working directory
write.csv(x = S1.ItemDesc, file = "S1_ItemDescriptives.csv")


## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa <- fa.poly(x = S1.efa, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(S1.efa)
# Sphericity
cortest.bartlett(S1.efa)
# Eigenvalues
efa.pa$fa$values
# Scree plot and parallel analysis
efa.poly <- polychoric(S1.efa)
efa.ev <- eigen(efa.poly$rho)
plotnScree(nScree(efa.ev$values), 
           main = "Scree Plot & Parallel Analysis")
# Print polychoric correlation matrix
print(efa.poly)


## Check 2 factor solution
efa.out.2f <- fa.poly(x = S1.efa, 
                   fm = "pa", 
                   nfactors = 2, 
                   rotate = "promax", 
                   residual = TRUE)
# Inspect model residuals
resid.2f <- as.matrix(efa.out.2f$fa$residual)
describe(resid.2f)
boxplot(resid.2f)
# Factor plot
factor.plot(efa.out.2f$fa)
# Print summary
print.psych(x = efa.out.2f, 
            digits = 3, 
            sort = TRUE)


## Calculate 2 factor composite scale scores
## Victimization
S1.efa$vict.sum <- S1.efa[, .(vict.sum = rowSums(.SD)), 
                            .SDcols = c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial",
                                        "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut")]                            
## Perpetration
S1.efa$perp.sum <- S1.efa[, .(perp.sum = rowSums(.SD)), 
                            .SDcols = c("pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                                        "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut")]                            


## Calculate descriptives for scales
## Victimization
summary(S1.efa$vict.sum)
describe(S1.efa$vict.sum)
# Plot distribution 
histogram(x = S1.efa$vict.sum)
# Internal consistency
alpha(x = S1.efa[,c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial", 
                    "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut")])
## Perpetration
summary(S1.efa$perp.sum)
describe(S1.efa$perp.sum)
# Plot distribution
histogram(S1.efa$perp.sum)
# Internal consistency
alpha(x = S1.efa[,c("pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                    "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut")])



########## Alternative Models ############

## Check 3 factor solution
efa.out.3f <- fa.poly(x = S1.efa, 
                      fm = "pa", 
                      nfactors = 3, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.3f <- as.matrix(efa.out.3f$fa$residual)
describe(resid.3f)
boxplot(resid.3f)
# Factor plot
factor.plot(efa.out.3f$fa)
# Print summary
print.psych(x = efa.out.3f, 
            digits = 3, 
            sort = TRUE)


## Check 4 factor solution
efa.out.4f <- fa.poly(x = S1.efa, 
                      fm = "pa", 
                      nfactors = 4, 
                      rotate = "promax", 
                      residual = TRUE)
# Inspect model residuals
resid.4f <- as.matrix(efa.out.4f$fa$residual)
describe(resid.4f)
boxplot(resid.4f)
# Factor plot
factor.plot(efa.out.4f$fa)
# Print summary
print.psych(x = efa.out.4f, 
            digits = 3,
            sort = TRUE)