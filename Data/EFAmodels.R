############## EFA Models ###############

## Load relevant packages
library(psych)
library(nFactors)
library(data.table)

## Import first random split half of data (S1)
S1 <- read.csv("HBSC Bullying Scale_S1_EFA.csv", 
               stringsAsFactors=FALSE)

## Create data frame of only the demographic and bullying items in S1
S1.2 <- data.frame(
        S1[,c("CASEID","METRO3","Q1","Q3B","Q4","Q6_COMP","AFFLUENT",
              "Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
              "Q66G","Q66H","Q66I","Q66J","Q66K",
              "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
              "Q68G","Q68H","Q68I","Q68J","Q68K")])
# Give columns meaningful names
setnames(x = S1.2,
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
# Recode missing data from -9 to NA
# Age, Race, and Family affluence
S1.2$age[S1.2$age == -9] <- NA
S1.2$race[S1.2$race == -9] <- NA
S1.2$famAffluence[S1.2$famAffluence == -9] <- NA
#Set relevant variables as factor
S1.2$broadResidence <- as.factor(S1.2$broadResidence)
S1.2$sex <- as.factor(S1.2$sex)
S1.2$race <- as.factor(S1.2$race)
S1.2$gradeLevel <- as.factor(S1.2$gradeLevel)


## Get descriptives for all items
S1.ItemDesc <- data.frame(describe(S1.2))
describe(S1.2)
summary(S1.2)
# Export item descriptives to .csv file in working directory
write.csv(x = S1.ItemDesc, file = "S1_ItemDescriptives.csv")

## Subset only the bullying items in the data frame for EFA
S1.efa <- data.frame(
  S1.2[,c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial", 
          "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut",
          "pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
          "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut")])
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
plotnScree(nScree(efa.pa$fa$values), 
           main = "Scree Plot & Parallel Analysis")


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


## Calculate 2 factor composite scale scores
## NOTE: Need to temporarily convert S1.efa to data table
## to calculate and append scale score columns
setDT(S1.efa)
## Victimization
S1.efa$vict.sum <- S1.efa[, .(vict.sum = rowSums(.SD)), 
                            .SDcols = c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial",
                                        "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut")]                            
## Perpetration
S1.efa$perp.sum <- S1.efa[, .(perp.sum = rowSums(.SD)), 
                            .SDcols = c("pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                                        "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut")]                            


## Calculate descriptives for both scales
## Note: Convert S1.efa back to data frame for subsequent analyses
setDF(S1.efa)
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

