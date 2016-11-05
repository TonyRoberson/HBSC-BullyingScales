############ CFA Models ############

## Load relevant packages
library(lavaan)
library(psych)
library(semTools)
library(data.table)

## Import second split half of data (S2)
S2 <- read.csv(file = "HBSC Bullying Scale_S2_CFA.csv", 
               stringsAsFactors=FALSE)


## Create data frame of only the relevant demographic and bullying items in S2
S2.cfa <- data.frame(
  S2[,c("CASEID","METRO3","Q1","Q3B","Q4","Q6_COMP","AFFLUENT",
        "Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
        "Q66G","Q66H","Q66I","Q66J","Q66K",
        "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
        "Q68G","Q68H","Q68I","Q68J","Q68K")])
# Give columns meaningful names
setnames(x = S2.cfa,
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
S2.cfa$age[S2.cfa$age == -9] <- NA
S2.cfa$race[S2.cfa$race == -9] <- NA
S2.cfa$famAffluence[S2.cfa$famAffluence == -9] <- NA
#Set relevant variables as factor
S2.cfa$broadResidence <- as.factor(S2.cfa$broadResidence)
S2.cfa$sex <- as.factor(S2.cfa$sex)
S2.cfa$race <- as.factor(S2.cfa$race)
S2.cfa$gradeLevel <- as.factor(S2.cfa$gradeLevel)


## Get descriptives for all items
S2.ItemDesc <- data.frame(describe(S2.cfa))
describe(S2.cfa)
summary(S2.cfa)
# Export item descriptives to .csv file in working directory
write.csv(x = S2.ItemDesc, file = "S2_ItemDescriptives.csv")


####### TWO FACTOR CFA MODEL #######
## Follow-up test from EFA results


# Specify the two factor model
Model.2factor <- '## Latent factor specification
                    Victimization =~ vVerbal + vExclusion + vPhysical + vRelational + vRacial + 
                      vReligious + vSexual + vComp + vCell + vCompOut + vCellOut
                    Perpetration =~ pVerbal + pExclusion + pPhysical + pRelational + pRacial + 
                      pReligious + pSexual + pComp + pCell + pCompOut + pCellOut
                  ## Latent variable covariance
                    Victimization ~~ Perpetration'
# Fit the model as a CFA
fit.2factor <- cfa(model = Model.2factor, 
                   estimator = "WLSMV",
                   ordered = names(S2.cfa),
                   data = S2.cfa)
# Print fit summary
summary(object = fit.2factor,
        fit.measures = TRUE,
        standardized = TRUE)


## Calculate 2 factor composite scale scores
## NOTE: Temporarily convert S2.cfa to data table to calculate sum scores
setDT(S2.cfa)
## Victimization
S2.cfa$vict.sum <- S2.cfa[, .(vict.sum = rowSums(.SD)),
                            .SDcols = c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial",
                                        "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut")]                            
## Perpetration
S2.cfa$perp.sum <- S2.cfa[, .(perp.sum = rowSums(.SD)), 
                            .SDcols = c("pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                                        "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut")]                            


## Calculate descriptives for scales
## Note: Convert S2.cfa back to data table for following analyses
setDF(S2.cfa)
## Victimization
summary(S2.cfa$vict.sum)
describe(S2.cfa$vict.sum)
# Plot distribution 
histogram(x = S2.cfa$vict.sum)
# Internal consistency
alpha(x = S2.cfa[,c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial", 
                    "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut")])
## Perpetration
summary(S2.cfa$perp.sum)
describe(S2.cfa$perp.sum)
# Plot distribution
histogram(S2.cfa$perp.sum)
# Internal consistency
alpha(x = S2.cfa[,c("pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                    "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut")])



###### MEASUREMENT AND STRUCTURAL INVARIANCE #######


# Indicate the relevant fit indices
fit.indices <- c("chisq.scaled", "df.scaled", "pvalue.scaled", "rmsea.scaled", "rmsea.ci.lower.scaled",
                 "rmsea.ci.upper.scaled", "srmr", "cfi.scaled")

## By sex/gender

# Configural fit
sex.configural.fit <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "sex",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
fitMeasures(sex.configural.fit, fit.indices)

# Weak fit
sex.weak.fit <- cfa(model = Model.2factor, 
                    data = S2.cfa, 
                    group = "sex",
                    estimator = "WLSMV", 
                    ordered = names(S2.cfa), 
                    group.equal = c("loadings"))
fitMeasures(sex.weak.fit, fit.indices)

# Strong fit
sex.strong.fit <- cfa(model = Model.2factor, 
                    data = S2.cfa, 
                    group = "sex",
                    estimator = "WLSMV", 
                    ordered = names(S2.cfa), 
                    group.equal = c("loadings", "intercepts"))
fitMeasures(sex.strong.fit, fit.indices)

# Strict fit
sex.strict.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "sex",
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(sex.strict.fit, fit.indices)

# Add latent variance constraint
sex.var.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "sex",
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts", "residuals", "lv.variances"))
fitMeasures(sex.var.fit, fit.indices)

# Add latent means constraint
sex.means.fit <- cfa(model = Model.2factor, 
                   data = S2.cfa, 
                   group = "sex",
                   estimator = "WLSMV", 
                   ordered = names(S2.cfa), 
                   group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means"))
fitMeasures(sex.means.fit, fit.indices)



## By Grade-level

# Configural fit
gradeLevel.configural.fit <- cfa(model = Model.2factor, 
                          data = S2.cfa, 
                          group = "gradeLevel",
                          estimator = "WLSMV", 
                          ordered = names(S2.cfa))
fitMeasures(gradeLevel.configural.fit, fit.indices)

# Weak fit
gradeLevel.weak.fit <- cfa(model = Model.2factor, 
                    data = S2.cfa, 
                    group = "gradeLevel",
                    estimator = "WLSMV", 
                    ordered = names(S2.cfa), 
                    group.equal = c("loadings"))
fitMeasures(gradeLevel.weak.fit, fit.indices)

# Strong fit
gradeLevel.strong.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "gradeLevel",
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts"))
fitMeasures(gradeLevel.strong.fit, fit.indices)

# Strict fit
gradeLevel.strict.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "gradeLevel",
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(gradeLevel.strict.fit, fit.indices)

# Add latent variance constraint
gradeLevel.var.fit <- cfa(model = Model.2factor, 
                   data = S2.cfa, 
                   group = "gradeLevel",
                   estimator = "WLSMV", 
                   ordered = names(S2.cfa), 
                   group.equal = c("loadings", "intercepts", "residuals", "lv.variances"))
fitMeasures(gradeLevel.var.fit, fit.indices)

# Add latent means constraint
gradeLevel.means.fit <- cfa(model = Model.2factor, 
                     data = S2.cfa, 
                     group = "gradeLevel",
                     estimator = "WLSMV", 
                     ordered = names(S2.cfa), 
                     group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means"))
fitMeasures(gradeLevel.means.fit, fit.indices)



## By student race
## Only White, Black/African American, and Hispanic groups had sufficient sample sizes for comparison

# Configural fit
race.configural.fit <- cfa(model = Model.2factor, 
                          data = S2.cfa,
                          group = "race",
                          group.label = c("1","2","7"),
                          estimator = "WLSMV", 
                          ordered = names(S2.cfa))
fitMeasures(race.configural.fit, fit.indices)

# Weak fit
race.weak.fit <- cfa(model = Model.2factor, 
                    data = S2.cfa, 
                    group = "race",
                    group.label = c("1","2","7"),
                    estimator = "WLSMV", 
                    ordered = names(S2.cfa), 
                    group.equal = c("loadings"))
fitMeasures(race.weak.fit, fit.indices)

# Strong fit
race.strong.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "race",
                      group.label = c("1","2","7"),
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts"))
fitMeasures(race.strong.fit, fit.indices)

# Strict fit
race.strict.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "race",
                      group.label = c("1","2","7"),
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(race.strict.fit, fit.indices)

# Add latent variance constraint
race.var.fit <- cfa(model = Model.2factor, 
                   data = S2.cfa, 
                   group = "race",
                   group.label = c("1","2","7"),
                   estimator = "WLSMV", 
                   ordered = names(S2.cfa), 
                   group.equal = c("loadings", "intercepts", "residuals", "lv.variances"))
fitMeasures(race.var.fit, fit.indices)

# Add latent means constraint
race.means.fit <- cfa(model = Model.2factor, 
                     data = S2.cfa,
                     group = "race",
                     group.label = c("1","2","7"),
                     estimator = "WLSMV", 
                     ordered = names(S2.cfa), 
                     group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means"))
fitMeasures(race.means.fit, fit.indices)
