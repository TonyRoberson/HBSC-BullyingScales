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

### By sex/gender

## Individual groups

# Male fit
male.S2 <- cfa(model = Model.2factor, 
                       data = S2.cfa,
                       group = "sex",
                       group.label = "1",
                       estimator = "WLSMV", 
                       ordered = names(S2.cfa))
summary(object = male.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# Female fit
female.S2 <- cfa(model = Model.2factor, 
               data = S2.cfa,
               group = "sex",
               group.label = "2",
               estimator = "WLSMV", 
               ordered = names(S2.cfa))
summary(object = female.S2,
        fit.measures = TRUE,
        standardized = TRUE)


## Measurement Invariance

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


## Structural Invariance

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

## Scaled-shifted chi-square difference test
lavTestLRT(sex.configural.fit, sex.weak.fit, sex.strong.fit, 
           sex.var.fit, sex.means.fit, method = "satorra.bentler.2010")


### By Grade-level

## Individual groups

# 5th grade fit
fifth.S2 <- cfa(model = Model.2factor, 
               data = S2.cfa,
               group = "gradeLevel",
               group.label = "5",
               estimator = "WLSMV", 
               ordered = names(S2.cfa))
summary(object = fifth.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# 6th grade fit
sixth.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "gradeLevel",
                group.label = "6",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = sixth.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# 7th grade fit
seventh.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "gradeLevel",
                group.label = "7",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = seventh.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# 8th grade fit
eighth.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "gradeLevel",
                group.label = "8",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = eighth.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# 9th grade fit
ninth.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "gradeLevel",
                group.label = "9",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = ninth.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# 10th grade fit
tenth.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "gradeLevel",
                group.label = "10",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = tenth.S2,
        fit.measures = TRUE,
        standardized = TRUE)


## Measurement Invariance

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


## Sturctural Invariance

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


## Scaled chi-squared difference test
lavTestLRT(gradeLevel.configural.fit, gradeLevel.weak.fit, gradeLevel.strong.fit, 
      gradeLevel.var.fit, gradeLevel.means.fit, method = "satorra.bentler.2010")



### By student race
### Note: Only White, Black/African American, and Hispanic/Latino groups had 
### sufficient sample sizes for comparison


## Individual groups

# White fit
white.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "race",
                group.label = "2",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = white.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# Hispanic/Latino fit
hispanic.S2 <- cfa(model = Model.2factor, 
                data = S2.cfa,
                group = "race",
                group.label = "7",
                estimator = "WLSMV", 
                ordered = names(S2.cfa))
summary(object = hispanic.S2,
        fit.measures = TRUE,
        standardized = TRUE)

# Black/African American
black.S2 <- cfa(model = Model.2factor, 
                   data = S2.cfa,
                   group = "race",
                   group.label = "1",
                   estimator = "WLSMV", 
                   ordered = names(S2.cfa))
summary(object = black.S2,
        fit.measures = TRUE,
        standardized = TRUE)


## Measurement invariance

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


## Structural invariance

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

## Scaled chi-squared difference test
lavTestLRT(race.configural.fit, race.weak.fit, race.strong.fit, 
           race.var.fit, race.means.fit, method = "satorra.bentler.2010")
