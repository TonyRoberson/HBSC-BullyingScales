############ CFA Models ############

## Load relevant packages
library(lavaan)
library(data.table)
library(psych)
library(semTools)

## Import second split half of data (S2)
S2 <- read.csv(file = "HBSC Bullying Scale_S2_CFA.csv", 
               stringsAsFactors=FALSE)

## Create data frame of only the relevant demographic and bullying items in S2
S2.cfa <- as.data.frame(
  S2[,c("METRO3","Q1","Q3B","Q4","Q6_COMP","AFFLUENT",
        "Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
        "Q66G","Q66H","Q66I","Q66J","Q66K",
        "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
        "Q68G","Q68H","Q68I","Q68J","Q68K")])
# Give columns meaningful names
setnames(x = S2.cfa, 
         old = c("METRO3","Q1","Q3B","Q4","Q6_COMP","AFFLUENT",
                 "Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
                 "Q66G","Q66H","Q66I","Q66J","Q66K",
                 "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
                 "Q68G","Q68H","Q68I","Q68J","Q68K"), 
         new = c("broadResidence","sex","age","gradeLevel","race","famAffluence",
                  "vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial", 
                 "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut",
                 "pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                 "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut"))

## Get descriptives for S2.cfa items
S2.ItemDesc <- as.data.frame(describe(S2.cfa))
# View data frame
View(S2.ItemDesc)
# Export item descriptives to .csv file in working directory
write.csv(x = S2.ItemDesc,
          file = "S2_ItemDescriptives.csv")


## TWO FACTOR MODEL ##
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



## MEASUREMENT INVARIANCE ##

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

# Configural fit
race.configural.fit <- cfa(model = Model.2factor, 
                          data = S2.cfa,
                          group = "race",
                          estimator = "WLSMV", 
                          ordered = names(S2.cfa))
fitMeasures(race.configural.fit, fit.indices)

# Weak fit
race.weak.fit <- cfa(model = Model.2factor, 
                    data = S2.cfa, 
                    group = "race",
                    estimator = "WLSMV", 
                    ordered = names(S2.cfa), 
                    group.equal = c("loadings"))
fitMeasures(race.weak.fit, fit.indices)

# Strong fit
race.strong.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "race",
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts"))
fitMeasures(race.strong.fit, fit.indices)

# Strict fit
race.strict.fit <- cfa(model = Model.2factor, 
                      data = S2.cfa, 
                      group = "race",
                      estimator = "WLSMV", 
                      ordered = names(S2.cfa), 
                      group.equal = c("loadings", "intercepts", "residuals"))
fitMeasures(race.strict.fit, fit.indices)

# Add latent variance constraint
race.var.fit <- cfa(model = Model.2factor, 
                   data = S2.cfa, 
                   group = "race",
                   estimator = "WLSMV", 
                   ordered = names(S2.cfa), 
                   group.equal = c("loadings", "intercepts", "residuals", "lv.variances"))
fitMeasures(race.var.fit, fit.indices)

# Add latent means constraint
race.means.fit <- cfa(model = Model.2factor, 
                     data = S2.cfa, 
                     group = "race",
                     estimator = "WLSMV", 
                     ordered = names(S2.cfa), 
                     group.equal = c("loadings", "intercepts", "residuals", "lv.variances", "means"))
fitMeasures(race.means.fit, fit.indices)
