############ CFA Models ############

## Import second split half of data (S2)
S2 <- read.csv(file = "HBSC Bullying Scale_S2_CFA.csv", 
               stringsAsFactors=FALSE)

## Load relevant packages
library(lavaan)
library(data.table)

## Create data frame of only the bullying items in S2
S2.cfa <- as.data.frame(
  S2[,c("Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
        "Q66G","Q66H","Q66I","Q66J","Q66K",
        "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
        "Q68G","Q68H","Q68I","Q68J","Q68K")])
# Give columns meaningful names
setnames(x = S2.cfa, 
         old = c("Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
                 "Q66G","Q66H","Q66I","Q66J","Q66K",
                 "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
                 "Q68G","Q68H","Q68I","Q68J","Q68K"), 
         new = c("vVerbal", "vExclusion", "vPhysical", "vRelational", "vRacial", 
                 "vReligious", "vSexual", "vComp", "vCell", "vCompOut", "vCellOut",
                 "pVerbal", "pExclusion", "pPhysical", "pRelational", "pRacial", 
                 "pReligious", "pSexual", "pComp", "pCell", "pCompOut", "pCellOut"))

## Get descriptives for S2 bullying items
S2.ItemDesc <- as.data.frame(describe(S2.cfa))
# View data frame
View(S2.ItemDesc)
# Export item descriptives to .csv file in working directory
write.csv(x = S2.ItemDesc, 
          file = "S2_ItemDescriptives.csv")
# Print polychoric correlation matrix
polychoric(x = S2.cfa)


## TWO FACTOR MODEL: WLSMV ##
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
                   data = S2.cfa)
# Print fit summary
summary(object = fit.2factor,
        fit.measures = TRUE,
        standardized = TRUE)



##### Alternative CFA Models #####


## THREE FACTOR MODEL: WLSMV ##


# Specify the three factor model
Model.3factor <-'## Latent factor specification
                  tradVict =~ vVerbal + vExclusion + vPhysical + vRelational + vRacial + 
                    vReligious + vSexual
                  cyberVict  =~ vComp + vCell + vCompOut + vCellOut
                  Perp =~ pVerbal + pExclusion + pPhysical + pRelational + pRacial + 
                    pReligious + pSexual + pComp + pCell + pCompOut + pCellOut
                ## Latent variable covariances
                  tradVict ~~ cyberVict
                  tradVict ~~ Perp
                  cyberVict ~~ Perp'
# Fit the model as a CFA
fit.3factor <- cfa(model = Model.3factor,
                       estimator = "WLSMV",
                       data = S2.cfa)
# Print fit summary
summary(object = fit.3factor, 
        fit.measures = TRUE,
        standardized = TRUE)


## FOUR FACTOR MODEL: WLSMV ##


# Specify the four factor model
Model.4factor <- '## Latent factor specification
                    tradVict =~ vVerbal + vExclusion + vPhysical + vRelational + vRacial + 
                      vReligious + vSexual
                    cyberVict =~ vComp + vCell + vCompOut + vCellOut
                    tradPerp =~ pVerbal + pExclusion + pPhysical + pRelational + pRacial + 
                      pReligious + pSexual
                    cyberPerp =~ pComp + pCell + pCompOut + pCellOut
                  ## Latent variable covariances
                    tradVict ~~ cyberVict
                    tradVict ~~ tradPerp
                    tradVict ~~ cyberPerp
                    tradPerp ~~ cyberPerp
                    tradPerp ~~ cyberVict
                    cyberVict ~~ cyberPerp'
# Fit the model as a CFA
fit.4factor <- cfa(model = Model.4factor, 
                       estimator = "WLSMV",
                       data = S2.cfa)
# Print fit summary
summary(object = fit.4factor, 
        fit.measures = TRUE,
        standardized = TRUE)
