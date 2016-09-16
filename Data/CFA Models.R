## CFA Models

## Load relevant packages
library(lavaan)


## TWO FACTOR MODEL ##


# Specify the two factor model
Model.2factor <- '## Victimization factor
                    Victimization =~ Q66A+Q66B+Q66C+Q66D+Q66E+Q66F+Q66G+Q66H+Q66I+Q66J+Q66K
                  ## Perpetration factor
                    Perpetration =~ Q68A+Q68B+Q68C+Q68D+Q68E+Q68F+Q68G+Q68H+Q68I+Q68J+Q68K
                  ## Covariance between Vict and Perp
                    Victimization ~~ Perpetration'
# Fit the model as a CFA
fit.2factor <- cfa(model = Model.2factor, 
                   estimator = 'ML',
                   test = "bollen-stine",
                   bootstrap = 2000,
                   data = s2rel)
# Print fit summary
summary(object = fit.2factor,
        fit.measures = TRUE,
        standardized = TRUE)


## THREE FACTOR MODEL ##


# Specify the three factor model
Model.3factor <-'## Latent factor specification
                  tradVict =~ Q66A+Q66B+Q66C+Q66D+Q66E+Q66F+Q66G
                  cyberVict  =~ Q66H+Q66I+Q66J+Q66K
                  Perp =~ Q68A+Q68B+Q68C+Q68D+Q68E+Q68F+Q68G+Q68H+Q68I+Q68J+Q68K
                 ## Latent variable covariances
                  tradVict ~~ cyberVict
                  tradVict ~~ Perp
                  cyberVict ~~ Perp'
# Fit the model as a CFA
fit.3factor <- cfa(model = Model.3factor, 
                   estimator = "ML",
                   test = "bollen-stine",
                   bootstrap = 2000,
                   data = s2rel)
# Print fit summary
summary(object = fit.3factor, 
        fit.measures = TRUE,
        standardized = TRUE)


## FOUR FACTOR MODEL ##


# Specify the four factor model
Model.4factor <- '## Latent factor specification
                    tradVict =~ Q66A+Q66B+Q66C+Q66D+Q66E+Q66F+Q66G
                    cyberVict =~ Q66H+Q66I+Q66J+Q66K
                    tradPerp =~ Q68A+Q68B+Q68C+Q68D+Q68E+Q68F+Q68G
                    cyberPerp =~ Q68H+Q68I+Q68J+Q68K
                  ## Latent variable covariances
                    tradVict ~~ cyberVict
                    tradVict ~~ tradPerp
                    tradVict ~~ cyberPerp
                    tradPerp ~~ cyberPerp
                    tradPerp ~~ cyberVict
                    cyberVict ~~ cyberPerp'
# Fit the model as a CFA
fit.4factor <- cfa(model = Model.4factor, 
                   estimator = "ML",
                   test = "bollen-stine",
                   bootstrap = 2000,
                   data = s2rel)
# Print fit summary
summary(object = fit.4factor, 
        fit.measures = TRUE,
        standardized = TRUE)
