## EFA models 
## Data considered ordered categorical rather than continuous

## Import first random split half of data (S1)
S1 <- read.csv("HBSC Bullying Scale_S1_EFA.csv", 
               stringsAsFactors=FALSE)

## Load relevant packages
library(psych)
library(polycor)
library(nFactors)


## Create dataframe of only the bullying items in S1
efa <- as.data.frame(
        S1[,c("Q66A","Q66B","Q66C","Q66D","Q66E","Q66F",
              "Q66G","Q66H","Q66I","Q66J","Q66K",
              "Q68A","Q68B","Q68C","Q68D","Q68E","Q68F",
              "Q68G","Q68H","Q68I","Q68J","Q68K")])


## Get descriptives for all items
item.desc <- as.data.frame(describe(efa))
# View data frame
View(item.desc)
# Export item descriptives to .csv file in working directory
write.csv(x = item.desc, 
          file = "ItemDescriptives.csv")


## Check results from unconstrained principal axis factoring analysis
## using the polychoric correlation matrix
efa.pa <- fa.poly(x = efa, 
                  fm = "pa", 
                  rotate = "promax")
# Kaiser-Meyer-Olkin Sampling Adequacy
KMO(efa)
# Sphericity
cortest.bartlett(efa)
# Eigenvalues
efa.pa$fa$values
# Scree plot and parallel analysis
efa.poly <- polychoric(efa)
efa.ev <- eigen(efa.poly$rho)
plotnScree(nScree(efa.ev$values), 
           main = "Scree Plot & Parallel Analysis")


## Check 2 factor solution
efa.out.2f <- fa.poly(x = efa, 
                   fm = "pa", 
                   nfactors = 2, 
                   rotate = "promax", 
                   residual = TRUE)
# Pattern matrix loadings
efa.out.2f$fa$loadings
# Correlation between factors
efa.out.2f$fa$Phi
# Factor plot
factor.plot(efa.out.2f$fa)
# Print summary
print.psych(x = efa.out.2f, 
            digits = 3, 
            cut = .3, 
            sort = TRUE)


## Check 3 factor solution
efa.out.3f <- fa.poly(x = efa, 
                      fm = "pa", 
                      nfactors = 3, 
                      rotate = "promax", 
                      residual = TRUE)
# Pattern matrix loadings
efa.out.3f$fa$loadings
# Correlation between factors
efa.out.3f$fa$Phi
# Factor plot
factor.plot(efa.out.3f$fa)
# Print summary
print.psych(x = efa.out.3f, 
            digits = 3, 
            cut = .3, 
            sort = TRUE)


## Check 4 factor solution
efa.out.4f <- fa.poly(x = efa, 
                      fm = "pa", 
                      nfactors = 4, 
                      rotate = "promax", 
                      residual = TRUE)
# Pattern matrix loadings
efa.out.4f$fa$loadings
# Correlation between factors
efa.out.4f$fa$Phi
# Factor plot
factor.plot(efa.out.4f$fa)
# Print summary
print.psych(x = efa.out.4f, 
            digits = 3, 
            cut = .3, 
            sort = TRUE)
