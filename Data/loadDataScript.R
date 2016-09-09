# Load import package
library(foreign)

# Import first split half for EFA
s1 <- read.spss(file = "HBSC Bullying Scale_S1_EFA.sav", 
                     use.value.labels = FALSE, 
                     to.data.frame = TRUE)

# Import second split half for CFA
s2 <- read.spss(file = "HBSC Bullying Scale_S2_CFA.sav", 
          use.value.labels = FALSE,
          to.data.frame = TRUE)

# Import only relevant variables for CFA
s2rel <- read.spss(file = "S2_relevant_variables.sav", 
                        use.value.labels = FALSE, 
                        to.data.frame = TRUE)