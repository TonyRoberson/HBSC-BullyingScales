# Load first split half for EFA
sample1 <- read.spss(file = "HBSC Bullying Scale_S1_EFA.sav", 
                     use.value.labels = FALSE, 
                     to.data.frame = TRUE)

# Load second split half for CFA
sample2 <- read.spss(file = "HBSC Bullying Scale_S2_CFA.sav", 
          use.value.labels = FALSE,
          to.data.frame = TRUE)

# Load only relevant variables for CFA
sample2rel <- read.spss(file = "S2_relevant_variables.sav", 
                        use.value.labels = FALSE, 
                        to.data.frame = TRUE)