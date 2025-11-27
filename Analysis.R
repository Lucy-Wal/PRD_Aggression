##############################**Install Packages**##############################
packages <- c("dplyr", "Hmisc", "lavaan", "psych", "report")
for (package in packages){
  if(!is.element(package, .packages(all.available = TRUE))){install.packages(package)}
  library(package, character.only = TRUE)}

options(warn = -1)
####**Read In Data**####
data <- read.csv("Waldren_Callan25.csv")

###########################**Descriptive Statistics**###########################
##**Reliability**## 
#Perceived Constraints
psych::alpha(data[,c(paste0("SCS", c(1, 2, 4, 7, 9:12), "_PC"))])$total
psych::omega(data[,c(paste0("SCS", c(1, 2, 4, 7, 9:12), "_PC"))], nfactor = 1)$omega.tot

#Personal Mastery 
psych::alpha(data[,c(paste0("SCS", c(3, 5, 6, 8), "_PM"))])$total
psych::omega(data[,c(paste0("SCS", c(3, 5, 6, 8), "_PM"))], nfactor = 1)$omega.tot

#Psychological Distress
psych::alpha(data[,c(paste0("DASS", 1:21))])$total
psych::omega(data[,c(paste0("DASS", 1:21))], nfactor = 1)$omega.tot

#PRD
psych::alpha(data[,c(paste0("PRDS", 1:5))])$total
psych::omega(data[,c(paste0("PRDS", 1:5))], nfactor = 1)$omega.tot

#Aggression
psych::alpha(data[,c(paste0("BPAQ", 1:12))])$total
psych::omega(data[,c(paste0("BPAQ", 1:12))], nfactor = 1)$omega.tot

##**Means and SDs**##
means <- data %>% select(Income, Gender, Age, PC, PM, DASS, PRDS, BPAQ)
psych::describe(means)

##**Correlations**##
cor <- rcorr(as.matrix(means), type = "pearson")

#############################**Mediation Analyses**#############################
##**Parallel Mediation**##
soc <- 'DASS ~ b1 * PM + b2 * PC + c * PRDS                 #Direct effect
        PM ~ a1 * PRDS + Age + Gender + Income              #Mediation via PM
        PC ~ a2 * PRDS + Age + Gender + Income              #Mediation via PC
        indirect1 := a1 * b1                                #Indirect PM
        indirect2 := a2 * b2                                #Indirect PC
        total := c + indirect1 + indirect2                  #Total
        PM ~~ PC'

soc_fit <- sem(model = soc, data = data, bootstrap = 10000)   
parameterEstimates(soc_fit, boot.ci.type = "bca.simple", standardized = F)
fitMeasures(soc_fit)

##**Serial Mediation**##
agg <- 'BPAQ ~ c * PRDS + b1 * PC + b2 * DASS               #Direct Effect
        PC ~ a1 * PRDS + Gender + Age + Income              #Mediation via PC
        DASS ~ d * PC + a2 * PRDS + Gender + Age + Income   #Mediation via DASS
        indirect1 := a1 * b1                                #PRDS -> PC -> BPAQ
        indirect2 := a1 * d * b2                            #PRDS -> PC -> DASS -> BPAQ
        indirect3 := a2 * b2                                #PRDS -> DASS -> BPAQ
        total := c + indirect1 + indirect2 + indirect3'     #Total

agg_fit <- sem(model = agg, data = data, bootstrap = 10000)   
parameterEstimates(agg_fit, boot.ci.type = "bca.simple", standardized = F)
fitMeasures(agg_fit)

###########################**Supplementary Analyses**###########################
##**Mediation Analyses Without Covariates**##
#*Parallel Mediation*#
soc_nc <- 'DASS ~ b1 * PM + b2 * PC + c * PRDS                 #Direct effect
           PM ~ a1 * PRDS                                      #Mediation via PM
           PC ~ a2 * PRDS                                      #Mediation via PC
           indirect1 := a1 * b1                                #Indirect PM
           indirect2 := a2 * b2                                #Indirect PC
           total := c + indirect1 + indirect2                  #Total
           PM ~~ PC'

soc_nc_fit <- sem(model = soc_nc, data = data, bootstrap = 10000)   
parameterEstimates(soc_nc_fit, boot.ci.type = "bca.simple", standardized = F)

#*Serial Mediation*#
agg_nc <- 'BPAQ ~ c * PRDS + b1 * PC + b2 * DASS               #Direct Effect
           PC ~ a1 * PRDS                                      #Mediation via PC
           DASS ~ d * PC + a2 * PRDS                           #Mediation via DASS
           indirect1 := a1 * b1                                #PRDS -> PC -> BPAQ
           indirect2 := a1 * d * b2                            #PRDS -> PC -> DASS -> BPAQ
           indirect3 := a2 * b2                                #PRDS -> DASS -> BPAQ
           total := c + indirect1 + indirect2 + indirect3'     #Total

agg_nc_fit <- sem(model = agg_nc, data = data, bootstrap = 10000)   
parameterEstimates(agg_nc_fit, boot.ci.type = "bca.simple", standardized = F)

############################**Session Information**#############################
session <- sessionInfo()
report_system(session)
as.data.frame(report(session))