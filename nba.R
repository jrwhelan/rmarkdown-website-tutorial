
library(RSGHB)
library(glue)

choicedata <- winloss %>%
	rename(ID = idGame) %>%
	arrange(ID)

teamnames <- teamLogs %>%
	dplyr::select(slugTeam) %>%
	dplyr::distinct()

teamList <- teamnames$slugTeam

# ---------------------------------------------------------------------------
# THE LIKELIHOOD FUCNTION

fixed <- FALSE

if (fixed==TRUE) {
# FIXED PARAMS
n <- ""
for (i in 1:length(teamList)){
  n <- glue('{n} B_{teamList[i]} <- fc[cc]; cc <- cc + 1;')
}
} else {
# RANDOM PARAMS
n <- ""
for (i in 1:length(teamList)){
	n <- glue('{n} B_{teamList[i]} <- b[, cc]; cc <- cc + 1;')
}
}

g1 <- glue('v1 <- ')
for (i in 1:(length(teamList)-1)){
	g1 <- glue('{g1} B_{teamList[i]}*choicedata$win_{teamList[i]} + ')
}
g1 <- glue('{g1} B_{teamList[length(teamList)]}*choicedata$win_{teamList[length(teamList)]} ')

g2 <- glue('v2 <- ')
for (i in 1:(length(teamList)-1)){
	g2 <- glue('{g2} B_{teamList[i]}*choicedata$lose_{teamList[i]} + ')
}
g2 <- glue('{g2} B_{teamList[length(teamList)]}*choicedata$lose_{teamList[length(teamList)]} ')

j <- glue('p <- (exp(v1)*choicedata$choice1 + exp(v2)*choicedata$choice2) / (exp(v1) + exp(v2))')
z <- glue('likelihood <- function(fc, b) {{ cc <- 1; {n} {g1}; {g2}; {j}; return(p) }}') 
###    ', .open="{{", .close="}}"
eval(parse(text=z))
# ---------------------------------------------------------------------------


# ITERATION SETTINGS
gNCREP    <- 50000  # Number of iterations to use prior to convergence
gNEREP    <- 2000  # Number of iterations to keep for averaging after convergence has been reached
gNSKIP    <- 10   # Number of iterations to do in between retaining draws for averaging
gINFOSKIP <- 1000    # How frequently to print info about the iteration process

iters <- gNCREP + (gNEREP*gNSKIP)



if (fixed==TRUE) {
# FIXED PARAMS
modelname <- "simpleCompare-aggregate" 
gVarNamesFixed <- glue("B_{teamList}")
FC <- rep(0, length(teamList))         # for the fixed coefficients

control <- list(
  modelname = modelname,
  gVarNamesFixed = gVarNamesFixed,
  FC = FC,
  gNCREP = gNCREP,
  gNEREP = gNEREP,
  gNSKIP = gNSKIP,
  gINFOSKIP = gINFOSKIP,
  targetAcceptanceNormal = 0.234,
  gSeed = 1984
)
} else {
# RANDOM PARAMS
modelname <- "simpleCompare" 
gVarNamesNormal <- glue("B_{teamList}")
gDIST <- c(rep(1, length(teamList)))
svN <- rep(0, length(teamList))     # for the random coefficients

control <- list(
  modelname = modelname,
  gVarNamesNormal = gVarNamesNormal,
  svN = svN,
  gDIST = gDIST,
  gNCREP = gNCREP,
  gNEREP = gNEREP,
  gNSKIP = gNSKIP,
  gINFOSKIP = gINFOSKIP,
  targetAcceptanceNormal = 0.234,
  gSeed = 1984
)
}


#HIT RATE TESTING:
#	HIT.TEST <- dplyr::filter(choicedata, Task==5)
#	write.csv(HIT.TEST,"RSGHB hit_test - noOil - negNormP.csv", row.names = FALSE)


#filtering obs:
# choicedata <- dplyr::filter(choicedata, Task!=5)

# Estimate the model
# model <- doHB(likelihood, choicedata, control)

# Save model object
######## save(model, file = paste0(model$modelname, ".RData"))

# write the model utilities
# write.csv(model$C,"RSGHB utilities - Best - negNormP -censored.csv", row.names = FALSE)


