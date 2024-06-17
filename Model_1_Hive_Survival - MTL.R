library(car)       # vif and box-tidwell tests
library(DHARMa)    # simulated residuals analysis 
library(sandwich)  # standard error and robust values
library(sjPlot)   # tab_model function for model output
library(spdep)    # for spatial dependency analysis
library(sp)       # for spatial objects
library(pscl)     # McFadden R2
library(mfx)      # marginal effects
library(pgirmess) #correlog function for spatial correlogram
library(spfilteR) # glmFilter to filter for spatial autocorrelation
library(betareg)
library(rgdal)
library(readxl)
library(xlsx)
library(tidyverse) 

rm(list=ls())
setwd("C:/_MY FOLDERS/RECHERCHE/HIVE")
HIVE <- read.csv("HIVE_1KM.csv")


################################################################################
#                                Variable creation
################################################################################

# Remove Place Unknown
HIVE <- HIVE[HIVE$Place != "Unknown", ]

# Levels for Median Income
HIVE$MedInc_Qrt <- factor(HIVE$MedInc_Qrt, levels = c("1", "2", "3","4"))

# Levels for Hive count
HIVE$HvCnt_Cat <- factor(HIVE$HvCnt_Jenks, levels = c("<2","<5",'<10', '<50', "50+" ))
HIVE$HvCnt_Cat <- relevel(as.factor(HIVE$HvCnt_Jenks), ref = "<2")

#Year dummy
# HIVE$Year_ <- factor(ifelse(HIVE$Year %in% c('2017', '2018'), 'PreCov',
#                             ifelse(HIVE$Year %in% c('2020', '2021', '2022'), 'Covid', '2019')), 
#                      levels = c("PreCov", "2019", "Covid"))
# HIVE$Year_ <- relevel(HIVE$Year_, ref = "2019")


# HIVE$Year_ <- factor(ifelse(HIVE$Year %in% c('2017', '2018'), 'PreCov', HIVE$Year))
# HIVE$Year_ <- relevel(HIVE$Year_, ref = "2019")

HIVE$Year_ <- factor(ifelse(HIVE$Year %in% c('2020', '2021', '2022'), 'Covid', 'reference'))
HIVE$Year_ <- relevel(HIVE$Year_, ref = 'reference')

HIVE$Year_Cat <- relevel(as.factor(HIVE$Year), ref = "2019")

###############################################################################
#                            FUNCTION
################################################################################

RSQS <- function(loglike.full, loglike.null,full.deviance, null.deviance, nb.params, n){
  explained_dev <- 1-(full.deviance / null.deviance)
  K <- nb.params
  r2_faddenadj <- 1- (loglike.full - K) / loglike.null
  Lm <- loglike.full
  Ln <- loglike.null
  Rcs <- 1 - exp((-2/n) * (Lm-Ln))
  Rn <- Rcs / (1-exp(2*Ln/n))
  return(
    list("explained deviance" = explained_dev,
         "McFadden ajusted" = r2_faddenadj,
         "Cox and Snell" = Rcs,
         "Nagelkerke" = Rn
    )
  )
}


HIVE <- filter(HIVE,City == "MTL")


#################################################################################
#                                    Equation 1.1
################################################################################
name <- c("MTL")

VARSLIST <- c("NDVI_50_100",
              "NDVI_20_50",
              "Water_Bi" ,
              #"Build_Pct",
              "BckYrd_mean",
              "Hghwy_len_Binary",
              "Smoke",
              "PM25_mean",
              "O3" ,
              "Own_mean", 
              #"MedInc_Qrt",
              "Place" ,  
              "HvCnt_Cat", 
              "Year_")

form <- as.formula(paste("Status_Binary ~", paste(VARSLIST, collapse = " + ")))
MODEL_1.1 <- glm(formula = form,family = binomial,data = HIVE); summary(MODEL_1.1)


# Outliers
COOK_DIST <- data.frame(dist = cooks.distance(MODEL_1.1),oid = 1:nrow(HIVE))
Outliers_List <- subset(HIVE, COOK_DIST$dist>=0.003)$HiveID # criteria (8/n) = 0.002
HIVE_SUB <- subset(HIVE, !(HiveID %in% Outliers_List))

# NO OUTLIERS
MODEL_1.1 <- glm(formula = form,family = binomial,data = HIVE_SUB); summary(MODEL_1.1)
vif(MODEL_1.1)

# INDEPENDANCE OF OBSERVATIONS (deviance residuals)
summary_model <- summary(MODEL_1.1)
summary_dev_res <- summary_model$deviance.resid
quantile(summary_dev_res)
HIVE_SUB <- HIVE_SUB[order(HIVE_SUB$Year),] # Sort the dataset by HIVE_SUB$Year
HIVE_SUB$Index_Number <- 1:nrow(HIVE_SUB) # Create HIVE_SUB$Index_Number to represent the order of observations in the dataset

ggplot(data=data.frame(x=HIVE_SUB$Index_Number, y=scale(resid(MODEL_1.1, type='deviance'))), 
       aes(x=x, y=y)) +
  geom_line() +
  ggtitle("Residual Series Plot") +
  xlab("Index Number") +
  ylab("Deviance Residuals") +
  geom_hline(yintercept=0, linetype="dashed", color="red")

# Null model
MODEL_NULL <- glm(Status_Binary ~1,family = binomial(link="logit"),data = HIVE_SUB)

rsqs_out<- RSQS(loglike.full = as.numeric(logLik(MODEL_1.1)), 
                loglike.null = as.numeric(logLik(MODEL_NULL)), 
                full.deviance = deviance(MODEL_1.1), 
                null.deviance = deviance(MODEL_NULL), 
                nb.params = MODEL_1.1$rank, 
                n = nrow(HIVE_SUB)) 

# PREPARE TABLE
COV_MODEL_1.1<- vcovHC(MODEL_1.1, type = "HC0") # HCO METHOD BASED ON RESIDUS
STD_ERR_ROBUST <- sqrt(diag(COV_MODEL_1.1)) # EXTRACT DIAGONAL for ROBUST STANDARD ERRROR
COEFFS <- MODEL_1.1$coefficients # EXTRACT COEFFICIENTS
Z_SCOR_ROBUST <- COEFFS / STD_ERR_ROBUST # Z SCORES
P_VAL_ROBUST <- 2 * pnorm(abs(Z_SCOR_ROBUST), lower.tail = FALSE)# P VALUES
ODDRATIO <- exp(COEFFS)# ODDRATIO
CI_LOW <- exp(COEFFS - 1.96 * STD_ERR_ROBUST) # CONFIDENCE INTERVALS
CI_HIGH <- exp(COEFFS + 1.96 * STD_ERR_ROBUST)# CONFIDENCE INTERVALS
PVAL_STAR <- case_when(P_VAL_ROBUST <=  0.001  ~ "***",P_VAL_ROBUST >  0.001 & P_VAL_ROBUST <= 0.01 ~ "**",P_VAL_ROBUST >  0.01 & P_VAL_ROBUST <= 0.05 ~ "*",P_VAL_ROBUST >  0.05 & P_VAL_ROBUST <= 0.1 ~ ".",TRUE ~ "")
MCFADDEN <- pR2(MODEL_1.1)['McFadden']
CHI2 <- with(MODEL_1.1, null.deviance - deviance)
SIG <- with(MODEL_1.1, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
NAGLE <- rsqs_out$Nagelkerke
AKAIKE<- AIC(MODEL_1.1)
MARG_1.1 <- logitmfx(formula = form, data = HIVE_SUB) 
margeffect <- data.frame(dF_dx = c(0, MARG_1.1$mfxest[, "dF/dx"])) # marginal effects dataframe with empty row added to top

# RESULTS IN A TABLE
TABLE_MODEL_1.1 <- data.frame(
  coeff = paste(sprintf("%.2f", COEFFS), PVAL_STAR, sep = " "),
  marginal.effects = margeffect,
  oddratio = ODDRATIO,
  ci = paste("(", round(CI_LOW, 2), ",", round(CI_HIGH, 2), ")"),
  std.err = STD_ERR_ROBUST,
  z.scors = Z_SCOR_ROBUST,
  p.value = P_VAL_ROBUST,
  sign = PVAL_STAR,
  n = nrow(HIVE_SUB),
  Chi2 = CHI2,
  Sig = SIG,
  McFaddenR2 = MCFADDEN,
  Naglekerke = NAGLE,
  AIC = AKAIKE
)

write.xlsx(TABLE_MODEL_1.1, file = paste0(name, "Model_1-1",  ".xlsx"), sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


###############################################################################
#                               Equation 1.2
#################################################################################
summary(MODEL_1.1)

VARSLIST <- c("City",
              #"NDVI_20_50",
              #"Hghwy_len_Binary",
              #"Own_mean",
              "NDVI_50_100",
              "O3", 
              "Place",  
              "HvCnt_Cat", 
              "Year_Cat")

form <- as.formula(paste("Status_Binary ~", paste(VARSLIST, collapse = " + ")))
MODEL_1.2 <- glm(formula = form,family = binomial,data = HIVE_SUB);summary(MODEL_1.2)


# Outliers
COOK_DIST <- data.frame(dist = cooks.distance(MODEL_1.2),oid = 1:nrow(HIVE_SUB))
Outliers_List <- subset(HIVE_SUB, COOK_DIST$dist>=0.002)$HiveID # criteria (8/n) = 0.002
HIVE_SUB2 <- subset(HIVE_SUB, !(HiveID %in% Outliers_List))

# NO OUTLIERS
MODEL_1.2 <- glm(formula = form,family = binomial,data = HIVE_SUB2);summary(MODEL_1.2)

pR2(MODEL_1.2)['McFadden']
vif(MODEL_1.2)

# Null model
MODEL_NULL <- glm(Status_Binary ~1,family = binomial(link="logit"),data = HIVE_SUB2)

rsqs_out<- RSQS(loglike.full = as.numeric(logLik(MODEL_1.2)), # loglikelihood of complete model 
             loglike.null = as.numeric(logLik(MODEL_NULL)), # loglikelihood of null model
             full.deviance = deviance(MODEL_1.2), # deviance of complete model 
             null.deviance = deviance(MODEL_NULL), # deviance of null model
             nb.params = MODEL_1.2$rank, # number of parameters in complete model 
             n = nrow(HIVE_SUB2)) # number of observations

# PREPARE TABLE
COV_MODEL_1.2<- vcovHC(MODEL_1.2, type = "HC0") # HCO METHOD BASED ON RESIDUALS
STD_ERR_ROBUST <- sqrt(diag(COV_MODEL_1.2)) # EXTRACT DIAGONAL for ROBUST STANDARD ERRROR
COEFFS <- MODEL_1.2$coefficients # EXTRACT COEFFICIENTS
Z_SCOR_ROBUST <- COEFFS / STD_ERR_ROBUST # Z SCORES
P_VAL_ROBUST <- 2 * pnorm(abs(Z_SCOR_ROBUST), lower.tail = FALSE)# P VALUES
ODDRATIO <- exp(COEFFS)# ODDRATIO
CI_LOW <- exp(COEFFS - 1.96 * STD_ERR_ROBUST) # CONFIDENCE INTERVALS
CI_HIGH <- exp(COEFFS + 1.96 * STD_ERR_ROBUST)# CONFIDENCE INTERVALS
PVAL_STAR <- case_when(P_VAL_ROBUST <=  0.001  ~ "***",P_VAL_ROBUST >  0.001 & P_VAL_ROBUST <= 0.01 ~ "**",P_VAL_ROBUST >  0.01 & P_VAL_ROBUST <= 0.05 ~ "*",P_VAL_ROBUST >  0.05 & P_VAL_ROBUST <= 0.1 ~ ".",TRUE ~ "")
CHI2 <- with(MODEL_1.2, null.deviance - deviance)
SIG <- with(MODEL_1.2, pchisq(null.deviance - deviance, df.null - df.residual, lower.tail = FALSE))
MCFADDEN <- pR2(MODEL_1.2)['McFadden']
NAGLE <- rsqs_out$Nagelkerke
AKAIKE<- AIC(MODEL_1.2)
MARG_1.2 <- logitmfx(formula = form, data = HIVE_SUB2)
margeffect <- data.frame(dF_dx = c(0, MARG_1.2$mfxest[, "dF/dx"])) # marginal effects dataframe with empty row added to top

TABLE_MODEL_1.2 <- data.frame(
  coeff = paste(sprintf("%.2f", COEFFS), PVAL_STAR, sep = " "),
  marginal.effects = margeffect,
  oddratio = ODDRATIO,
  ci = paste("(", round(CI_LOW, 2), ",", round(CI_HIGH, 2), ")"),
  std.err = STD_ERR_ROBUST,
  z.scors = Z_SCOR_ROBUST,
  p.value = P_VAL_ROBUST,
  sign = PVAL_STAR,
  n = nrow(HIVE_SUB2),
  Chi2 = CHI2,
  Sig = SIG,
  McFaddenR2 = MCFADDEN,
  Naglekerke = NAGLE,
  AIC = AKAIKE
)

write.xlsx(TABLE_MODEL_1.2, file = paste0(name, "Model_1-2",  ".xlsx"), sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)


##############################################################################
#             ASSUMPTIONS
###########################################################################
HIVE_SUB2$NDVI_50_100_1 <-((HIVE_SUB2$NDVI_50_100)+0.01)
HIVE_SUB2$NDVI_20_50_1 <-((HIVE_SUB2$NDVI_20_50)+0.01)

# Box Tidwell Test 

boxTidwell(formula = Status_Binary ~
             NDVI_50_100_1 +
             NDVI_20_50_1 +
             Own_mean +
             O3,
           other.x = ~ 
             HvCnt_Cat +
             MedInc_Qrt +
             City +
             Place +
             Year_Cat,
           data = HIVE_SUB2)


# CALCUL RESIDUS SIMULES
PROBS <- predict(MODEL_1.2, type = "response") # extract probabilities predicted by the model
SIMS <- lapply(1:length(PROBS), function(i){    #Calculate 1000 simulations based on adjusted model
  P <- PROBS[[i]]
  VALS <- rbinom(n = 1000, size = 1,prob = P)})
MATSIM <- do.call(rbind,SIMS) # put into matrix
RESIDUS_SIM <- createDHARMa(simulatedResponse = MATSIM,  
                            observedResponse = HIVE_SUB2$Status_Binary,
                            fittedPredictedResponse = PROBS,
                            integerResponse = T)
plot(RESIDUS_SIM)
ggplot()+
  geom_histogram(aes(x = residuals(RESIDUS_SIM)),
                 bins = 30, fill = "white", color = rgb(0.3,0.3,0.3))



# plot Deviance residuals vs Year
dev_resid <- residuals(MODEL_1.2, type = "deviance")
my_plot <- ggplot(data = HIVE_SUB2, aes(x = Year))
my_plot + geom_point(aes(y = dev_resid ))


# SPATIAL AUTOCORRELATION ON Y
HIVE_SUB2_SP <- HIVE_SUB2
coords <- HIVE_SUB2[, c("Lng", "Lat")]
coordinates(HIVE_SUB2_SP) <- coords
proj4string(HIVE_SUB2_SP) <- CRS("+proj=longlat +datum=WGS84")
HIVE_mtm8 <- spTransform(HIVE_SUB2_SP, CRS("+proj=tmerc +lat_0=0 +lon_0=-73.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# plot(HIVE_mtm8)
PointsXY <- coordinates(HIVE_mtm8)
distmat <- as.matrix(dist(PointsXY))
diag(distmat) <- 0
# distance matrixes
DistMat2Km <- ifelse(distmat<= 2000 & distmat!=0, 1/distmat, 0)
W_DistMat2Km <- mat2listw(DistMat2Km, style="M")                   
W_DistMat2Km <- nb2listw(W_DistMat2Km$neighbours,style="W",zero.policy = TRUE)

moran.test(MODEL_1.2$residuals, 
           W_DistMat2Km, 
           alternative="two.sided",  # two-sided tests for both positive and negative spatial autocorrelation
           zero.policy = T) # TRUE = observations with missing values are included in the analysis. If set to FALSE, they are excluded.


################################################################################
#   DESCRIPTIVE STATS
################################################################################
# Hive status as numeric
HIVE_SUB$Status_numeric <- ifelse(HIVE_SUB$Status == "Healthy", 1, 0)
VARSLIST <- c("NDVI_0_20", "NDVI_20_50", "NDVI_50_100", "Water_Pct" ,"Build_Pct", "Hghwy_len","MedInc_mean",
              "Smoke", "O3" , "Own_mean", "HvCnt", "Status_numeric")

DESCSTAT <- data.frame(Min = sapply(HIVE_SUB[VARSLIST], min, na.rm = TRUE),
                       Max = sapply(HIVE_SUB[VARSLIST], max, na.rm = TRUE),
                       Mean = sapply(HIVE_SUB[VARSLIST], mean, na.rm = TRUE),
                       StdDev = sapply(HIVE_SUB[VARSLIST], sd, na.rm = TRUE)
)

write.xlsx(DESCSTAT, file = paste0("DESCSTATS_1km",".xlsx"), sheetName = "Sheet1", col.names = TRUE, row.names = TRUE, append = FALSE)

ggplot(HIVE_SUB, aes(x = Status)) +
  stat_count()

table(HIVE_SUB$Status)
table(HIVE_SUB$Water_Bi)
table(HIVE_SUB$Hghwy_len_Binary)
table(HIVE_SUB$Place)
table(HIVE_SUB$City)
table(HIVE_SUB$HvCnt_Cat)


