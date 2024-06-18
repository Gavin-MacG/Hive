# This script uses data pulled from buffers placed around honeybee hives in Montreal and Toronto :
# - Creates a first GLM logistic regression model on a binary Hive health variable
# - Evaluates and removes outliers identified in the first model at 8/n criteria
# - Creates a second GLM logistic model containing only the significant variables
# - Evaluates and removes outliers identified in the second model at 8/n criteria
# - Performs the assumption tests for the models
# - Checks for spatial autocorrelation
# - Exports the resulting models in two excel table

#################################################################
# Install packages and load data ------------------------------------------
#################################################################

required_packages <- c("tidyverse", 
                       "openxlsx",
                       "car",      # vif and box-tidwell tests
                       "sandwich", # standard error and robust values
                       "pscl",     # McFadden R2
                       "mfx",      # to calculate marginal effects
                       "DHARMa",   # simulated residuals analysis
                       "spdep",    # for spatial dependency analysis
                       "sp",       # for spatial objects
                       "pgirmess", # correlog function for spatial correlogram
                       "spfilteR", # glmFilter to filter for spatial autocorrelation
                       "betareg",
                       "rgdal"
                       ) 

for(Package in required_packages){
  if(!require(Package,character.only = TRUE)) { 
    install.packages(Package, dependencies=TRUE)
  }
  library(Package,character.only = TRUE)
}

load(file = "DataTreated/Hive_1km_Buffers.rda")
# load(file = "DataTreated/Hive_3km_Buffers.rda")

###############################################################################
#  Select data to process  ---------------------------------
################################################################################

# Choose dataset (1km or 3km)
HIVE <- Hive1Km

# # Subset cities if needed
# HIVE <- filter(HIVE,City == "MTL")
# HIVE <- filter(HIVE,City == "TO")

# Set naming for export
export_name <- c("Buff_1km")

###############################################################################
#  Function to create output table ---------------------------------
################################################################################

create_table <- function(model, data, form_x) {
  # Number of observations and parameters
  n_obs <- nrow(data)
  n_par <- model$rank
  
  # Null model
  model_null <- glm(Status_Bi ~1,family = binomial(link="logit"),data = data)
  
  # Log-likelihoods
  Loglike.full <- as.numeric(logLik(model))
  Loglike.null <- as.numeric(logLik(model_null))
  
  # Deviance
  Deviance.full <- deviance(model)
  Deviance.null <- deviance(model_null)
  
  # Degrees of freedom
  DFree <- model_null$df.residual - model$df.residual
  
  # Explained deviance
  Explained_dev <- 1 - (Deviance.full / Deviance.null)
  
  # Pseudo R-squared values
  Mcfadden <- pR2(model)['McFadden']
  Mcfadden_adj <- 1 - (Loglike.full - n_par) / Loglike.null
  Cox_snell <- 1 - exp((-2 / n_obs) * (Loglike.full - Loglike.null))
  Nagelkerke <- Cox_snell / (1 - exp(2 * Loglike.null / n_obs))
  
  # Chi-squared and significance
  Chi2 <- Deviance.null - Deviance.full
  Significance <- pchisq(Chi2, DFree, lower.tail = FALSE)
  
  # Robust standard errors
  Std_Err_Robust <- sqrt(diag(vcovHC(model, type = "HC0")))
  
  # Coefficients, odds ratios and marginal effects
  Coeffs <- model$coefficients 
  OddsRatios <- exp(Coeffs)
  Marg_1.2 <- logitmfx(formula = form_x, data = data) 
  MarginalEffects <- data.frame(dF_dx = c(0, Marg_1.2$mfxest[, "dF/dx"])) # marginal effects dataframe with empty row added to top
  
  # Z-scores and P-Values
  Z_Score_Robust <- Coeffs / Std_Err_Robust 
  PVal_Robust <- 2 * pnorm(abs(Z_Score_Robust), lower.tail = FALSE)
  PVal_Stars <- case_when(
    PVal_Robust <= 0.001 ~ "***",
    PVal_Robust > 0.001 & PVal_Robust <= 0.01 ~ "**",
    PVal_Robust > 0.01 & PVal_Robust <= 0.05 ~ "*",
    PVal_Robust > 0.05 & PVal_Robust <= 0.1 ~ ".",
    TRUE ~ ""
  )
  
  # Confidence intervals
  CI_Low <- exp(Coeffs - 1.96 * Std_Err_Robust)
  CI_High <- exp(Coeffs + 1.96 * Std_Err_Robust)
  
  # Create dataframe for model output
  output_df <- data.frame(
    coeff = paste(sprintf("%.2f", Coeffs), PVal_Stars, sep = " "),
    marginal.effects = round(MarginalEffects$dF_dx, 2),
    oddratio = round(OddsRatios, 2),
    ci = paste("(", round(CI_Low, 2), ",", round(CI_High, 2), ")", sep = ""),
    std.err = round(Std_Err_Robust, 2),
    z.score = round(Z_Score_Robust, 2),
    p.value = round(PVal_Robust, 2),
    sign = PVal_Stars,
    n = n_obs,
    Chi2 = round(Chi2, 2),
    Sig = round(Significance, 3),
    McFaddenR2 = round(Mcfadden, 2),
    Naglekerke = round(Nagelkerke, 2),
    AIC = round(AIC(model), 2),
    Mcfadden_adj = round(Mcfadden_adj,2),
    Cox_snell = round(Cox_snell,2)
  )
  
  return(output_df)
}

################################################################################
################################################################################

# Model_1 (full variables list) ----------------------------------------------

################################################################################
################################################################################

# Prepare variables for the formula
VariableList <- c("NDVI_50_100*City",  # Interaction with City
                  "NDVI_20_50",
                  "Water_Bi" ,
                  "Hghwy_Bi",
                  "Smoke",
                  "O3" ,
                  "Own_mean", 
                  "MedInc_Qrt",
                  "Place" ,  
                  "HvCnt_Cat", 
                  "Year_Cat")

form_1 <- as.formula(paste("Status_Bi ~", paste(VariableList, collapse = " + ")))

######################################################################
# Model_1 - GLM binominal logit ---------------------------------------------------------
######################################################################

MODEL_1.1 <-
  glm(formula = form_1,
      family = binomial,
      data = HIVE) ; summary(MODEL_1.1)

######################################################################
# Model_1 - GLM binominal logit (without outliers) ---------------------------------------------------------
######################################################################

# Calculate and remove outliers
CooksDist <- data.frame(dist = cooks.distance(MODEL_1.1), oid = 1:nrow(HIVE))
Outliers_List <- subset(HIVE, CooksDist$dist >= 0.002)$HiveID # criteria (8/n) = 0.002
HIVE_Sub1 <- subset(HIVE,!(HiveID %in% Outliers_List))

MODEL_1.2 <-
  glm(formula = form_1,
      family = binomial,
      data = HIVE_Sub1) ; summary(MODEL_1.2)

######################################################################
# Model_1 - Assumption tests ---------------------------------------------------------
######################################################################

# Check multicollinearity
vif(MODEL_1.2)

# Check independance of observations
summary_model <- summary(MODEL_1.2)
summary_dev_res <- summary_model$deviance.resid
quantile(summary_dev_res)
HIVE_Sub1 <- HIVE_Sub1[order(HIVE_Sub1$Year),] # Sort the dataset by HIVE_Sub$Year
HIVE_Sub1$Index_Number <- 1:nrow(HIVE_Sub1) # Create HIVE_Sub$Index_Number to represent the order of observations in the dataset

ggplot(data = data.frame(x = HIVE_Sub1$Index_Number,
                         y = scale(resid(MODEL_1.2, type = 'deviance'))),
       aes(x = x, y = y)) +
  geom_line() +
  ggtitle("Residual Series Plot") +
  xlab("Index Number") +
  ylab("Deviance Residuals") +
  geom_hline(yintercept = 0,
             linetype = "dashed",
             color = "red")

######################################################################
# Model_1 - Export table  ---------------------------------------------------------
######################################################################

# Apply the table creation function
TABLE_1.2 <- create_table(MODEL_1.2, HIVE_Sub1, form_1)

# Define the custom order for the row names
custom_order <- c("(Intercept)", "CityTO", "NDVI_50_100:CityTO", "NDVI_20_50", "NDVI_50_100", 
                  "Water_Bi", "Hghwy_Bi", "Smoke", "O3", "Own_mean", "MedInc_Qrt2", 
                  "MedInc_Qrt3", "MedInc_Qrt4", "PlaceRoof", "HvCnt_Cat<5", "HvCnt_Cat<10", 
                  "HvCnt_Cat<50", "HvCnt_Cat50+", "Year_Cat2017", "Year_Cat2018", 
                  "Year_Cat2020", "Year_Cat2021", "Year_Cat2022")

# Reorder the rows based on the custom order
TABLE_1.2 <- TABLE_1.2[match(custom_order, rownames(TABLE_1.2)), ]

# Export table
write.xlsx(TABLE_1.2, file = paste0("Outputs/",export_name, "_Model_1-2",  ".xlsx"), 
           sheetName = "Sheet1", 
           colNames = TRUE, 
           rowNames = TRUE, 
           append = FALSE)

################################################################################
################################################################################

# Model_2 (reduced variables list) ----------------------------------------------

################################################################################
################################################################################

summary(MODEL_1.2)

# Prepare variables for the formula
VariableList <- c("City",
                  "NDVI_50_100",
                  "O3", 
                  "Place",  
                  "HvCnt_Cat", 
                  "Year_Cat")

form_2 <- as.formula(paste("Status_Bi ~", paste(VariableList, collapse = " + ")))

######################################################################
# Model_2 - GLM binominal logit ---------------------------------------------------------
######################################################################

MODEL_2.1 <-
  glm(formula = form_2,
      family = binomial,
      data = HIVE_Sub1) ; summary(MODEL_2.1)

######################################################################
# Model_2 - GLM binominal logit (no outliers) ---------------------------------------------------------
######################################################################

CooksDist <- data.frame(dist = cooks.distance(MODEL_2.1),oid = 1:nrow(HIVE_Sub1))
Outliers_List <- subset(HIVE_Sub1, CooksDist$dist>=0.002)$HiveID # criteria (8/n) = 0.002
HIVE_Sub2 <- subset(HIVE_Sub1, !(HiveID %in% Outliers_List))

MODEL_2.2 <-
  glm(formula = form_2,
      family = binomial,
      data = HIVE_Sub2) ; summary(MODEL_2.2)

######################################################################
# Model_2 - Assumption tests ---------------------------------------------------------
######################################################################

# Check multicollinearity
vif(MODEL_2.2)

# Make sure there are no zero values for NDVI
HIVE_Sub2$NDVI_50_100_1 <-((HIVE_Sub2$NDVI_50_100)+0.01)
HIVE_Sub2$NDVI_20_50_1 <-((HIVE_Sub2$NDVI_20_50)+0.01)

# Box-Tidwell test : Linearity between the predictors and the logit
# Note: this test can only handle three variables at a time, adjust accordingly
boxTidwell(formula = Status_Bi ~
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
           data = HIVE_Sub2)


# Simulated residuals
ProbsPred <- predict(MODEL_2.2, type = "response") # extract probabilities predicted by the model
Sims <- lapply(1:length(ProbsPred), function(i){    #Calculate 1000 simulations based on adjusted model
  P <- ProbsPred[[i]]
  Vals <- rbinom(n = 1000, size = 1,prob = P)})
SimMatrix <- do.call(rbind,Sims) # put into matrix
SimResid <- createDHARMa(simulatedResponse = SimMatrix,  
                            observedResponse = HIVE_Sub2$Status_Bi,
                            fittedPredictedResponse = ProbsPred,
                            integerResponse = T)
plot(SimResid)
ggplot()+
  geom_histogram(aes(x = residuals(SimResid)),
                 bins = 30, 
                 fill = "white", 
                 color = rgb(0.3,0.3,0.3))

# plot Deviance residuals vs Year
dev_resid <- residuals(MODEL_2.2, type = "deviance")
my_plot <- ggplot(data = HIVE_Sub2, aes(x = Year_Cat))
my_plot + geom_point(aes(y = dev_resid ))

############################################################
# Spatial autocorrelation tests -------------------------------------------
###########################################################

# WARNING :
# These tests will not work with the anonymized data
# that is made available on my Github

HIVE_Sub2_SP <- HIVE_Sub2
coords <- HIVE_Sub2[, c("Lng", "Lat")]
coordinates(HIVE_Sub2_SP) <- coords
proj4string(HIVE_Sub2_SP) <- CRS("+proj=longlat +datum=WGS84")
HIVE_mtm8 <- spTransform(HIVE_Sub2_SP, CRS("+proj=tmerc +lat_0=0 +lon_0=-73.5 +k=0.9999 +x_0=304800 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"))
# Plot(HIVE_mtm8)
PointsXY <- coordinates(HIVE_mtm8)
distmat <- as.matrix(dist(PointsXY))
diag(distmat) <- 0
# Distance matrixes
DistMat2Km <- ifelse(distmat<= 2000 & distmat!=0, 1/distmat, 0)
W_DistMat2Km <- mat2listw(DistMat2Km, style="M")                   
W_DistMat2Km <- nb2listw(W_DistMat2Km$neighbours,style="W",zero.policy = TRUE)

# Morans I test
moran.test(MODEL_2.2$residuals, 
           W_DistMat2Km, 
           alternative="two.sided",  # two-sided tests for both positive and negative spatial autocorrelation
           zero.policy = T) # TRUE = observations with missing values are included in the analysis. If set to FALSE, they are excluded.

######################################################################
# Model_2 - Export table ---------------------------------------------------------
######################################################################

# Apply the table creation function
TABLE_2.2 <- create_table(MODEL_2.2, HIVE_Sub2, form_2)

# Define the custom order for the row names
custom_order2 <- c("(Intercept)", "CityTO", "NDVI_50_100", 
                  "O3", "PlaceRoof", "HvCnt_Cat<5", "HvCnt_Cat<10", 
                  "HvCnt_Cat<50", "HvCnt_Cat50+", "Year_Cat2017", "Year_Cat2018", 
                  "Year_Cat2020", "Year_Cat2021", "Year_Cat2022")

# Reorder the rows based on the custom order
TABLE_2.2 <- TABLE_2.2[match(custom_order2, rownames(TABLE_2.2)), ]

write.xlsx(TABLE_2.2, file = paste0("Outputs/",export_name, "_Model_2-2",  ".xlsx"), 
           sheetName = "Sheet1", 
           colNames = TRUE, 
           rowNames = TRUE, 
           append = FALSE)

