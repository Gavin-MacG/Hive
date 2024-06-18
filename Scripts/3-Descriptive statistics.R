# This script uses data pulled from buffers placed around honeybee hives in Montreal and Toronto :
# Produces a table of descriptive statistics

#################################################################
# Install packages and load data ------------------------------------------
#################################################################

required_packages <- c("tidyverse",
                       "openxlsx",) 

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

################################################################################
# Descriptive statistics --------------------------------------------------
################################################################################

# HCreate a numeric Hive Status variable
HIVE_Sub$Status_numeric <- ifelse(HIVE_Sub$Status == "Healthy", 1, 0)

VariableList <- c("NDVI_0_20", 
                  "NDVI_20_50", 
                  "NDVI_50_100", 
                  "Water_Pct" ,
                  "Build_Pct", 
                  "Hghwy_len",
                  "MedInc_mean",
                  "Smoke", 
                  "O3" , 
                  "Own_mean", 
                  "HvCnt", 
                  "Status_numeric")

DESCSTAT <- data.frame(Min = sapply(HIVE_Sub[VariableList], min, na.rm = TRUE),
                       Max = sapply(HIVE_Sub[VariableList], max, na.rm = TRUE),
                       Mean = sapply(HIVE_Sub[VariableList], mean, na.rm = TRUE),
                       StdDev = sapply(HIVE_Sub[VariableList], sd, na.rm = TRUE)
)

write.xlsx(DESCSTAT, file = paste0("Outputs/",export_name,"_DescStats",".xlsx"), 
           sheetName = "Sheet1", 
           colNames = TRUE, 
           rowNames = TRUE, 
           append = FALSE)

ggplot(HIVE_Sub, aes(x = Status)) +
  stat_count()

table(HIVE_Sub$Status)
table(HIVE_Sub$Water_Bi)
table(HIVE_Sub$Hghwy_len_Binary)
table(HIVE_Sub$Place)
table(HIVE_Sub$City)
table(HIVE_Sub$HvCnt_Cat)