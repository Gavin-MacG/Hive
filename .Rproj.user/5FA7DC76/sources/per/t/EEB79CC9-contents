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

load(file = "DataTreated/Buff_1km_HIVE_Sub11.rda")
# load(file = "DataTreated/Buff_3km_HIVE_Sub11.rda")

###############################################################################
#  Select data to process  ---------------------------------
################################################################################

# # Subset cities if needed
# HIVE_Sub1 <- filter(HIVE_Sub1,City == "MTL")
# HIVE_Sub1 <- filter(HIVE_Sub1,City == "TO")

# Set naming for export
export_name <- c("Buff_1km")

################################################################################
# Descriptive statistics (continuous variables) --------------------------------------------------
################################################################################

# Create a numeric Hive Status variable
HIVE_Sub1$Status_numeric <- ifelse(HIVE_Sub1$Status == "Healthy", 1, 0)

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

# calculate descriptive statistics
DescStat <- data.frame(
  Min = sapply(HIVE_Sub1[VariableList], function(x) round(min(x, na.rm = TRUE), 2)),
  Max = sapply(HIVE_Sub1[VariableList], function(x) round(max(x, na.rm = TRUE), 2)),
  Mean = sapply(HIVE_Sub1[VariableList], function(x) round(mean(x, na.rm = TRUE), 2)),
  StdDev = sapply(HIVE_Sub1[VariableList], function(x) round(sd(x, na.rm = TRUE), 2))
)

# Export
write.xlsx(DescStat, file = paste0("Outputs/",export_name,"_DescStats",".xlsx"), 
           sheetName = "Sheet1", 
           colNames = TRUE, 
           rowNames = TRUE, 
           append = FALSE)



################################################################################
# Descriptive statistics (binary variables) --------------------------------------------------
################################################################################

ggplot(HIVE_Sub1, aes(x = Status)) +
  stat_count()

BinaryList <- lapply(HIVE_Sub1[c("Status", 
                                 "Water_Bi", 
                                 "Hghwy_Bi", 
                                 "Place", 
                                 "City", 
                                 "HvCnt_Cat")], table)

# Convert each table to a dataframe and add a variable name
binary_df_list <- lapply(names(BinaryList), function(name) {
  df <- as.data.frame(BinaryList[[name]])
  colnames(df) <- c("Value", "Count")
  df$Variable <- name
  df <- df[, c("Variable", "Value", "Count")] # Reorder columns
  df
})

# Combine all dataframes into one
DescStat_Bi <- do.call(rbind, binary_df_list)

# Export 
write.xlsx(DescStat_Bi, file = paste0("Outputs/",export_name,"_DescStats_Binary",".xlsx"), 
           sheetName = "Sheet1", 
           colNames = TRUE, 
           rowNames = TRUE, 
           append = FALSE)

