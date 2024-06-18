library(dplyr)
library(tidyr)
library(xlsx)
library(readxl)
library(BAMMtools) # jenks breaks function
library(ggplot2)

rm(list=ls())
setwd("C:/_MY FOLDERS/RECHERCHE/HIVE")

# (n = 456)
MTL <- read.csv("BUFF_MTL.csv", 
                  header = TRUE, sep = ",", dec = ".",
                  stringsAsFactors = FALSE)

# (n = 291)
TO <- read.csv("BUFF_TO.csv", 
                header = TRUE, sep = ",", dec = ".",
                stringsAsFactors = FALSE)

###############################################################################
#                                    MERGE BUFFERS
###############################################################################

MTL <- MTL %>%  transmute(
  CliID = CliID,
  City = City,
  Water_Pct = Water_pc/100,
  NDVI_mean = NDVI_mean,
  Canopy_Pct = Canopy_pc/100,
  Tree_Nec = Tree_Nec,
  Tree_Pol = Tree_Pol,
  Tree_NP = Tree_NP,
  Build_Pct = Build_pc/100,
  Hghwy_len = Hghwy_len,
  ArtTot_len = Art1_len + Art2_len,
  NO2_mean = NO2_mean,
  AQI = AQI,
  O3 = O3,
  Smoke = Smoke,
  PM25_mean = PM25_mean,
  BckYrdDup_mean = BckYrdDup_,
  BckYrd_mean = BckYrd_mea,
  Own_mean = Own_mean,
  MedInc_mean = MedInc_mea,
  HvCnt_Full = HvCnt_17 + HvCnt_18 + HvCnt_19 + HvCnt_20 + HvCnt_21 + HvCnt_22 ,
  HvCnt_22 = HvCnt_22,
  HvCnt_21 = HvCnt_21,
  HvCnt_20 = HvCnt_20,
  HvCnt_19 = HvCnt_19,
  HvCnt_18 = HvCnt_18,
  HvCnt_17 = HvCnt_17,
  Nobuild_Pct = NoBuild_pc/100,
  NoTree_Pct = NoTree_pc/100,
  NDVI_0_20 =  NDVI_0_2_1 / NDVI_0_20_,
  NDVI_20_50 =  NDVI_20__1 / NDVI_0_20_,
  NDVI_50_100 =  NDVI_50__1 / NDVI_0_20_
)

TO <- TO %>%  transmute(
  CliID = CliID,
  City = City,
  Water_Pct = Water_pc/100,
  NDVI_mean = NDVI_mean,
  Canopy_Pct = Canopy_pc/100,
  Tree_Nec = Tree_Nec,
  Tree_Pol = Tree_Pol,
  Tree_NP = Tree_NP,
  Build_Pct = Build_pc/100,
  Hghwy_len = Hghwy_len,
  ArtTot_len = Art1_len,
  NO2_mean = NO2_mean,
  AQI = AQI,
  O3 = O3,
  Smoke = Smoke,
  PM25_mean = PM25_mean,
  BckYrdDup_mean = BckYrdDup_,
  BckYrd_mean = BckYrd_mea,
  Own_mean = Own_mean,
  MedInc_mean = MedInc_mea,
  HvCnt_Full = HvCnt_17 + HvCnt_18 + HvCnt_19 + HvCnt_20 + HvCnt_21 + HvCnt_22 ,
  HvCnt_22 = HvCnt_22,
  HvCnt_21 = HvCnt_21,
  HvCnt_20 = HvCnt_20,
  HvCnt_19 = HvCnt_19,
  HvCnt_18 = HvCnt_18,
  HvCnt_17 = HvCnt_17,
  Out_Pct = Out_pc/100,
  NDVI_0_20 =  NDVI_0_2_1 / NDVI_0_20_,
  NDVI_20_50 =  NDVI_20__1 / NDVI_0_20_,
  NDVI_50_100 =  NDVI_50__1 / NDVI_0_20_
)

########   Proportional ajustement for buffer areas outside study bounds ##########

MTL$Tree_Nec <- MTL$Tree_Nec / (1 - MTL$NoTree_Pct)
MTL$Tree_Pol <- MTL$Tree_Pol / (1 - MTL$NoTree_Pct)
MTL$Tree_NP <- MTL$Tree_NP / (1 - MTL$NoTree_Pct)
MTL$Build_Pct<- MTL$Build_Pct *(1 - MTL$Nobuild_Pct)

# Remove 6 NA observations
MTL$Tree_NP[is.na(MTL$Tree_NP)] <- 0
MTL$Tree_Nec[is.na(MTL$Tree_Nec)] <- 0
MTL$Tree_Pol[is.na(MTL$Tree_Pol)] <- 0

TO$Tree_Nec <- TO$Tree_Nec / (1 - TO$Out_Pct)
TO$Tree_Pol <- TO$Tree_Pol / (1 - TO$Out_Pct)
TO$Tree_NP <- TO$Tree_NP / (1 - TO$Out_Pct)
TO$Canopy_Pct <- TO$Canopy_Pct *(1 - TO$Out_Pct)
TO$Build_Pct <- TO$Build_Pct *(1 - TO$Out_Pct)

# change city name
MTL$City <- 'MTL'
TO$City <- 'TO'

BUFF <-bind_rows(MTL, TO)


#############################################################################
#                         PREPARE FINAL DATA
#############################################################################

HIVE_Cases <- read.csv("C:/_MY FOLDERS/RECHERCHE/HIVE/SOURCE/HIVE_Cases.csv", 
                       header = TRUE, sep = ",", dec = ".",
                       stringsAsFactors = FALSE)

# take out city row 
HIVE_Cases <- HIVE_Cases %>% select(-City)

# Reverse X and Y in this function when dataset is complete
HIVE_joined <- left_join(HIVE_Cases, BUFF, by = "CliID")
# Remove clients outside study area
HIVE <- HIVE_joined[complete.cases(HIVE_joined$City),]

################################################################################
#                                 HIVE COUNT
################################################################################

HIVE$HvCnt <- ifelse(HIVE$Year == 2017, HIVE$HvCnt_17,
                     ifelse(HIVE$Year == 2018, HIVE$HvCnt_18,
                            ifelse(HIVE$Year == 2019, HIVE$HvCnt_19,
                                   ifelse(HIVE$Year == 2020, HIVE$HvCnt_20,
                                          ifelse(HIVE$Year == 2021, HIVE$HvCnt_21,
                                                 HIVE$HvCnt_22)))))

HIVE <- HIVE[, !names(HIVE) %in% c("HvCnt_17", "HvCnt_18", "HvCnt_19", "HvCnt_20", "HvCnt_21", "HvCnt_22")]

summary(HIVE$HvCnt)


# Hive Count Jenks
getJenksBreaks(HIVE$HvCnt, 6, subset = NULL)
HIVE$HvCnt_Jenks <- as.factor(ifelse(HIVE$HvCnt < 3, '<2', 
                                     ifelse(HIVE$HvCnt < 6, '<5',
                                            ifelse(HIVE$HvCnt < 11, '<10',
                                        
                                                          ifelse(HIVE$HvCnt < 51, '<50',"50+")))))

HIVE$HvCnt_Jenks <- factor(HIVE$HvCnt_Jenks, levels = c("<2",'<5', '<10',  '<50', "50+" ))
table(HIVE$HvCnt_Jenks)

quantile(HIVE$HvCnt)

HIVE$HvCnt_log <- log(HIVE$HvCnt)
HIVE$HvCnt_log2 <- log2(HIVE$HvCnt)
ggplot(HIVE, aes(x = HvCnt)) +
  stat_count()

###############################################################################
#                          Variable creation
###############################################################################

#Create binary Status variable
HIVE$Status_Binary <- ifelse(HIVE$Status == 'Dead', 0, 1)

# Create Median Income categorial by City
HIVE <- HIVE %>%
  arrange(City, MedInc_mean) %>%
  group_by(City) %>%
  mutate(MedInc_Qrt = ntile(MedInc_mean, 4)) %>%
  ungroup()

# Create Median Income first quartile variable 
HIVE$MedInc_Q1 <- ifelse(HIVE$MedInc_Qrt == 1, 1, 0)

#Create Water_Dummy (To allow for Tidwell test)
summary(HIVE$Water_Pct)
HIVE$Water_Bi <- ifelse(HIVE$Water_Pct >= 0.00076, 1, 0)
HIVE$Water_Pct_1 <-((HIVE$Water_Pct)+0.01)

#Added 0.01 to remove 0 value (to allow for Box-Tidwell test)
HIVE$Build_Pct_1 <-((HIVE$Build_Pct)+0.01)

#Create binary Highway length variable (To allow for Tidwell test)
summary(HIVE$Hghwy_len)
HIVE$Hghwy_len_Binary <- ifelse(HIVE$Hghwy_len >= 2692, 1, 0)

# Tree as integer
HIVE$Tree_NP <- as.integer(HIVE$Tree_NP)


###############################################################################
# Anonymize data ----------------------------------------------------------
###############################################################################

# Extract unique HiveID
unique_HiveID <- unique(HIVE$HiveID)

# Assign a unique number to each unique HiveID
HiveID_mapping <- data.frame(
  HiveID = unique_HiveID,
  NewHiveID = 1:length(unique_HiveID)
)

HIVE <- HIVE %>%
  left_join(HiveID_mapping, by = "HiveID") %>%
  select(-HiveID) %>%
  rename(HiveID = NewHiveID) %>% 
  select(HiveID, everything())

# Remove Place Unknown
HIVE <- HIVE[HIVE$Place != "Unknown", ]

# Levels for Median Income
HIVE$MedInc_Qrt <- factor(HIVE$MedInc_Qrt, levels = c("1", "2", "3","4"))

# Levels for Hive count
HIVE$HvCnt_Cat <- factor(HIVE$HvCnt_Jenks, levels = c("<2","<5",'<10', '<50', "50+" ))
HIVE$HvCnt_Cat <- relevel(as.factor(HIVE$HvCnt_Jenks), ref = "<2")

# City Dummy
HIVE$City <- relevel(as.factor(HIVE$City), ref = "MTL")
HIVE$Place <- relevel(as.factor(HIVE$Place), ref = "Ground")

#Year dummy
HIVE$Year_2017 <- ifelse(HIVE$Year == '2017', 1, 0)
HIVE$Year_2018 <- ifelse(HIVE$Year == '2018', 1, 0)
HIVE$Year_2019 <- ifelse(HIVE$Year == '2019', 1, 0)
HIVE$Year_2020 <- ifelse(HIVE$Year == '2020', 1, 0)
HIVE$Year_2021 <- ifelse(HIVE$Year == '2021', 1, 0)
HIVE$Year_2022 <- ifelse(HIVE$Year == '2022', 1, 0)
HIVE$Year_Cat <- relevel(as.factor(HIVE$Year), ref = "2019")



Hive1Km<- HIVE %>%
  rename(Hghwy_Bi = Hghwy_len_Binary,
         Status_Bi = Status_Binary) %>% 
  select(HiveID, City, Status, Status_Bi, Place, NDVI_0_20, NDVI_20_50,
         NDVI_50_100, NDVI_mean, Water_Bi, Water_Pct, Build_Pct, 
         Hghwy_Bi, Hghwy_len,
         NO2_mean, O3, Smoke, PM25_mean, BckYrd_mean, Own_mean,
         MedInc_mean, MedInc_Qrt, HvCnt, HvCnt_Cat,
         Year_Cat)

save(Hive1Km, file="DataTreated/Hive_1km_Buffers.Rda")






