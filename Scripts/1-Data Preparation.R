library(dplyr)
library(lubridate)
library(xlsx)

rm(list=ls())
setwd("C:/_MY FOLDERS/RECHERCHE/HIVE/SOURCE")

## LOAD ########################################################################
HIVE <- read.csv("Hive_Health_220113.csv",header = TRUE, sep = ";", dec = ".") %>% 
  mutate(Month = month(mdy(Sample.date)),
         Year = year(mdy(Sample.date)),
         Number = Hive.number,
         CliID = match(Client.name, unique(Client.name)),
         AdrID = match(Address, unique(Address)),
         HiveID = paste0(CliID, ".", Number),
         Date = mdy(Sample.date),
         Status = Hive.status, 
         Place = Hive.placement,
         Season = case_when(
           Month %in% c(3, 4, 5) ~ "Spg",
           Month %in% c(6, 7, 8) ~ "Sum",
           Month %in% c(9, 10, 11) ~ "Fall",
           Month %in% c(12, 1, 2) ~ "Win"))%>% 
   select(HiveID, CliID, Number, Year, Season, Status, Place, Date, Lat, Lng, AdrID, City, Address, Client.name, Cause.of.death) %>%
   # select(HiveID, Year, Season, Status, Place, Date, City, Address, Client.name, AdrID, Cause.of.death, Lat, Lng)%>% 
   # select(HiveID, Year, Status, Place, Date, Number, Lat, Lng)%>% 
   arrange(HiveID,Date)


#################################################################################
#                 Check if Client ID or Address ID is better         
################################################################################

# HIVE %>% select(CliID) %>% n_distinct()  #1901 by client ID
# HIVE %>% select(AdressID) %>% n_distinct()  #1871 by address ID

# More clients than addresses,some addresses must have multiple clients

################################################################################
#                                  CORRECT NULL GEOGRAPHY
################################################################################

# extract all data with null geography coordinates
# FindGeo <- subset(HIVE, grepl("null", HIVE$Lat))
# FindGeo_subset <- FindGeo[,c("Address","City")]
# FindGeo_subset %>% select(Address) %>% n_distinct()  # 200 null geocoordinate locations
# write.xlsx(FindGeo_subset, "FindGeo_subset.xlsx", row.names = FALSE)

# prepare the coordinate vectors
HIVE$Lat <- ifelse(HIVE$Lat == "null", NA, HIVE$Lat)
HIVE$Lng <- ifelse(HIVE$Lng == "null", NA, HIVE$Lng)
HIVE$Lat <- as.numeric(as.character(HIVE$Lat))
HIVE$Lng <- as.numeric(as.character(HIVE$Lng))

# import new geography coordinates for missing hives (Found using Awesome Table for Gsheets)
MISSING <- read.csv("Missing Hives_GeoCoded.csv",header = TRUE, sep = ",", dec = ".")
MISSING <- MISSING[!duplicated(MISSING), ]

# Insert new latitude and longitude coordinates
HIVE$Lat <- ifelse(is.na(HIVE$Lat), MISSING$Lat[match(HIVE$Address,MISSING$Address)], HIVE$Lat)
HIVE$Lng <- ifelse(is.na(HIVE$Lng), MISSING$Lng[match(HIVE$Address,MISSING$Address)], HIVE$Lng)

# # check to see if match worked (one case with null address)
# missing_lat <- filter(HIVE, is.na(Lat))


###############################################################################
#                             REMOVE VARIABLES
################################################################################

to_remove <- c("Beekeeping Practices (Starvation, Poor Wrapping, Moisture Control, Other)",
               "Animal/Human Interference (Rodents, Vandalism, Robbing, Other)",
               "Swarmed hive")
HIVE <- HIVE[!(HIVE$Cause.of.death %in% to_remove), ]

# Remove observations with hive number 0 and 71 and address is null    (6 removed)
HIVE <- subset(HIVE, HIVE$Number != 0 & HIVE$Number != 71 & HIVE$Address != "null")

# remove pure duplicates - daily sample 
HIVE <- HIVE[!duplicated(HIVE), ]

###############################################################################
#                  SUPERHIVES
################################################################################
# Dead count per HiveID variable creation
DeadCountID <- HIVE %>% 
  group_by(HiveID) %>% 
  summarize(DeadCnt = sum(Status == "Dead"))
HIVE <- left_join(HIVE, DeadCountID, by="HiveID")

# Super Locations with hives that never die (n = 863)
SuperLoc <- HIVE %>% 
  group_by(HiveID) %>% 
  summarize(SuperLoc = any(DeadCnt == 0))
HIVE <- left_join(HIVE, SuperLoc, by="HiveID")

#super clients with all hives that never die (n = 671)
SuperCli <- HIVE %>% 
  group_by(CliID) %>% 
  summarize(SuperCli = any(DeadCnt == 0))
HIVE <- left_join(HIVE, SuperCli, by="CliID")

# # distinct hive locations (n = 1898)
# LOC_distinct <- HIVE %>% distinct(HiveID, .keep_all = TRUE)

# distinct hive clients (n = 1168)
# CLI_distinct <- HIVE %>% distinct(CliID, .keep_all = TRUE)

# Find the maximum amount of hives for each client
MaxHive <- HIVE %>%
  group_by(CliID) %>%
  summarize(MaxHive = max(Number))
HIVE <- left_join(HIVE, MaxHive, by="CliID")

# HIVEdescstat <- summary(HIVE$MaxHive)
# SUPERCLIdescstat <- summary(SUPERCLI$MaxHive)
# print(HIVEdescstat)
# print(SUPERCLIdescstat)


# transform Place variable
HIVE$Place <- ifelse(HIVE$Place == "White Roof", "Roof", HIVE$Place)
HIVE$Place <- ifelse(HIVE$Place %in% c("Balcony", "Backyard", "Field"), "Ground", HIVE$Place)



################################################################################
#                               HIVE ID - replacement suffix
################################################################################

# Create the "RepCnt" column in the dataframe and set all values to 0
HIVE$RepCnt <- 0
# Create counts for replacement hives when earlier hive status shifts from "Dead" to "Healthy"
for (i in unique(HIVE$HiveID)) {
  sub_HIVE <- subset(HIVE,HiveID == i)
  for (j in 2:nrow(sub_HIVE)) {
    earlier_row <- subset(sub_HIVE, Date < sub_HIVE$Date[j])
    if(nrow(earlier_row)>0){
      earlier_row <- earlier_row[which.max(earlier_row$Date),]
      if (sub_HIVE$Status[j] == "Healthy" & earlier_row$Status == "Dead") {
        sub_HIVE$RepCnt[j] <- earlier_row$RepCnt + 1
      } else {
        sub_HIVE$RepCnt[j] <- earlier_row$RepCnt
      }
    }
    HIVE[HIVE$HiveID == i,] <- sub_HIVE
  }
}  
# Create colony ID
HIVE$ClnyID <- paste(HIVE$HiveID, "_", HIVE$RepCnt, sep="") # paste replacement count as a suffix to HiveID
HIVE <- HIVE %>% select(ClnyID, everything()) 

HIVE$HiveID <- as.character(HIVE$HiveID)
HIVE$CliID <- as.character(HIVE$CliID )
HIVE$ClnyID <- as.character(HIVE$ClnyID)

# Keep only one HiveID of each status per Year
HIVE_cut <- HIVE %>%
  group_by(Year,HiveID) %>%
  slice(if("Dead" %in% Status) match("Dead", Status) else 1) %>%
  ungroup()

# Location Weight by year
Y_LOC <- HIVE_cut %>%
  group_by(Year) %>% 
  summarize(Y_LOC = sum(MaxHive))
HIVE_cut<- left_join(HIVE_cut, Y_LOC, by="Year")

HIVE_cut$Wgt_Y <-HIVE_cut$MaxHive / HIVE_cut$Y_LOC


# save unique clients
CLIENTS <- distinct(HIVE_cut, CliID, .keep_all = TRUE)
write.csv(CLIENTS ,"CLIENTS.csv",row.names = FALSE)

# Save
write.csv(HIVE_cut,"HIVE_Cases.csv",row.names = FALSE)








