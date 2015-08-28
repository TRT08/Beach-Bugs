#Load the XLS files into lists

require(XLConnect) 
library(Hmisc)
library(plyr)
library(data.table)

list.files("G:/Americorps/Data Collected/")

Fall2014 <- loadWorkbook("G:/Americorps/Data Collected/FALL Americorps Data Entry - more recent.xlsx") 
Fall2014list = readWorksheet(Fall2014, sheet = getSheets(Fall2014))
rm(Fall2014)

Summer2014 <- loadWorkbook("G:/Americorps/Data Collected/SUMMER Americorps Data Entry  - more recent.xlsx") 
Summer2014list = readWorksheet(Summer2014, sheet = getSheets(Summer2014))
rm(Summer2014)

All2013 <- loadWorkbook("G:/Americorps/Data Collected/AMERICORPS FINAL DATA 12 10 2013.xlsx") 
All2013list = readWorksheet(All2013, sheet = getSheets(All2013))
rm(All2013)

#combine the fall and summer 2014 data
#names(Fall2014list)
#[1] "EPA"                 "Clad water Readings" "Cladophora"          "Birds"               "Fish"               
#[6] "Wildlife"            "Samples"             "Keys" 

#names(Summer2014list)
# [1] "EPA"                   "Clad water Readings"   "Cladophora"            "Birds"                 "Fish"                 
# [6] "Wildlife"              "Bird Activities"       "Samples"               "10m Bug Survey"        "10m Bug Weather"      
# [11] "10m Bug MatWater Data" "Keys"  

#names(Summer2014list)[(which(names(Summer2014list)%nin%names(Fall2014list)))]
#"Bird Activities"       "10m Bug Survey"        "10m Bug Weather"       "10m Bug MatWater Data"

EPA2014 <- rbind.fill(Fall2014list$EPA, Summer2014list$EPA)
EPA2014$index <- 1:nrow(EPA2014)
names(EPA2014)[!names(EPA2014) %in% c("Date", "Beach.Name")]  <- paste("EPA", names(EPA2014)[!names(EPA2014) %in% c("Date", "Beach.Name")] , sep = "_")
EPA2014$Beach.Name <- ifelse(EPA2014$Beach.Name %in% c("Esch Beach (SLBEB)","SLBEB"), "Esch", 
                             ifelse(EPA2014$Beach.Name %in% c("Glen Haven (SLBEC)","SLBEC"), "Glen Haven",
                                    ifelse(EPA2014$Beach.Name %in% c("Good Harbor (SLBED)","SLBED"), "Good Harbor",
                                           ifelse(EPA2014$Beach.Name %in% c("PETERSON","Peterson Beach","Peterson Beach "), "Peterson",
                                                  ifelse(EPA2014$Beach.Name %in% c("Platte River","SLBEA","Platt Beach (SLBEA)"), "Platte",
                                                         ifelse(EPA2014$Beach.Name == "Sunset Beach", "Sunset",EPA2014$Beach.Name))))))
EPA2014$Beach.Section <- NA

###RECOMPILE EPA ACTIVITIES###

act1 <- EPA2014[,c("Date","Beach.Name","Beach.Section","EPA_Monitor.Name","EPA_Time","EPA_Beach.Activities.Memo", "EPA_Activity.1","EPA_Activity.1.Count")]
act2 <- EPA2014[,c("Date","Beach.Name","Beach.Section","EPA_Monitor.Name","EPA_Time","EPA_Beach.Activities.Memo", "EPA_Activity.2","EPA_Activity.2.Count")]
act3 <- EPA2014[,c("Date","Beach.Name","Beach.Section","EPA_Monitor.Name","EPA_Time","EPA_Beach.Activities.Memo", "EPA_Activity.3","EPA_Activity.3.Count")]
act4 <- EPA2014[,c("Date","Beach.Name","Beach.Section","EPA_Monitor.Name","EPA_Time","EPA_Beach.Activities.Memo", "EPA_Activity.4","EPA_Activity4Count")]
act5 <- EPA2014[,c("Date","Beach.Name","Beach.Section","EPA_Monitor.Name","EPA_Time","EPA_Beach.Activities.Memo", "EPA_Activity.5","EPA_Activity5Count")]

names(act1)[names(act1) == "EPA_Activity.1"] <- "Beach_Activity.1"
names(act2)[names(act2) == "EPA_Activity.2"] <- "Beach_Activity.1"
names(act3)[names(act3) == "EPA_Activity.3"] <- "Beach_Activity.1"
names(act4)[names(act4) == "EPA_Activity.4"] <- "Beach_Activity.1"
names(act5)[names(act5) == "EPA_Activity.5"] <- "Beach_Activity.1"

names(act1)[names(act1) == "EPA_Activity.1.Count"] <- "Beach_Activity.1.Count"
names(act2)[names(act2) == "EPA_Activity.2.Count"] <- "Beach_Activity.1.Count"
names(act3)[names(act3) == "EPA_Activity.3.Count"] <- "Beach_Activity.1.Count"
names(act4)[names(act4) == "EPA_Activity4Count"] <- "Beach_Activity.1.Count"
names(act5)[names(act5) == "EPA_Activity5Count"] <- "Beach_Activity.1.Count"

beach2014 <- rbind.fill(list(act1, act2, act3, act4, act5))
beach2014[ beach2014 == "--" ] = NA
beach2014[ beach2014 == "---" ] = NA
rm(act1, act2, act3, act4, act5)

beach2014 <- beach2014[!(is.na(beach2014$Beach_Activity.1)), ]

beach2014[,7]<- NULL

#Give them 2013 names
setnames(beach2014, old = c('EPA_Monitor.Name','EPA_Time', "EPA_Beach.Activities.Memo"), new = c('Beach_Monitor','Beach_Time','Beach_Memo'))

#get rid of old columns
names(EPA2014)

EPA2014 <- EPA2014[ , -which(names(EPA2014) %in% c("EPA_Activity.1","EPA_Activity.1.Count", "EPA_Activity.2","EPA_Activity.2.Count","EPA_Activity.3",                     
                                              "EPA_Activity.3.Count","EPA_Activity.4","EPA_Activity4Count",                 
                                               "EPA_Activity.5","EPA_Activity5Count","EPA_Beach.Activities.Memo" ))]

Water2014 <- rbind.fill(Fall2014list[2], Summer2014list[2])
Water2014$index <- 1:nrow(Water2014)
names(Water2014)[!names(Water2014) %in% c("Date", "Beach.Name")]  <- paste("Water", names(Water2014)[!names(Water2014) %in% c("Date", "Beach.Name")] , sep = "_")

Water2014$Beach.Name[Water2014$Beach.Name == "Petrson NE"] <- "Peterson North"
Water2014$Beach.Name[Water2014$Beach.Name == "Petrson Mid"] <- "Peterson Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Peterson Mid"] <- "Peterson Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Petrson SW"] <- "Peterson South"
Water2014$Beach.Name[Water2014$Beach.Name == "Platte Mid"] <- "Platte Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Esch Mid"] <- "Esch Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Glen Haven Mid"] <- "Glen Haven Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Good Harbor Mid"] <- "Good Harbor Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Sunset Mid"] <- "Sunset Middle"
Water2014$Beach.Name[Water2014$Beach.Name == "Glen Haven North"] <- "Glen Haven West"
Water2014$Beach.Name[Water2014$Beach.Name == "Platte North"] <- "Platte West"
Water2014$Beach.Name[Water2014$Beach.Name == "Glen Haven South"] <- "Glen Haven East"
Water2014$Beach.Name[Water2014$Beach.Name == "Platte South"] <- "Platte East"
Water2014$Beach.Name[Water2014$Beach.Name == "Good Harbor North"] <- "Good Harbor West"
Water2014$Beach.Name[Water2014$Beach.Name == "Good Harbor South"] <- "Good Harbor East"

Water2014$Beach.Section <- ifelse(is.na(Water2014$Beach.Name), NA,
                    ifelse(grepl("North-Mid",Water2014$Beach.Name), "North-Mid",
                                ifelse(grepl("North",Water2014$Beach.Name), "North", 
                                  ifelse(grepl("South",Water2014$Beach.Name), "South",
                                         ifelse(grepl("East",Water2014$Beach.Name), "East",
                                                ifelse(grepl("West",Water2014$Beach.Name), "West",
                                                       ifelse(grepl("Middle",Water2014$Beach.Name), "Middle", NA)))))))

Water2014$Beach.Name2 <-  ifelse(grepl("Esch",Water2014$Beach.Name), "Esch",
                                         ifelse(grepl("Glen Haven",Water2014$Beach.Name), "Glen Haven", 
                                                ifelse(grepl("Good Harbor",Water2014$Beach.Name), "Good Harbor",
                                                       ifelse(grepl("Peterson ",Water2014$Beach.Name), "Peterson ",
                                                              ifelse(grepl("Platte",Water2014$Beach.Name), "Platte",
                                                                     ifelse(grepl(" Sunset",Water2014$Beach.Name), " Sunset", NA))))))
Water2014$Beach.Name<- Water2014$Beach.Name2 
Water2014$Beach.Name2 <- NULL

###CLAD DATA IS EMPTY#####

Birds2014 <- rbind.fill(Fall2014list[4], Summer2014list[4])
Birds2014$index <- 1:nrow(Birds2014)
names(Birds2014)[!names(Birds2014) %in% c("Date", "Beach.Location")]  <- paste("Birds", names(Birds2014)[!names(Birds2014) %in% c("Date", "Beach.Location")] , sep = "_")
Birds2014$Beach.Name <- Birds2014$Beach.Location
Birds2014$Beach.Location <- NULL
Birds2014$Beach.Section <- NA
Birds2014$Beach.Section[Birds2014$Beach.Name %in% c("Peterson Mid", "Petrson Mid", "Good Harbor Mid", "Esch Mid")] <- "Middle"
Birds2014$Birds_Incidental <- NA
Birds2014$Birds_Incidental[Birds2014$Beach.Name %in% c("Glen Haven Incidental", "Good Harbor Incidental", "Peterson Inc.", "Platte Incidental","Sunset Incidental")] <- "Yes"
Birds2014$Beach.Name <- ifelse(grepl("Glen Haven",Birds2014$Beach.Name), "Glen Haven", 
                              ifelse(grepl("Esch",Birds2014$Beach.Name), "Esch",
                                     ifelse(grepl("Good Harbor",Birds2014$Beach.Name), "Good Harbor",
                                            ifelse(grepl("Sunset",Birds2014$Beach.Name), "Sunset",
                                                   ifelse(grepl("Platte",Birds2014$Beach.Name), "Platte",
                                                          ifelse(Birds2014$Beach.Name %in% c("Peterson", "Peterson Inc.","Peterson Mid","Petrson Mid"), "Peterson", Birds2014$Beach.Name))))))


Fish2014 <- rbind.fill(Fall2014list[5], Summer2014list[5])
Fish2014$index <- 1:nrow(Fish2014)
names(Fish2014)[!names(Fish2014) %in% c("Date", "Beach.Name")]  <- paste("Fish", names(Fish2014)[!names(Fish2014) %in% c("Date", "Beach.Name")] , sep = "_")
Fish2014$Beach.Section <- NA

Wildlife2014 <- rbind.fill(Fall2014list[6], Summer2014list[6])
Wildlife2014$index <- 1:nrow(Wildlife2014)
names(Wildlife2014)[!names(Wildlife2014) %in% c("Date", "Beach.Name")]  <- paste("Wild", names(Wildlife2014)[!names(Wildlife2014) %in% c("Date", "Beach.Name")] , sep = "_")
Wildlife2014$Beach.Section <- NA

Samples2014 <- rbind.fill(Fall2014list["Samples"], Summer2014list["Samples"])
Samples2014$Sample.index <- 1:nrow(Samples2014)

Data2014 <- join_all(list(EPA2014, Water2014, beach2014, Birds2014, Fish2014, Wildlife2014), by=c("Beach.Name","Date","Beach.Section"), type = "left", match="all")
rm(EPA2014, Water2014, Birds2014, Fish2014, Wildlife2014, Fall2014list, beach2014)

Data2014$Year <- 2014

#########
#Compile Kayla's bug transect data
bug1 <- Summer2014list[["10m Bug Survey"]]
bug1$index <- 1:nrow(bug1)
names(bug1)[!names(bug1) %in% c("Date", "Beach.Name")]  <- paste("Surv", names(bug1)[!names(bug1) %in% c("Date", "Beach.Name")] , sep = "_")

bug2 <- Summer2014list[["10m Bug Weather"]]
bug2$index <- 1:nrow(bug2)
names(bug2)[!names(bug2) %in% c("Date", "Beach.Name")]  <- paste("Weath", names(bug2)[!names(bug2) %in% c("Date", "Beach.Name")] , sep = "_")

bug3 <- Summer2014list[["10m Bug MatWater Data"]]
bug3$index <- 1:nrow(bug3)
names(bug3)[!names(bug3) %in% c("Date", "Beach.Name")]  <- paste("Wat", names(bug3)[!names(bug3) %in% c("Date", "Beach.Name")] , sep = "_")

BugDataSummer2014 <- join_all(list(bug1, bug2, bug3), by=c("Beach.Name","Date"), type = "left", match="all")
write.csv(BugDataSummer2014, "G:/Americorps/Final Compiled Data/BugDataSummer2014.csv")
rm(bug1, bug2, bug3, Summer2014list)

###########################################
#Combine the 2013 and 2014 data
names(All2013list)
#[1] "EPA"                 "beach activities"    "Clad water Readings" "Cladophora"          "Birds"              
#[6] "Fish"                "Wildlife"            "Bird Activities"     "Samples"             "Keys"               
#[11] "Photos"              "Sites & Notes"       "HOBO data subsetted" "RAW HOBO recorded"  

EPA <- All2013list[["EPA"]]
EPA$index <- 1:nrow(EPA)
names(EPA)[!names(EPA) %in% c("Date", "Beach.Name")]  <- paste("EPA", names(EPA)[!names(EPA) %in% c("Date", "Beach.Name")] , sep = "_")
EPA$Beach.Section <- NA

Beach <- All2013list[["beach activities"]]
Beach$index <- 1:nrow(Beach)
names(Beach)[!names(Beach) %in% c("Date", "Beach.Name")]  <- paste("Beach", names(Beach)[!names(Beach) %in% c("Date", "Beach.Name")] , sep = "_")
Beach$Beach.Section <- NA

Water <- All2013list[["Clad water Readings"]]
Water$index <- 1:nrow(Water)
names(Water)[!names(Water) %in% c("Date", "Beach.Name")]  <- paste("Water", names(Water)[!names(Water) %in% c("Date", "Beach.Name")] , sep = "_")
Water$Beach.Section <- Water$Water_Name
Water$Beach.Section[Water$Beach.Section %in% c("Mid", "M")] <- "Middle"
Water$Beach.Section[Water$Beach.Section == "S"] <- "South"
Water$Beach.Section[Water$Beach.Section == "N"] <- "North"
Water$Beach.Section <- ifelse(Water$Beach.Section == "Start" & Water$Beach.Name == "Sunset", "North", 
                              ifelse(Water$Beach.Section == "Start" & Water$Beach.Name == "Good Harbor", "West", 
                                     ifelse(Water$Beach.Section == "End" & Water$Beach.Name == "Good Harbor", "East",
                                            ifelse(Water$Beach.Section == "End" & Water$Beach.Name == "Sunset", "South", 
                                                   ifelse(Water$Beach.Section == "West" & Water$Beach.Name == "Peterson", "South",
                                                          ifelse(Water$Beach.Section == "East" & Water$Beach.Name == "Peterson", "North", Water$Beach.Section))))))

Clad <- All2013list[["Cladophora"]]
Clad$index <- 1:nrow(Clad)
names(Clad)[!names(Clad) %in% c("Date", "Beach.Name")]  <- paste("Clad", names(Clad)[!names(Clad) %in% c("Date", "Beach.Name")] , sep = "_")
Clad$Beach.Section <- NA

Birds <- All2013list[["Birds"]]
Birds$index <- 1:nrow(Birds)
names(Birds)[!names(Birds) %in% c("Date", "Beach.Name")]  <- paste("Birds", names(Birds)[!names(Birds) %in% c("Date", "Beach.Name")] , sep = "_")
names(Birds)[names(Birds) == 'Birds_Point'] <- 'Beach.Section'
Birds$Beach.Section <- ifelse(Birds$Beach.Section == "Start" & Birds$Beach.Name == "Sunset", "North", 
                              ifelse(Birds$Beach.Section == "Start" & Birds$Beach.Name == "Good Harbor", "West", 
                                     ifelse(Birds$Beach.Section == "End" & Birds$Beach.Name == "Good Harbor", "East",
                                            ifelse(Birds$Beach.Section == "End" & Birds$Beach.Name == "Sunset", "South", 
                                                   ifelse(Birds$Beach.Section == "West" & Birds$Beach.Name == "Peterson", "South",
                                                          ifelse(Birds$Beach.Section == "East" & Birds$Beach.Name == "Peterson", "North", Birds$Beach.Section))))))

Fish <- All2013list[["Fish"]]
Fish$index <- 1:nrow(Fish)
names(Fish)[!names(Fish) %in% c("Date", "Beach.Name")]  <- paste("Fish", names(Fish)[!names(Fish) %in% c("Date", "Beach.Name")] , sep = "_")
Fish$Beach.Section <- Fish$Beach.Name
Fish$Beach.Name[Fish$Beach.Name == "Esch All"] <- "Esch"
Fish$Beach.Name[Fish$Beach.Section %in% c("Peterson", "Peterson M", "Peterson N", "Peterson S")] <- "Peterson"
Fish$Beach.Name[Fish$Beach.Name == "peterson N"] <- "Peterson"
Fish$Beach.Section[Fish$Beach.Section %in% c("peterson N", "Peterson N")] <- "End"
Fish$Beach.Section[Fish$Beach.Section == "Peterson S"] <- "Start"
Fish$Beach.Section[Fish$Beach.Section == "Peterson M"] <- "Middle"
Fish$Beach.Section[Fish$Beach.Section %in% c("Esch","Esch All", "Good Harbor", "Peterson", "Sunset" )] <- NA
Fish$Beach.Section <- ifelse(Fish$Beach.Section == "Start" & Fish$Beach.Name == "Peterson", "South", 
                              ifelse(Fish$Beach.Section == "End" & Fish$Beach.Name == "Peterson", "North", Fish$Beach.Section))

Wildlife <- All2013list[["Wildlife"]]
Wildlife$index <- 1:nrow(Wildlife)
names(Wildlife)[!names(Wildlife) %in% c("Date", "Beach.Name")]  <- paste("Wild", names(Wildlife)[!names(Wildlife) %in% c("Date", "Beach.Name")] , sep = "_")
Wildlife$Beach.Section <- Wildlife$Beach.Name
Wildlife$Beach.Section[Wildlife$Beach.Section %in% c("Esch", "Good Harbor", "peterson", "Sunset" )] <- NA
Wildlife$Beach.Section[Wildlife$Beach.Section == "Peterson M"] <- "Middle"
Wildlife$Beach.Name[Wildlife$Beach.Name %in% c("peterson", "Peterson M")] <- "Peterson"

#Do separately
birdact <- All2013list[["Bird Activities"]]

Samples2013 <- All2013list[["Samples"]]
Samples2013$Sample.index <- 1:nrow(Samples2013)

Photos <- All2013list[["Photos"]]
sites <- All2013list[["Sites & Notes"]]
HOBOdatasub <- All2013list[["HOBO data subsetted"]]
HOBOdataRAW <- All2013list[["RAW HOBO recorded"]]

###Combine 2013 data#####
Data2013 <- join_all(list(EPA, Beach, Water, Clad, Birds, Fish, Wildlife), by=c("Beach.Name","Date", "Beach.Section"), type = "left", match="all")
rm(EPA, Beach, Water, Clad, Birds, Fish, Wildlife, All2013list)

Data2013$Year <- 2013

##FIX THE DATA

Data2014[ Data2014 == "--" ] = NA
Data2014[ Data2014 == "na" ] = NA
Data2014[ Data2014 == "Na" ] = NA
Data2013[ Data2013 == "na" ] = NA
Data2013[ Data2013 == "Na" ] = NA

#columns not matching between years
bad2013 <- names(Data2013)[(which(names(Data2013)%nin%names(Data2014)))]
bad2013 <- bad2013[!grepl("Clad_", bad2013)]
bad2013
names(Data2014)[(which(names(Data2014)%nin%names(Data2013)))]

#change all the bad column names

setnames(Data2014, old = c('EPA_Checked.','EPA_Monitor.Name',"Water_Checked.","Birds_Checked...Y.","Fish_Checked.","Fish_Monitor.Name","Wild_Checked.","Wild_Monitor.Name","Wild_Activity...Notes","Water_Monitor.Name","Birds_Monitor.Name","Water_NTU..Turbidity.","Water_Salinity"), 
         new = c('EPA_Checked','EPA_Monitor',"Water_checked","Birds_checked","Fish_Checked","Fish_Monitor","Wild_checked", "Wild_Monitor","Wild_Activity","Water_Monitor","Birds_Monitor","Water_NTU","Water_Salinity..ppm."))

###COMBINE 2013 and 2014 data, FINALLY!

BothYears <- rbind.fill(Data2013, Data2014)










