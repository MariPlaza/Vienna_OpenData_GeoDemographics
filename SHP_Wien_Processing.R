'Set-Up environment and charge usefull libraries'
#LIBRARIES
list.of.packages <- c("rgdal","sp", "rgeos", "RMySQL")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)

#DIRECTORY
directory <- ''
setwd(directory)

mydb <- dbConnect(MySQL(), user='', password='', dbname='', host='localhost')


#CODE
Buildings <- readOGR("GENFLWIDMUNGOGD","GENFLWIDMUNGOGDPolygon")
Limits <- readOGR("ZAEHLBEZIRKOGD","ZAEHLBEZIRKOGDPolygon")
Limits@data$ID <- paste0('9',ifelse(Limits@data$BEZNR<10,paste0('0',Limits@data$BEZNR),Limits@data$BEZNR),
                         ifelse(Limits@data$ZBEZNR<10,paste0('0',Limits@data$ZBEZNR),Limits@data$ZBEZNR))

centers <- cbind(Buildings@data$OBJECTID,data.frame(gCentroid(Buildings, byid=TRUE)))
names(centers) <- c('ObjectID','Latitud','Longitud') 

coordinates(centers) <- c('Latitud', 'Longitud')
proj4string(centers) <- proj4string(Limits)

centers$Sub_District <- over(centers, Limits)$ID
centers$SD_Area <- over(centers, Limits)$FLAECHE

BuildingDF <- Buildings@data
CenterDF <- cbind(centers@data,centers@coords)

Buildings_Complete <- merge(BuildingDF, CenterDF, by.x = "OBJECTID", by.y = "ObjectID")
Category_Appartment <- c("WO","WOFB","WOGV","GB","GBGV","GBBG")
Logical_Vector <- Buildings_Complete$WIDMUNGSKL %in% Category_Appartment

Final_Buildings <- rbind(Buildings_Complete[Logical_Vector,], 
                         Buildings_Complete[Buildings_Complete$OBJECTID=='1569254',],
                         Buildings_Complete[Buildings_Complete$OBJECTID=='1561602',],
                         Buildings_Complete[Buildings_Complete$OBJECTID=='1559078',]
                          )
Final_Buildings$Category <- 2
Final_Buildings$Type <- 'Appartments'
Final_Buildings <- Final_Buildings[,c(11:12,2,10,9,1,6,7,4)]
names(Final_Buildings) <- c("Category", "Type", "SubType", "Latitud", "Longitud", "ObjectID", "Area", "Sub_District", "District")

dbWriteTable(mydb, name='locations', value=Final_Buildings, append = TRUE,row.names=FALSE)  


'Read files and build dataframes'
Population_Gender <- read.csv(file="vie_304.csv", header=TRUE, skip = 1, sep=";")
Population_Gender <-  Population_Gender[,c("REF_Date","SUB_DISTRICT_CODE", "POP_MEN","POP_WOMEN")]
Population_Gender <- melt(Population_Gender, id.vars=c("REF_Date","SUB_DISTRICT_CODE"))
names(Population_Gender) <- c("REF_DATE","SUB_DISTRICT_CODE","SEX","N")
Population_Gender$SEX <- as.numeric(Population_Gender$SEX)

Population_Age_Group <- read.csv(file="vie_303.csv", header=TRUE, skip = 1, sep=";")
Population_Age_Group <- Population_Age_Group[,c("REF_DATE","SUB_DISTRICT_CODE","SEX","AGE_00_02","AGE_03_05","AGE_06_09","AGE_10_14","AGE_15_19","AGE_20_24","AGE_25_29","AGE_30_44","AGE_45_59","AGE_60_74","AGE_75.")]
Population_Age_Group[,c(4:14)] <-  round(Population_Age_Group[,c(4:14)]/rowSums(Population_Age_Group[,c(4:14)]),8)

Population_Nationality <- read.csv(file="vie_113.csv", header=TRUE, skip = 1, sep=";")
Population_Nationality <- Population_Nationality[,c("REF_DATE","SUB_DISTRICT_CODE","SEX","POP_AUT","POP_SCG","POP_TUR","POP_DEU","POP_POL","POP_BIH","POP_HRV","POP_ROU","POP_CZE","POP_HUN","POP_OTHER")]
Population_Nationality <- Population_Nationality[Population_Nationality$REF_DATE>=20140101,]
Population_Nationality[,c(4:14)] <-  round(Population_Nationality[,c(4:14)]/rowSums(Population_Nationality[,c(4:14)]),8)

Population_Density <- merge(Population_Gender, Population_Age_Group, by= c("REF_DATE", "SUB_DISTRICT_CODE","SEX"), all.x = TRUE)
Population_Density <- merge(Population_Density, Population_Nationality, by= c("REF_DATE", "SUB_DISTRICT_CODE","SEX"), all.x = TRUE)
