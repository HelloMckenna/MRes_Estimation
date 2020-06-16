##
# THis script is to identify and apply the gear efficency coefficents by Walker et al 2017 to Species data from the IBTS
#
#
#
#### DEFAULTS
rm(list=ls()) 
if (!require('rgdal')) install.packages('rgdal'); library('rgdal')
if (!require('raster')) install.packages('raster'); library('raster')
if (!require('rgeos')) install.packages('rgeos'); library('rgeos')
if (!require('reshape2')) install.packages('reshape2'); library('reshape2')


#setwd("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/__PAPER 1/FINAL RESULTS/DATA")
Home <- getwd()
setwd(Home)

# Load maps
CS <- readOGR("./Data",layer = "CS_OUTLINE") # Celtic Sea Outline
Eur <- readOGR("./Data",layer = "europe") # Europe Outline


# Load Walker Species and catchability dataframes
SpeciesList <- read.csv("./Data/Walker_species_list.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)
GOV <-  read.csv("./Data/EfficiencyTab_GOV_subset.csv")


# list of dataframes for each species of interest
LaM.raw <- read.csv("./Data/LaM_Estimates.csv")



# IMPORT DATA FOR ALL SPECIES 
data.raw <- read.csv("./Data/raw.data_CS.csv", stringsAsFactors = FALSE)
data3 <- data.raw[data.raw$MonthShot %in% 10:12,] # Subset for quarter 4
data2 <- data3[data3$Gear == "GOV",] # SUBSET TO GOV ONLY
data <- data2[data2$SpeciesSciName %in% SpeciesList$Species,] # subset for Walker species

unique(data$MonthShot)
unique(data$Ship)
length(unique(data$HaulID))
length(unique(data$SpeciesSciName))



# Species length at maturity in common between data and Walker species list
LaM <- LaM.raw[LaM.raw$Species %in% data$SpeciesSciName,] 


#for (b in 1:nrow(LaM)){ # for 
  
for (b in c(22,28)){
gc()

# Identify species names for loop    
SoI <- as.character(LaM$Species[b])
CommonName <- as.character(as.character(LaM$Common[b]))
Code <- as.character(as.character(LaM$Code[b]))
print(paste(b ,Code, CommonName, SoI))

# Create Directory
setwd(Home)
mainDir <- "./Data"
subDir <- paste(CommonName)
dir.create(file.path(mainDir, subDir), showWarnings = FALSE)
setwd(file.path(mainDir, subDir))
getwd()




# Subset for Species

Gear_efficency <- GOV[GOV$Species == CommonName,]

# Plot Walker Catchability cooefs for this species
png(paste0("GOV gear efficency - ", CommonName,".png"))
ifelse( is.na(Gear_efficency$Efficiency[1] == TRUE),
        
c( plot(0,0, main = paste("GOV gear efficiency for",CommonName ,"by length\n with 95% confidence intervals "), col = "white"),text(0,0,"NO GEAR \nEFFICENCY \nCOEFFICENT \nFOUND", cex = 3),  try(dev.off()))
,
# Show the efficency by length model 
 c(       plot(Gear_efficency$Length, Gear_efficency$Efficiency, type = "l", main = paste("GOV gear efficiency for",CommonName ,"by length\n with 95% confidence intervals "),xlab = "Length", ylab = "Efficiency"),
        lines(Gear_efficency$Length, Gear_efficency$Eff.l.95, lty = 2),
        lines(Gear_efficency$Length, Gear_efficency$Eff.u.95, lty = 2),
                range(Gear_efficency$Length),
        try(dev.off())))




# Subset for Species selected by this loop
Species.RAW <- Species <- data[data$SpeciesSciName == SoI,]
length(unique(data$HaulID))
length(unique(Species$HaulID))

head(Species)


# Some information about the data
length(Species.RAW$DensAbund_N_Sqkm)
(Range <- range(sort(unique(Species.RAW$YearShot))))
(Summary <- summary(Species.RAW$DensAbund_N_Sqkm))



# Identify if there are less than 2 data points so that the histogram does not crash
ifelse(nrow(Species) <= 2,
      c(
        png(paste0("Histogram of length data ",CommonName," (",SoI,").png")),
        plot(0,0, main = paste(CommonName ,"Histogram"), col = "white"),
        text(0,0,"NOT ENOUGH \nDATA POINTS \nFOR \nHISTOGRAM", cex = 3),
        try(dev.off()),
        
        
       next),
       paste("More than 2 data points detected")
)



# Plot Histogram of lengths
png(paste0("Histogram of length data ",CommonName," (",SoI,").png"))
MASS::truehist(Species$FishLength_cm, main = paste0(CommonName," (",SoI,") ", "\nLength histogram"), xlab = "Fish Length cm")
try(dev.off())


# Next if there are less than 5 data points - loop will move on to next loop
ifelse(nrow(Species) <= 5,
       next,
       paste("More than 5 data points detected")
       )



# Length contained by the raw data
(Lengths <- sort(unique(Species$FishLength_cm)))

# apply coeffs to Density abundance per km^2 using number of individuals in each length class for loop selected species-  
for (i in 1:nrow(Species)){
  gc()
  ifelse( is.na(Gear_efficency$Efficiency[1] == TRUE),
          c(print("NO CATCHABILITY COEFF DETECTED"),{next}), print("Catchability Coeff Detected") )

try(Species$DensAbund_N_Sqkm[i] <- Species$DensAbund_N_Sqkm[i] * abs(Gear_efficency[Gear_efficency$Length == Species$FishLength_cm[i],]$Efficiency -1 * 2)
)
print(paste("Row",i, "of", nrow(Species), "Length class=" , Gear_efficency[Gear_efficency$Length == Species$Number[i],]$Length,"cm", "   multiplier=", abs(Gear_efficency[Gear_efficency$Length == Species$Number[i],]$Efficiency -1 * 2) ))
       
}


# Check a change has been applied
head(Species)
head(Species.RAW)


Species.RAW1 <- Species.RAW
Species1 <- Species
Summary1 <- Summary

# Plot of before and after catch coeffs
{png(paste("Before and after application of Walker Catchability coeffs", CommonName,".png"))
par(mfrow = c(1,2))
plot(Species.RAW1$ShootLong_degdec, Species.RAW1$ShootLat_degdec, cex = Species.RAW1$DensAbund_N_Sqkm / (Summary1[6]/3), bty = "l", pch = 19, col = "#CC000070", main = paste0("Before Gear efficiencies\n", Range[1], "-",Range[2]), asp = 1)
#lines(Eur)
plot(Species1$ShootLong_degdec, Species1$ShootLat_degdec, cex = Species1$DensAbund_N_Sqkm / (Summary1[6]/3), bty = "l", pch = 19, col = "#DD077777", main = paste0("After Gear efficiencies\n", Range[1], "-",Range[2]), asp = 1)
#lines(Eur)
par(mfrow = c(1,1))
dev.off()}

Species$DensAbund_N_Sqkm_RAW <- Species.RAW$DensAbund_N_Sqkm



#########_______________________________ AGGREGATE TO cm LENGTH CLASS __________________________________#######



# Define length at maturity for this loop
LaM.t <- data.frame(
  Juvenile =  seq(1, round(LaM$mu[b], 0), l = 2),
  Adult = seq(round(LaM$mu[b],0), max(Species$FishLength_cm), l = 2) )
  


LaM.Label <- data.frame(Juvenile = paste0(min(LaM.t[1]),"-",max(LaM.t[1])), Adult = paste0(min(LaM.t[2]),"-",max(LaM.t[2])))

for (j in 1:length(LaM.t)){
  gc()
  LaM.Label[j]

LaM_selected <- LaM.t[,j]  # Length class for processing

# loop 




length(unique(Species$HaulID))
#Maturity <- Species[Species$FishLength_cm > as.integer(LaM_selected[1]) &  as.integer(LaM_selected[2]),]
 
Maturity <-  subset(Species, FishLength_cm > LaM_selected[1] & FishLength_cm < LaM_selected[2])
# Adult <- Species[Species$FishLength_cm >= LaM[,1][2],]

length(unique(Maturity$HaulID))# Number after we have subset for our length class of interest


# Stop to check there are positive catches
ifelse( dim(Maturity)[1] == 0 ,
               c(paste("No positive catches detected for length class ",min(LaM),":",max(LaM)," cm loop stopped"),
        {next}), 
 #       stop("No positive catches detected for length class ",min(LaM),":",max(LaM)," cm loop stopped", call. = FALSE)),
        paste("Positive catches detected for length class")
        )




range(Maturity$FishLength_cm)
dim(Maturity)
(Summary <- summary(Maturity$DensAbund_N_Sqkm))
#hist(Maturity$DensAbund_N_Sqkm)

# Test to check where maturity lies
# max(Maturity$FishLength_cm)
# min(Adult$FishLength_cm)





unique(Maturity$HaulID)




f.data <- Maturity
f.data$X.1 <- NULL #removing unnesessary column added by system
f.data$X <- NULL 


# AGGREGATING DATA

Test_survey <- f.data$HaulID[1]  # this is to test the subsetting is working properly later on
#Age@length <- function(data, LaM)

Order <- colnames(f.data)# For rearranging data frames order later

Class <-LaM.Label[j]
  
# ifelse(max(f.data$FishLength_cm) > LaM,
#        Class <- "ADULT",
#        Class <- "JUVENILE")

dim(f.data)
length(unique(f.data$HaulID))
sort(unique(f.data$YearShot))
#f.data$count=NA

new.df<-stats::aggregate(f.data$Number,
                  by = list(f.data$HaulID,f.data$SpeciesSciName),
                  FUN = sum)

# Catch for when no catches are detected
ifelse(nrow(new.df)==0,
c(paste("No positive catches detected"), {next}),
paste("Positive catches detected")
       )

# Aggregate each column
names(new.df)[1:3]<-c("HaulID","SpeciesSciName", "Number")
new.df$ShootLat_degdec<- f.data$ShootLat_degdec[match(new.df$HaulID,f.data$HaulID)]
new.df$ShootLong_degdec<-f.data$ShootLong_degdec[match(new.df$HaulID,f.data$HaulID)]
new.df$Ship<-f.data$Ship[match(new.df$HaulID,f.data$HaulID)]
new.df$WingSwpArea_sqkm<-f.data$WingSwpArea_sqkm[match(new.df$HaulID,f.data$HaulID)]
new.df$YearShot<-f.data$YearShot[match(new.df$HaulID,f.data$HaulID)]
new.df$MonthShot   <-      f.data$MonthShot[match(new.df$HaulID,f.data$HaulID)]
new.df$DayShot        <-    f.data$DayShot[match(new.df$HaulID,f.data$HaulID)]
new.df$GearType <-  f.data$GearType[match(new.df$HaulID,f.data$HaulID)]
new.df$Gear     <- f.data$Gear[match(new.df$HaulID,f.data$HaulID)]
new.df$Survey_Acronym.x <- f.data$Survey_Acronym.x[match(new.df$HaulID,f.data$HaulID)]
new.df$TimeShot          <- f.data$TimeShot[match(new.df$HaulID,f.data$HaulID)]
new.df$HaulDur_min <- f.data$HaulDur_min[match(new.df$HaulID,f.data$HaulID)]
new.df$ICESStSq       <-    f.data$ICESStSq[match(new.df$HaulID,f.data$HaulID)]
new.df$SurvStratum      <- f.data$SurvStratum[match(new.df$HaulID,f.data$HaulID)]
new.df$Depth_m <- f.data$Depth_m[match(new.df$HaulID,f.data$HaulID)]
new.df$Distance_km <- f.data$Distance_km[match(new.df$HaulID,f.data$HaulID)]
new.df$WingSpread_m <- f.data$WingSpread_m[match(new.df$HaulID,f.data$HaulID)]
new.df$DoorSpread_m <- f.data$DoorSpread_m[match(new.df$HaulID,f.data$HaulID)]
new.df$NetOpen_m <- f.data$NetOpen_m[match(new.df$HaulID,f.data$HaulID)]
new.df$WingSwpVol_CorF <- f.data$WingSwpVol_CorF[match(new.df$HaulID,f.data$HaulID)]
new.df$DoorSwptArea_CorF <- f.data$DoorSwptArea_CorF[match(new.df$HaulID,f.data$HaulID)]
new.df$DoorSwptVol_CorF <- f.data$DoorSwptVol_CorF[match(new.df$HaulID,f.data$HaulID)]
new.df$Survey_Acronym.y <- f.data$Survey_Acronym.y[match(new.df$HaulID,f.data$HaulID)]
new.df$Aphia_Code <- f.data$Aphia_Code[match(new.df$HaulID,f.data$HaulID)]
new.df$FishLength_cm <- as.character(Class[1,1])[1]
new.df$DensAbund_N_Sqkm <- aggregate(f.data$DensAbund_N_Sqkm,
                                     by = list(f.data$HaulID,f.data$SpeciesSciName),
                                     FUN = sum)[,3]

new.df$DensAbund_N_Sqkm_RAW <- aggregate(f.data$DensAbund_N_Sqkm_RAW,
                                     by = list(f.data$HaulID,f.data$SpeciesSciName),
                                     FUN = sum)[,3]


new.df$DensBiom_kg_Sqkm <- aggregate(f.data$DensBiom_kg_Sqkm,
                                     by = list(f.data$HaulID,f.data$SpeciesSciName),
                                     FUN = sum)[,3]

new.df <- new.df[,Order] # sort columns


head(new.df)
colnames(new.df)
#check

subset(new.df,new.df$HaulID == Test_survey & new.df$SpeciesSciName == SoI)
subset(f.data,f.data$HaulID == Test_survey & f.data$SpeciesSciName == SoI)
#

#________________________ WE HAVE THE LENGTH CLASS WE WANT SUBSET 
# NOW WE 
# 1 MOVE ONTO THE LENGTHS OUSIDE OF OUR DEFINED LENGTH CLASS AND REMOVE THE HAUL IDs THAT HAVE ALREADY BEEN AGGREGATED

HaulIDs_Processed <- unique(new.df$HaulID) # haulIDs for lengths we have aggregated 

# Removing Hauls THAT ARE DATA POSITIVE
data.negative <- subset(data.raw, !(HaulID %in% HaulIDs_Processed))



data.negative$X.1 <- NULL
# Unique Hauls THAT ARE DATA NEGATIVE 
#data.negative.unique <- data.negative[data.negative$HaulID == unique(data.negative$HaulID),]

# Have only rows with unique haul IDs  
data.negative <- data.negative[!duplicated(data.negative$HaulID),]


# ifelse(nrow(data.negative)<1,
#        next,
#        print("0 Catch hauls detected"))


# 2 AGGREGATE BY UNIQUE HAUL ID 

# data.negative$expand<-ifelse(data.negative$FishLength_cm == Class[1,1] ,1,0)
# 
# tmp<-tapply(Species$expand, Species$HaulID ,sum)
# length(tmp)
# length(tmp[tmp==0])  # 0 fish with

# 3 APPLY ZEROs to information on abundance 
data.negative$DensBiom_kg_Sqkm <- 0
data.negative$DensAbund_N_Sqkm <- 0
data.negative$DensAbund_N_Sqkm_RAW <- 0
data.negative$Number <- 0
data.negative$FishLength_cm <- paste("!",Class[1,1])[1]

dim(data.negative)

data.negative$X <- NULL



# 4 JOIN THEM TOGETHER 
Out <- rbind(new.df, data.negative)
Out$CommonName <- CommonName
Out$SpeciesSciName <- SoI



ifelse( is.na(Gear_efficency$Efficiency[1] == TRUE),
        Out$WalkerCoeffApplied <- NA,
Out$WalkerCoeffApplied <- TRUE
)


nrow(Out) == length(unique(data.raw$HaulID)) # If true Then we have all of our hauls accounted for


# 5 EXPORT 
library(stringr)
Survey <- paste0(sort(unique(word(Out$HaulID,1, sep = "/"))), collapse = "_&_") # Unique Survey IDs
Vessel <- paste0(sort(unique(Out$Ship)), collapse = "_")                      # Unique Vessle IDs
Years <- paste0(range(unique(Out$YearShot)), collapse = "_-_")                # Year range
Date <- gsub(":",".",date(), fixed = TRUE)





filename <- paste0(min(LaM.t[j]),"-" ,max(LaM.t[j]),"cm ", unique(Out$SpeciesSciName)," ",Years," ",unique(Out$Gear)," with Zeros ",Date )



# Write to file 
write.csv(Out, paste0(filename,".csv"))

try(dev.off())





# Save plot
xlim <- c(extent(CS)[1], extent(CS)[2]) # Define Celtic Sea Limits for plot
ylim <- c(extent(CS)[3], extent(CS)[4])



{
  png(paste0(filename,".png"))
  plot(CS, border = "lightgrey", main = paste0(min(LaM.t[,j])," to ",max(LaM.t[,j])," cm ",CommonName,"\n", Range[1], "-",Range[2]), asp = 1, ylim = ylim, xlim = xlim, xlab = "long", ylab = "lat"); lines(Eur, col = "lightgrey")
  
  color <- c("#399d20", "#c43b3b", "#3b58c4")  
  col <- adjustcolor(color, alpha.f = 0.5) 
  
  # CEXP POINTS
  points( Maturity[Maturity$Ship == "CEXP", ]$ShootLong_degdec, Maturity[Maturity$Ship == "CEXP", ]$ShootLat_degdec, cex = Maturity[Maturity$Ship == "CEXP", ]$DensAbund_N_Sqkm / (Summary[6]/3), bty = "l", pch = 19, col = col[1])
  
  # THA2 POINTS
  points(Maturity[Maturity$Ship == "THA2", ]$ShootLong_degdec, Maturity[Maturity$Ship == "THA2", ]$ShootLat_degdec, cex = Maturity[Maturity$Ship == "THA2", ]$DensAbund_N_Sqkm / (Summary[6]/3), bty = "l", pch = 19, col = col[2])
  
  # SCO3 POINTS
  points(Maturity[Maturity$Ship == "SCO3", ]$ShootLong_degdec, Maturity[Maturity$Ship == "SCO3", ]$ShootLat_degdec, cex = Maturity[Maturity$Ship == "SCO3", ]$DensAbund_N_Sqkm / (Summary[6]/3), bty = "l", pch = 19, col = col[3])
  
  legend("topleft", legend=c("CEXP", "THA2", "SCO3"),
         col= col, pch = 16)
  dev.off()
}


try(dev.off())

}


try(dev.off())
}



# END OF SCRIPT #