
# This is a scrit to subset the raw dataset used in the package 

library("rgdal")
CS <- readOGR("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/Boundaries/shapes","CS_OUTLINE") # Celtic Sea Outline


tmp <- read.csv("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/DATA-V3/SurveyEffort.GOV_2003-2016_ALL_SPECIES_&_LENGTHS_clipped_to_CELTIC_SEA.csv")


head(tmp)

dim(tmp)
# Month
unique(tmp$MonthShot)
tmp1 <- tmp[tmp$MonthShot == c(10,11,12),]
unique(tmp1$MonthShot)

dim(tmp1)

# Ship
unique(tmp1$Ship)
tmp2 <-  tmp1[tmp1$Ship == c("CEXP", "THA2", "SCO3"),]
unique(tmp2$Ship)
dim(tmp2)


# Walker list
Spec <- read.csv("C:/Users/Hello.Mckenna/OneDrive - GMIT/__MRes Thesis/PACKAGE/Walker_species_list.csv", sep = "/t")


tmp3 <- tmp2[tmp2$SpeciesSciName == as.character(tmp2$SpeciesSciName),]


length(unique(tmp3$SpeciesSciName))

nrow(Spec)


setwd("C:/Users/Hello.Mckenna/OneDrive - GMIT/__MRes Thesis/PACKAGE")

x11()


plot(tmp3$ShootLong_degdec, tmp3$ShootLat_degdec)
lines(CS)


getwd()


write.csv(tmp3, "CS_data_Filtered.csv")
