
#######################################################################################################################################
#
# hello.mckenna@gmail.com
#
# This script is used to estimate the geospatial distribution strucutre of selected species within the Celtic Sea Eco-region.
# Data is included in this analysis, Please input species of interest code from the Masterlist below into Master_list_selection

#rm(list=ls()) # Clear everything from memory


Master_list_selection <- c("CDT","HKE","LSD") # <- Put species Code/Codes for species of interest in here


WALKER <-  TRUE # Use Walker catchability coefficents

gc()
Home <- getwd()
setwd(Home)

{if (!require('rgdal')) install.packages('rgdal'); library('rgdal')
  if (!require('geoR')) install.packages('geoR'); library('geoR')
  if (!require('sp')) install.packages('sp'); library('sp')
  # if (!require('spatialEco')) install.packages('spatialEco'); library('spatialEco')
  # if (!require('RColorBrewer')) install.packages('RColorBrewer'); library('RColorBrewer')
  if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
  if (!require('gtools')) install.packages('gtools'); library('gtools')
  if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
  if (!require('rgeos')) install.packages('rgeos'); library('rgeos')
  if (!require('stringr')) install.packages('stringr'); library('stringr')
  if (!require('viridis')) install.packages('viridis'); library('viridis')
  if (!require('raster')) install.packages('raster'); library('raster')
  if (!require('RandomFields')) install.packages('RandomFields'); library('RandomFields')
  if (!require('beepr')) install.packages('beepr'); library('beepr')
  if (!require('plyr')) install.packages('plyr'); library('plyr')
  if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
 # if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
  }  


# source("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/__PAPER 1/ESTIMATION_FUNCTION.R") # Multistart function 

Loops <- 10 # number of times estmiation is repeated in order to find an average

# Set to test scale
TEST = FALSE
#TEST = TRUE
SCALE = 20 # 1/ SCALE

# Vessel Colours
col1 <- col.CEXP <- "#399d20"
col2 <- col.THA2 <- "#c43b3b"
col3 <- col.SCO3 <- "#3b58c4"


# Juvenile / Adult colours
JUV <- "#31B57B"
ADULT <- "#BCB16F"


# Projections
WGS84 <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"  # WGS84
LAEA <- "+proj=laea +lat_0=51 +lon_0=-8 +x_0=4321000 +y_0=3210000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
IrishGrid <- "+proj=tmerc +lat_0=53.5 +lon_0=-8 +k=1.000035 +x_0=200000 +y_0=250000 +a=6377340.189 +b=6356034.447938534 +units=m +no_defs"

# Boundary polygons
CS <- readOGR("./Data","CS_OUTLINE") # Celtic Sea Outline
Eur <- readOGR("./Data",layer = "europe") # Europe Outline
IS <- readOGR("./Data",layer = "IrishSea")
#Stat_rect <- readOGR("../DATA/ICES",layer = "ICES_StatisticalRectangles_CS_WGS84_CelticSeas-REGION3") # ICES statistical rectangles


# Holes in the kriging grid
IE <- raster::getData(country = "Ireland", level = 0, download = FALSE, path = paste0(Home, "/Data"))
UK <- raster::getData(country = "United Kingdom", level = 0, download = FALSE, path = paste0(Home, "/Data"))
FR <- raster::getData(country = "France", level = 0, download = FALSE, path = paste0(Home, "/Data"))
IM <- raster::getData(country = "Isle of Man", level = 0, download = FALSE, path = paste0(Home, "/Data"))

# IE <- spTransform(IE, CRS(LAEA))
# UK <- spTransform(UK, CRS(LAEA))
# FR <- spTransform(FR, CRS(LAEA))
# process polygon data for use in gglot
# Celtic Seas ecoregion
CS@data$id = rownames(CS@data)
CS.points = fortify(CS, region="id")
CS.df = join(CS.points, CS@data, by="id")
# Ireland
IE@data$id = rownames(IE@data)
IE.points = fortify(IE, region="id")
IE.df = join(IE.points, IE@data, by="id")
# UK
UK@data$id = rownames(UK@data)
UK.points = fortify(UK, region="id")
UK.df = join(UK.points, UK@data, by="id")
# Isle of Man
IM@data$id = rownames(IM@data)
IM.points = fortify(IM, region="id")
IM.df = join(IM.points, IM@data, by="id")

# France
FR@data$id = rownames(FR@data)
FR.points = fortify(FR, region="id")
FR.df = join(FR.points, FR@data, by="id")

# Transform data sets to LAEA
CS_LAEA <- spTransform(CS, CRS(LAEA))
Eur_LAEA <- spTransform(Eur, CRS(LAEA))
IS_LAEA <- spTransform(IS, CRS(LAEA))
#Stat_rect <- spTransform(Stat_rect, CRS(IrishGrid))

# Create buffer using covaraince distance

Hauls <- read.csv("Hauls.csv")

# GENERATING BUFFER EQUAL TO 0.5 COVARAINCE DISTANCE
# #### Preping tral locations

# Getting haul locations by vessel (its less taxing on processing)
CEXP <- Hauls[Hauls$Ship == "CEXP",]
THA2<- Hauls[Hauls$Ship == "THA2",]
SCO3<- Hauls[Hauls$Ship == "SCO3",]
# Make them spatail 
coordinates(CEXP) <- ~ShootLong_degdec+ShootLat_degdec
coordinates(THA2) <- ~ShootLong_degdec+ShootLat_degdec
coordinates(SCO3) <- ~ShootLong_degdec+ShootLat_degdec

# Give them a Coordinate Reference System
CEXP <- SpatialPoints(CEXP, proj4string =CRS( proj4string(CS)) )
THA2 <- SpatialPoints(THA2, proj4string =CRS( proj4string(CS)) )
SCO3 <- SpatialPoints(SCO3, proj4string =CRS( proj4string(CS)) )
proj4string(CEXP) # TEST it should be wgs84
# Change projection to LAEA for distance in meters
CEXP <- spTransform(CEXP, CRS(LAEA))
THA2 <- spTransform(THA2, CRS(LAEA))
SCO3 <- spTransform(SCO3, CRS(LAEA))
proj4string(SCO3) # TEST it should be LAEA
head(SCO3) # Coordinates are in meters



Data_location <- paste0(Home,"/Data")
#setwd(Data_location)
Folders <-  as.data.frame(as.character(list.dirs(path = "./Data", recursive=FALSE)))
colnames(Folders) <- "Folders"


# CREATE DATA LOG FOR RESULTS
RESULTS <- RESULTS_NEW <- data.frame(Year = NA,
                                     SpeciesSciName= NA, 
                                     CommonName = NA,
                                     Code = NA,
                                     Group = NA,
                                     Maturity = NA,
                                     Lengths = NA, 
                                     beta0 = NA,
                                     beta1 = NA,
                                     beta2 = NA,
                                     beta3 = NA,
                                     beta4 = NA,
                                     beta5= NA,
                                     beta6 = NA,
                                     beta7 = NA,
                                     beta8 = NA,
                                     beta9 = NA,
                                     beta10 = NA,
                                     beta11 = NA,
                                     beta12 = NA,
                                     beta13 = NA,
                                     beta14 = NA, 
                                     beta15 = NA,
                                     beta16 = NA,
                                     beta17 = NA,
                                     beta18 = NA,
                                     beta19 = NA,
                                     beta20 = NA,
                                     Sigmasq = NA,
                                     scaled_phi = NA,
                                     phi = NA,
                                     kappa = NA,
                                     loglik = NA,
                                     tausq = NA,
                                     practicalRange = NA,
                                     Transformation = NA, 
                                     n = NA,
                                     CEXP_pcnt = NA,
                                     THA2_pcnt = NA,
                                     SCO3_pcnt = NA,
                                     MaxDist = NA,
                                     AIC = NA,
                                     BIC = NA
)

#param_start <- read.csv("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/__PAPER 1/FINAL RESULTS/CPUE/Param_starting_values.csv")# HACK FOR FASTER MODEL FITS FROM PREVIOUS MODELS
#Species_selection <- data.frame(species_code =c("CD","H","LSD")) #HACK



# Generate a table with at least species name, common name and species code for the folders  
common <- as.data.frame(gsub("./Data/", "\\1", Folders$Folders), stringsAsFactors = FALSE)
colnames(common) <- "Species"


Walker <- read.csv("./Data/LaM_Estimates.csv", stringsAsFactors = FALSE)
SpeciesList <- read.csv("./Data/Walker_species_list.csv", header = TRUE, sep = "\t", stringsAsFactors = FALSE)

{Master_list <-  data.frame( Species = rep(NA, length(common$Species)), Common = NA, Code = NA, Group = NA)
  for (i in 1:length(common$Species)){
    Master_list[i,] <-  SpeciesList[SpeciesList$Common == common$Species[i],]
    print(i)
  }}

Master_list 



# Choose species from Master list using species code

Maturity_selection <- data.frame(maturity_code = c(1,2)) # to Identify qhcih maturity is being processed 


Master_list # read species coeds from Master_list and add to species_selection the species to be assessed
Species_selection <- Master_list[Master_list$Code %in% Master_list_selection,]




for (f in 1:Loops){ # Number of loops to run in order to average results
  
  # START THE LOOP
  for (m in 1:nrow(Species_selection)){ 
    # Select data folder
    # Create a file to hold the results of each run
    # Identify data files for each maturity 
    
    Species_selection_selected <- Species_selection$Common[m]  #
    
    gc()  
    #setwd(paste0(Data_location, sub(".","", Folders[m,])))  
    setwd(paste0(Data_location,"/", Species_selection[m,]$Common))  
    getwd()
    
    
    ifelse( 
      file.exists(paste0(getwd(),"/__RESULTS_CS.CSV")), #TEST
      print("__RESULTS_CS.CSV exists"),               #TRUE
      c( write.csv(RESULTS_NEW, "__RESULTS_CS.CSV", row.names=FALSE), print("__RESULTS_CS.CSV created") )             #FALSE
    )
    
    
    
    Files <- mixedsort(list.files(pattern = "\\.csv$"))[1:2] # show files in bio data
    
    ifelse( length(Files)== 0,# Skip to next loop if there are no files 
            next,
            paste("Files detected"))
    
    
    Lengths <- data.frame(length_cm = NA)
    
    
    # __________________________________________________________________________________________________
    # Select maturity
    
    for (j in 1:length(Files)){ # j is Maturity
      
      
      
      Maturity_model <<-  data.frame(Maturity = c(1,2), phi = NA,    # blank Data.frame for plotting maturity covaraince strucutres
                                     kappa = NA, 
                                     sigmasq = NA,
                                     tausq = NA)
      
      gc()  
      try(suppressWarnings({dev.off();dev.off();dev.off()})  )
      
      Maturity_selection_selected <- Maturity_selection$maturity_code[j] #Maturity code 1 = juvenile, 2 = Adult
      
      # Set name for plots
      plot_name_sub <- paste0("__",Species_selection_selected," ", Maturity_selection_selected," ")
      #setwd(Data_location)
      print(getwd())
      
      ifelse(str_detect(as.character(Files[j]), "RESULTS"), # to prevent results files from stopping the loop
             next,
             print(Files[j]))
      
      
      
      
      ifelse(is.na(Files[j]),
             next,
             paste("Files detected"))
      
      {
        DATA.RAW <- read.csv(Files[j])  
        nrow(DATA.RAW)
        
        if( TEST == TRUE){
          DATA.RAW <- sample_n(DATA.RAW, nrow(DATA.RAW)/SCALE)
          print("Scale reduced for testing")} # REDUCE SCALE FOR TESTING
        
        nrow(DATA.RAW)
        
        Species <-  unique(DATA.RAW$SpeciesSciName)
        CommonName <- unique(DATA.RAW$CommonName)
        YearRange <- paste0(min(unique(DATA.RAW$YearShot)),"-", max(unique(DATA.RAW$YearShot)))
        Lengths[j,] <- as.character(unique(DATA.RAW$FishLength_cm)[1])
        
        
        
        
        head(DATA.RAW)
        
        
        # Make Raw data a spatail points dataframe
        DATA.RAW <- DATA <- SpatialPointsDataFrame(cbind(DATA.RAW$ShootLong_degdec, DATA.RAW$ShootLat_degdec), as.data.frame(DATA.RAW)) # Make 
        proj4string(DATA.RAW) <- WGS84 # Apply projection
        DATA.RAW <- spTransform(DATA.RAW, CRS(LAEA)) # Convert to LAEA
        unique(DATA.RAW@data$Ship)
        
        DATA.RAW <- cbind(DATA.RAW@coords, DATA.RAW@data)
        
        dim(DATA.RAW)
        POINTS <- DATA.RAW
        dim(POINTS)
        
        
        #POINTS####
        # Data filtering
        dim(POINTS)
        POINTS <- POINTS[!duplicated(POINTS$HaulID),]     # Remove duplicate Haul IDs. Should be none to remove 
        dim(POINTS)
        
        # ########## OPTION TO USE WALKER COEFFS OR NOT ###### 

        ifelse(WALKER == TRUE,
               {
        # Data Treatment # MORE ADVANCED ZERO INFLATION MODEL TO BE APPLIED 
        z_tmp <- POINTS$DensAbund_N_Sqkm
        Zero_patch <- min(POINTS[POINTS$DensAbund_N_Sqkm != 0,]$DensAbund_N_Sqkm) /2 # lowest value not equal to 0
        (Zero_hauls <- sum(POINTS$DensAbund_N_Sqkm==0)) # Number of 0 hauls
        POINTS[POINTS$DensAbund_N_Sqkm == 0,]$DensAbund_N_Sqkm <- rep(Zero_patch, Zero_hauls) # Apply zero patch
        sum(POINTS$DensAbund_N_Sqkm==Zero_patch) # Number of 0 hauls converted to half the lowest value
        (Zero_hauls <- sum(POINTS$DensAbund_N_Sqkm==0)) # Number of 0 hauls not fixed to Zero_patch
        
        n <- nrow(POINTS)
        
        
        # Work out ratio of hauls in given year
        Vessel.Ratio <-  paste0( 
          round(100 /n * nrow(POINTS[POINTS$Ship == "CEXP",]),0) ,"% ", "CEXP"," ",
          round(100 /n * nrow(POINTS[POINTS$Ship == "THA2",]),0) ,"% ", "THA2"," ",
          round(100 /n * nrow(POINTS[POINTS$Ship == "SCO3",]),0) ,"% ", "SCO3")
        
        
        
        
        # NEXT LOOP IF THERE ARE less than 10  POINTS
        ifelse(nrow(POINTS) < 10,
               c(paste("Not enough points in",Lengths[j,],"cm class"),{next}),
               paste("Progessing to beta estimation"))
        
        summary(POINTS[POINTS$Ship == "CEXP",]$DensAbund_N_Sqkm)
        summary(POINTS[POINTS$Ship == "THA2",]$DensAbund_N_Sqkm)
        summary(POINTS[POINTS$Ship == "SCO3",]$DensAbund_N_Sqkm)
        
        
        # dotchart(POINTS$DensAbund_N_Sqkm, groups = factor(POINTS$Ship))
        # dotchart(log(POINTS$DensAbund_N_Sqkm), groups = factor(POINTS$Ship))
        # 
        
        # CONVERT INTO KILOMETERS
        x <- as.numeric(POINTS$coords.x1/1000)
        y <- as.numeric(POINTS$coords.x2/1000)
        z <- as.numeric(log(POINTS$DensAbund_N_Sqkm)) # NOTE THIS IS LOG TRANSFORMED
               },
        
        #### IF WALKER == FALSE USE THE RAW DENSABUND SQKM
        
        {        # Data Treatment # MORE ADVANCED ZERO INFLATION MODEL TO BE APPLIED 
          z_tmp <- POINTS$DensAbund_N_Sqkm_RAW
          Zero_patch <- min(POINTS[POINTS$DensAbund_N_Sqkm_RAW != 0,]$DensAbund_N_Sqkm_RAW) /2 # lowest value not equal to 0
          (Zero_hauls <- sum(POINTS$DensAbund_N_Sqkm_RAW==0)) # Number of 0 hauls
          POINTS[POINTS$DensAbund_N_Sqkm_RAW == 0,]$DensAbund_N_Sqkm_RAW <- rep(Zero_patch, Zero_hauls) # Apply zero patch
          sum(POINTS$DensAbund_N_Sqkm_RAW==Zero_patch) # Number of 0 hauls converted to half the lowest value
          (Zero_hauls <- sum(POINTS$DensAbund_N_Sqkm_RAW==0)) # Number of 0 hauls not fixed to Zero_patch
          
          n <- nrow(POINTS)
          
          
          # Work out ratio of hauls in given year
          Vessel.Ratio <-  paste0( 
            round(100 /n * nrow(POINTS[POINTS$Ship == "CEXP",]),0) ,"% ", "CEXP"," ",
            round(100 /n * nrow(POINTS[POINTS$Ship == "THA2",]),0) ,"% ", "THA2"," ",
            round(100 /n * nrow(POINTS[POINTS$Ship == "SCO3",]),0) ,"% ", "SCO3")
          
          
          
          
          # NEXT LOOP IF THERE ARE less than 10  POINTS
          ifelse(nrow(POINTS) < 10,
                 c(paste("Not enough points in",Lengths[j,],"cm class"),{next}),
                 paste("Progessing to beta estimation"))
          
          summary(POINTS[POINTS$Ship == "CEXP",]$DensAbund_N_Sqkm_RAW)
          summary(POINTS[POINTS$Ship == "THA2",]$DensAbund_N_Sqkm_RAW)
          summary(POINTS[POINTS$Ship == "SCO3",]$DensAbund_N_Sqkm_RAW)
          
          
          # dotchart(POINTS$DensAbund_N_Sqkm_RAW, groups = factor(POINTS$Ship))
          # dotchart(log(POINTS$DensAbund_N_Sqkm_RAW), groups = factor(POINTS$Ship))
          # 
          
          # CONVERT INTO KILOMETERS
          x <- as.numeric(POINTS$coords.x1/1000)
          y <- as.numeric(POINTS$coords.x2/1000)
          z <- as.numeric(log(POINTS$DensAbund_N_Sqkm_RAW)) # NOTE THIS IS LOG TRANSFORMED
         })
        
        # ########## OPTION TO USE WALKER COEFFS OR NOT ###### END !!!!!!!!!!!!!!
        
        
        
        #Ship <- droplevels(POINTS$Ship)
        Ship <- factor(as.character(POINTS$Ship))#,levels = c("CEXP", "THA2", "SCO3"))
        Year <- factor(POINTS$YearShot)
        
        
        # Find the best transformation to fit normaility
        # tryCatch(
        # suppressWarnings({ Transform <- bestNormalize::bestNormalize(z)})
        #  , error = {next})
        # z <- predict(Transform)
        xyz.raw <- data.frame(x = x, y= y, z= z, Ship = Ship, Year = Year)
        n <- nrow(xyz.raw)
        
        
        
        #### Check for duplicate coordinates ####
        max.jit <- 1 # maximum distance to jitter duplicte coordinates
        ifelse(  length(dup.coords(xyz.raw[,1:2]))>0 
                 # TRUE
                 ,c(paste(length(dup.coords(xyz.raw[,1:2])), "duplicate coordinates found and have been jittered randomly by a maximum of", max.jit, "unit(s) of distance"),
                    Jitter <- jitterDupCoords(xyz.raw[,1:2], max = max.jit),
                    xyz.tmp <- cbind(Jitter),
                    xyzFullScale <- xyz <<- data.frame(xyz.tmp, z = xyz.raw$z, Ship = xyz.raw$Ship, Year = xyz.raw$Year))
                 , c(paste("0 duplicates found, no jittering of duplicate coordinates required"),
                     x <- as.numeric(POINTS$coords.x1),
                     y <- as.numeric(POINTS$coords.x2),
                     z <- z,
                     xyzFullScale <-xyz <<- data.frame(x = xyz.raw$x, y= xyz.raw$y, z= xyz.raw$z, Ship = xyz.raw$Ship, Year = xyz.raw$Year)))
        
        
        
        # Choosing colour pallet based on whether it is adult of Juvenile
        ifelse( Maturity_selection_selected == 1,
                c(Viridis_option <-  "viridis", COL <- JUV), # Juv
                c(Viridis_option <-  "viridis", COL <-  ADULT) # Adult     # CHANGED FROM cividis 15.11.19
        )
        
        # SCALE COORDINATES BY THE MAXIMUM DISTANCE
        MaxDist <- max(dist(cbind(xyz$x, xyz$y))) # THIS IS IN KILOMETERS
        xyz$x <-  xyz$x / MaxDist
        xyz$y <-  xyz$y / MaxDist
        
        # adjustment for polynomial fit to be reversed post estimation 
        mean_x <- mean(xyz$x)#
        mean_y <- mean(xyz$y)
        
        xyz$x <- xyz$x - mean_x
        xyz$y <- xyz$y - mean_y
        h.dat <- as.geodata(xyz, coords.col = 1:2, data.col = 3, covar.col = c(4,5)) # With Ship as a covaraite
      }
      
      
      
      #### NORMALITY PLOTS ####
      {Ratio.text <-  paste0(n, " points ", 
                             Vessel.Ratio )
      Trans <- "log"
      Plot_name1 <- paste(plot_name_sub,"NORMALITY.png")
      png(Plot_name1)
      par(mfrow=c(1,2),oma = c(0, 0, 4, 0))
      qqnorm(z, asp = 1)
      qqline(z)
      Norm.test <- shapiro.test(z)
      MASS::truehist(z, main = paste( Trans, "transformed"), col =         
                       ifelse(Maturity_selection_selected == 1,
                              JUV, # Juv
                              ADULT # Adult     # CHANGED FROM cividis 15.11.19
                       )
      )
      title(  paste(Lengths[j,],"cm" , Species,YearRange,"\n", "Shapiro p=",round(Norm.test$p.value,2),"\n", Ratio.text) , cex = 2, outer = TRUE)
      par(mfrow = c(1, 1))
      dev.off()}
      
      
      
      print(paste("### Currently processing",CommonName,Lengths[j,],"cm ###"))
      
      
      
      #### Cloud and bin variogram fit ####
      
      
      
      # par(mfrow=c(1,2),oma = c(0, 0, 4, 0))
      
      # FullScale_xy <-  xyz <- dplyr::arrange(xyz, Ship) # Order by ship to make sure that CEXP is always beta0, THA2 beta1 and SCO3 beta2 
      
      # scale coordinates between 0 and 1 to allow model to converge
      # xyz$x <-  (xyz$x - min(xyz$x))  / max(xyz$x - min(xyz$x))
      # xyz$y <-  (xyz$y - min(xyz$y))  / max(xyz$y - min(xyz$y))
      
      
      
      
      
      
      #   h.dat.full <- as.geodata(FullScale_xy, coords.col = 1:2, data.col = 3, covar.col = c(4,5)) # With Ship as a covaraite
      #  h.dat$covar$Year <- factor(h.dat$covar$Year)
      # str(h.dat)
      
      # v1 <- variog(h.dat, option = "cloud", trend = "2nd", max.dist = 1)# cloud variogram
      # plot(v1, col = COL, xlab = "distance (km)", main = paste(Lengths[j,],"cm cloud variogram"))
      
      
      #trend <- trend.spatial(h.dat, trend = ~ Year + Ship ) # Original code
      
      try({rm(trend)})
      
      # try({trend <- trend.spatial(h.dat, trend = ~ Year + Ship)})
      trend <- trend.spatial(h.dat, trend = "2nd", add.to.trend = ~ Year + Ship)
      
      ifelse(exists("trend")==TRUE,
             print("trend exists"),
             c( try(dev.off), next))  
      
           rm(likfit.fit)
      
      print(paste("Loop",m,"of", Loops, "for",m, "of", nrow(Species_selection), "species", Sys.time())) # Just so we know how far along the estimation is
      
      
      try({
        BREAK <- FALSE
      for (B in 1:3){  
        
        if(BREAK == TRUE){next} # If phi is less than the maximum distance between 2 points  end this loop
                                # If phi is greater than MaxDsist, repeat estimation
        if(B== 3){break
          print(paste(CommonName, "does not enough to estimate strucutre, try a different species"))}
      (fit  <- likfit.fit <- likfit(h.dat,
                             cov.model="matern",
                             lik.method = "ML",
                             kappa = 0.1, # Starting parameter for kappa
                             fix.kappa = FALSE,
                             ini.cov.pars = c(0.1, min(dist(h.dat$coords))), # Starting parameters for sigmasq and phi
                             trend = trend,
                             fix.nugget = FALSE,
                             print.pars = FALSE))
                             
      BREAK <<-   fit$phi * MaxDist < MaxDist # if phi is greater than the maximum distance between 2 points then repeat the estimation
          
     # print(paste("phi greater than maximum distance (", round(MaxDist,0),"km) between two points. Repeating ;likfit estimation due to likely estimation error"))
        }
        })
      
      
      
      # SAVE RESULTS 
      # Scale phi to KM
      likfit.fit$phi <- fit$phi * MaxDist
      
      
      # Dataframe for plotting matrurity variograms and covaraince structures
      Maturity_model$phi[j] <- likfit.fit$phi
      Maturity_model$kappa[j] <- likfit.fit$kappa
      Maturity_model$sigmasq[j] <- likfit.fit$sigmasq
      Maturity_model$tausq[j] <- likfit.fit$tausq
      
      
      RESULTS <- data.frame(Year = YearRange,
                            SpeciesSciName= Species_selection$Species[m], 
                            CommonName = Species_selection[m,]$Common,
                            Code = Species_selection[m,]$Code,
                            Group = Species_selection[m,]$Group,
                            Maturity = j,
                            Lengths = paste0(" ",Lengths[j,]), 
                            beta0 = likfit.fit$parameters.summary$values[1],
                            beta1 = likfit.fit$parameters.summary$values[2],
                            beta2 = likfit.fit$parameters.summary$values[3],
                            beta3 = likfit.fit$parameters.summary$values[4],
                            beta4 = likfit.fit$parameters.summary$values[5],
                            beta5 = likfit.fit$parameters.summary$values[6],
                            beta6 = likfit.fit$parameters.summary$values[7],
                            beta7 = likfit.fit$parameters.summary$values[8],
                            beta8 = likfit.fit$parameters.summary$values[9],
                            beta9 = likfit.fit$parameters.summary$values[10],
                            beta10 = likfit.fit$parameters.summary$values[11],
                            beta11 = likfit.fit$parameters.summary$values[12],
                            beta12 = likfit.fit$parameters.summary$values[13],
                            beta13 = likfit.fit$parameters.summary$values[14],
                            beta14 = likfit.fit$parameters.summary$values[15], 
                            beta15 = likfit.fit$parameters.summary$values[16],
                            beta16 = likfit.fit$parameters.summary$values[17],
                            beta17 = likfit.fit$parameters.summary$values[18],
                            beta18 = likfit.fit$parameters.summary$values[19],
                            beta19 = likfit.fit$parameters.summary$values[20],
                            beta20 = likfit.fit$parameters.summary$values[21],
                            Sigmasq = likfit.fit$sigmasq,
                            scaled_phi = fit$phi,
                            phi = likfit.fit$phi,
                            kappa = likfit.fit$kappa,
                            loglik = likfit.fit$loglik,
                            tausq = likfit.fit$tausq,
                            practicalRange = practicalRange("matern", phi = likfit.fit$phi, kappa = likfit.fit$kappa),
                            Transformation = "log", 
                            n = n,
                            CEXP_pcnt = round(100 /n * nrow(POINTS[POINTS$Ship == "CEXP",]),0),
                            THA2_pcnt = round(100 /n * nrow(POINTS[POINTS$Ship == "THA2",]),0),
                            SCO3_pcnt = round(100 /n * nrow(POINTS[POINTS$Ship == "SCO3",]),0),
                            MaxDist = MaxDist,
                            AIC = likfit.fit$AIC,
                            BIC = likfit.fit$BIC
      )
      
      
      
      # save to root folder
      try(rm(RESULTS.FILE))
      RESULTS.FILE <- na.omit(read.csv( paste0(getwd(),"/__RESULTS_CS.CSV"), stringsAsFactors = FALSE))
      RESULTS.FILE$X <- NULL
      colnames(RESULTS.FILE) <- colnames(RESULTS)
      
     # if(RESULTS.FILE[RESULTS.FILE$phi > MaxDist, ]$phi <- NA) # REMOVE ROWS WITH PHI VALUE LARGER THAN THE MAXIMUM DISTANCE BETWEEN TWO SAMPLE LOCATIONS
      RESULTS_BIND <- na.omit(rbind(RESULTS.FILE, RESULTS))
      
      
      dat0 <- na.omit(RESULTS_BIND)
      
      Averaged <- data.frame( Maturity = c(1,2), 
                         phi = c(mean(dat0[dat0$Maturity == 1,]$phi), mean(dat0[dat0$Maturity == 2,]$phi)),
                         kappa = c(mean(dat0[dat0$Maturity == 1,]$kappa), mean(dat0[dat0$Maturity == 2,]$kappa)),
                         sigmasq = c(mean(dat0[dat0$Maturity == 1,]$Sigmasq), mean(dat0[dat0$Maturity == 2,]$Sigmasq)),
                         tausq = c(mean(dat0[dat0$Maturity == 1,]$tausq), mean(dat0[dat0$Maturity == 2,]$tausq))
      )
      
      # SAVE 
      write.csv(RESULTS_BIND, paste0(getwd(),"/__RESULTS_CS.CSV"), row.names=FALSE ) # write critical bit to table
      save.image(file = paste0(plot_name_sub,"ENVIRONMENT.rData")) # Save environment
      saveRDS(list(fit, likfit.fit, h.dat, common, m, j  ), paste0(plot_name_sub,"model_dataframes.RDS")) # save likfit model
      # saveRDS(likfit.fit, paste0(plot_name_sub,"likfit_model_Scaled.RDS")) # save likfit model
      # saveRDS(kcE, paste0(plot_name_sub,"krige.RDS"))
      dat <- h.dat
      dat$coords <- h.dat$coords * MaxDist 
      
      
      
      
      if(f==Loops){
             


      ########################## PLOTS ################
      #####################
      ####################
      ############
      #########
      #####
      ##
      #

      #  EMPERICAL VARIOGRAM
      # plot_name_sub <- paste0("_",Species_selection_selected, Maturity_selection_selected," ")
      png(paste(plot_name_sub," EMPERICAL_VARIOGRAM.png"))
      ifelse( exists("likfit.fit") == TRUE, # TEST
              c(paste("Variogram estimation complete"), # TRUE
                v2 <- variog(h.dat, option = "bin", trend = "2nd"), # bin variogram
                plot(v2, col = ifelse(Maturity_selection_selected == 1,
                                      JUV, # Juv
                                      ADULT # Adult     # CHANGED FROM cividis 15.11.19
                ), xlab = "distance (km)", main = paste(Lengths[j,],"cm bin variogram")),
                lines(likfit.fit),
                title( paste(Lengths[j,],"cm" , unique(POINTS$CommonName)),cex = 2, outer = TRUE),
                par(mfrow = c(1,1)),
                dev.off())
              ,
              c(paste("Variogram estimation failed, possibly too few data points"), # FALSE
                title( paste(Lengths[j,],"cm" , unique(POINTS$com),"\n", unique(POINTS$Ship)[1], unique(POINTS$Ship)[2], "overlap","\n", Ratio.text,"\nLIKFIT ESTIMATION FAILED"),
                       cex = 2, outer = TRUE),
                dev.off(),
                next ))

      ####
      ###
      ##
      #



      # Generate a GMRF to show averged structure
      res_x <- res_y <-  seq(0, MaxDist , 1)
      model <- RMmatern(nu =  Averaged$kappa[j], scale = Averaged$phi[j], var = Averaged$sigmasq[j])# + RMtrend(mean = 100)
      try(ifelse(Averaged$phi[j]== 0,# TEST
                 next,                           # TRUE
                 c(                              # FALSE
                   # Generate Simulation
                   simu <- RFsimulate(model, res_x, res_y, grid=TRUE, seed = 1, RFoptions(spConform=FALSE)), # Generate simulation
                   simu.rast <- raster(simu),
                   r.spdf <- as(simu, "SpatialPixelsDataFrame"), # Convert to Raster
                   r.df <- as.data.frame(r.spdf),
                   colnames(r.df) <- c("z", "x", "y")#, # Convert to dataframe
                 )))
      #PLOT FOR PAPER#

      # Size range being printed
      Lengths[j,]

      Sim <- ggplot() + geom_tile(data=r.df, aes(x=x, y=y, fill=z)) + scale_fill_viridis(option = "viridis", limits = c(-0.21, 8.9)) +  coord_equal() + labs( x = "x km", y = "y km" ) + labs(subtitle = paste("phi =", round(Averaged$phi[j], 2), "   kappa = ", round(Averaged$kappa[j],2), "   mu = ", 0, "   sigmasq = ", round(Averaged$sigmasq[j], 2) )) + theme(plot.subtitle=element_text(size=15))

      Sim # Plot of the raw simulation
      ggsave(file = paste(plot_name_sub,"GMRF for paper.png"), width = 15.91, height = 15.91, units = "cm") # SAVE


      #RESIDUALS
      png(paste(plot_name_sub,"likfit_RESIDUALS.png"))
      hist(likfit.fit$model.components$residuals, main =
             paste("likfit residuals Shapiro =",
                   round(shapiro.test(likfit.fit$model.components$residuals)$p,2)
                   ,"\n",
                   Lengths[j,],"cm" ,
                   unique(POINTS$SpeciesSciName),
                   "Celtic Sea",
                   YearRange, "\n",
                   Ratio.text)
           , col= ifelse(Maturity_selection_selected == 1,
                         JUV, # Juv
                         ADULT # Adult     # CHANGED FROM cividis 15.11.19
           ))
      dev.off()


      ####
      ###
      ##
      #

      # Plot to show difference in abundnace between years
      
  # Abundance_df <- data.frame(Year = min(DATA$YearShot): max(DATA$YearShot), Log_Abundance = c("2003" = likfit.fit$beta[1], 
  #                 "2004" = likfit.fit$beta[1] + likfit.fit$beta[7],
  #                 "2005" = likfit.fit$beta[1] + likfit.fit$beta[8],
  #                 "2006" = likfit.fit$beta[1] + likfit.fit$beta[9],
  #                 "2007" = likfit.fit$beta[1] + likfit.fit$beta[10],
  #                 "2008" = likfit.fit$beta[1] + likfit.fit$beta[11],
  #                 "2009" = likfit.fit$beta[1] + likfit.fit$beta[12],
  #                 "2010" = likfit.fit$beta[1] + likfit.fit$beta[13],
  #                 "2011" = likfit.fit$beta[1] + likfit.fit$beta[14],
  #                 "2012" = likfit.fit$beta[1] + likfit.fit$beta[15],
  #                 "2013" = likfit.fit$beta[1] + likfit.fit$beta[16],
  #                 "2014" = likfit.fit$beta[1] + likfit.fit$beta[17],
  #                 "2015" = likfit.fit$beta[1] + likfit.fit$beta[18],
  #                 "2016" = likfit.fit$beta[1] + likfit.fit$beta[19]))
  # rownames(Abundance_df) <- NULL
  # 
  # plot(Abundance_df)
      
      ####
      ###
      ##
      #
      

      # KRIGING RESULTS
      # Extent of CS Scaled
      pred.bbox <- (CS_LAEA@bbox/MaxDist)/1000
      # Pred girf
      pred.grid = expand.grid(seq(min(pred.bbox[1,]), max(pred.bbox[1,]), l = 200), seq(min(pred.bbox[2,]), max(pred.bbox[2,]), l = 200))

      # Readjusting zeros
      Zero_patch <- min(h.dat$data)
      TRUE_ZEROS <- h.dat$data == Zero_patch
      sum(h.dat$data == Zero_patch)
      tmp <- h.dat$data
      tmp[tmp == Zero_patch] <- 0
      h.dat_withZeros <- h.dat
      h.dat_withZeros$data <- as.matrix(tmp)
      sum(h.dat_withZeros$data == 0) == sum(h.dat$data == Zero_patch)


      # PATCH TO CORRECT DATA BY VESSEL AND BY YEAR
      # Bring all into a data.frame
      ################################### NOTE THAT WE ARE ADDING THE MEAN BACK TO THE COORDINATES HERE ##########################
      Coeff.tmp <- data.frame(x = h.dat$coords[,1] + mean_x, y = h.dat$coords[,2] + mean_y, z = h.dat_withZeros$data, Ship = h.dat$covar$Ship, YearShot = h.dat$covar$Year)


      #Load Results of previous runs
      # Estimations <- read.csv("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/__PAPER 1/FINAL RESULTS/CPUE/SCALED ESTIMATION AFTER HELP/LAST FINAL FINAL PLOTS/SUMMARY OF ALL RUNS.csv")



      range(h.dat_withZeros$data)
      

      # Applying catchability coefficents as an average of all the runs
      # # 2003  CEXP
      {Coeff.tmp[Coeff.tmp$YearShot == 2004,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2004,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta6)
      Coeff.tmp[Coeff.tmp$YearShot == 2005,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2005,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta7)
      Coeff.tmp[Coeff.tmp$YearShot == 2006,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2006,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta8)
      Coeff.tmp[Coeff.tmp$YearShot == 2007,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2007,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta9)
      Coeff.tmp[Coeff.tmp$YearShot == 2008,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2008,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta10)
      Coeff.tmp[Coeff.tmp$YearShot == 2009,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2009,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta11)
      Coeff.tmp[Coeff.tmp$YearShot == 2010,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2010,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta12)
      Coeff.tmp[Coeff.tmp$YearShot == 2011,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2011,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta13)
      Coeff.tmp[Coeff.tmp$YearShot == 2012,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2012,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta14)
      Coeff.tmp[Coeff.tmp$YearShot == 2013,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2013,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta15)
      Coeff.tmp[Coeff.tmp$YearShot == 2014,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2014,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta16)
      Coeff.tmp[Coeff.tmp$YearShot == 2015,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2015,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta17)
      Coeff.tmp[Coeff.tmp$YearShot == 2016,]$z <- Coeff.tmp[Coeff.tmp$YearShot == 2016,]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta18)
      Coeff.tmp[Coeff.tmp$Ship == "THA2",]$z <- Coeff.tmp[Coeff.tmp$Ship == "THA2",]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta19)
      Coeff.tmp[Coeff.tmp$Ship == "SCO3",]$z <- Coeff.tmp[Coeff.tmp$Ship == "SCO3",]$z + mean(RESULTS.FILE[RESULTS.FILE$Maturity == j,]$beta20)}


     for(Z in 1: nrow(Coeff.tmp)){ # FIX ZEROS TO BE TRUE ZEROS
         if(  TRUE_ZEROS[Z]==TRUE){
              Coeff.tmp$z[Z] <- 0}}
      
      range(Coeff.tmp$z)



      Processed_data <- as.geodata(Coeff.tmp, coords.col = 1:2, data.col = 3, covar.col = 4:5)



      # ###########################
      #
      # # Kriging
      kcE = krige.conv(Processed_data, loc = pred.grid, krige = krige.control(trend.d = "2nd", trend.l = "2nd", obj.m = likfit.fit))
      # image(kcE, col = rev(heat.colors(20)), xlab = "Longitude", ylab = "Latitude", asp = 1)
      # points(h.dat$coords, cex = log(h.dat$data), asp = 1)

      r <- SpatialPointsDataFrame(pred.grid * MaxDist * 1000, data.frame(predict = kcE$predict))
      gridded(r) <- TRUE
      r <- as(r,'RasterLayer')
      crs(r) <- LAEA


      # Only needed as a low res example output
      if(TEST == TRUE){
      png("example of 2nd order spatial fit.png", width=900, height=800)
      r_WGS84 <- projectRaster(r, crs=WGS84)
      plot(r_WGS84, col = rev(heat.colors(20)), xlab = "Longitude", ylab = "Latitude", asp = 1, main = paste(Lengths[j,], CommonName,"- kriged log Abundance from",length(h.dat$data)," samples"))
      Points <- SpatialPointsDataFrame(h.dat$coords * MaxDist * 1000, data = data.frame(h.dat_withZeros$data))
      crs(Points) <- LAEA
      Points.WGS84 <-  spTransform(Points, CRS(WGS84))
      points(Points.WGS84, cex = h.dat_withZeros$data)
      Points.WGS84_ZEROS <- Points.WGS84[Points.WGS84$h.dat_withZeros.data ==0 ,]
      points(Points.WGS84_ZEROS, pch = 1)
      plot(Eur, add = TRUE, col = "grey1")
      legend(title = "Haul Location\nlog Abundace", x = -14, y = 59,  c("8","6","4","2","0"), col = 1, pch =c(1,1,1,1,1), pt.cex = c(8,6,4,2,1), y.intersp=3, x.intersp = 2.5, box.col = NA, bg = "NA")
      dev.off()}
      
      
      ### CREATE BUFFER EQUAL TO 0.05 COVARAINCE DISTANCE
      PRACTICALRANGE <-  practicalRange("matern", phi = Averaged$phi[j], kappa = Averaged$kappa[j], correlation = 0.05)
      # Buffer points by 0.5 covaraince distance
      BufferCEXP <- gBuffer(CEXP, width = PRACTICALRANGE*1000) # * 1000 because its in meters
      BufferTHA2 <- gBuffer(THA2, width = PRACTICALRANGE*1000)
      BufferSCO3 <- gBuffer(SCO3, width = PRACTICALRANGE*1000)

      Bufferlist <- list(BufferCEXP, BufferTHA2, BufferSCO3)
      # join into 1 spatial object
      Practical_range_buffer <- do.call(bind, Bufferlist)

      # RASTER CLIPPING
      RangeMask <-  mask(r, Practical_range_buffer)
      CSMask <- mask(RangeMask, CS_LAEA)
      ISMask <- mask(CSMask, IS_LAEA, inverse=TRUE)
      LandMask <- mask(ISMask, Eur_LAEA, inverse=TRUE)
      rr <- projectRaster(LandMask, crs=WGS84) # Project into WGS84
      #image(rr)
      proj4string(rr)


      test_spdf <- as(rr, "SpatialPixelsDataFrame")
      test_df <- as.data.frame(test_spdf)
      colnames(test_df) <- c("value", "x", "y")
      ######################################################################
      krige.df <- data.frame(long = test_df$x, lat = test_df$y, n = test_df$value)

      # PLOTTING KRIGING OF RESULTS

      ### EXP
      # Plot krige model using ggplot # RESIDUALS ~~~~~~~~~~~~~
      ggplot(krige.df, aes(x=long, y=lat, fill=n)) + geom_raster() +
        scale_fill_viridis_c(option = "viridis", limits = c(-0.21, 8.9)) +
        theme(aspect.ratio=1) +
        ggtitle(paste(Lengths[j,],"cm", CommonName)) +
        coord_cartesian(xlim = c(extent(CS)[1], extent(CS)[2]), ylim = c(extent(CS)[3], extent(CS)[4] ))+
        #geom_polygon(data = CS.df,aes(long,lat,group=group), fill = NA, color = "grey1", size = .25)
        geom_polygon(data = IE.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        geom_polygon(data = UK.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        geom_polygon(data = IM.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        geom_polygon(data = FR.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        theme(aspect.ratio=1, plot.title=element_text(size=22), axis.text=element_text(size=15), axis.title = element_text(size = 20)) + labs(fill="log n") + xlab("Long") + ylab("Lat")

      ggsave(paste0(plot_name_sub," KRIGE.png"), width = 15.91, height = 15.91, units = "cm")



      # PLOTTING RESIDUALS

      Residials.df <- data.frame(x = x , y = y, Residuals = likfit.fit$model.components$residuals )

      ggplot(Residials.df , aes(x=POINTS$ShootLong_degdec, y=POINTS$ShootLat_degdec,)) + geom_point(aes(colour = Residuals)) +
        scale_colour_viridis_c(option = "magma") +
        theme(aspect.ratio=1) + ggtitle(paste(Lengths[j,],"cm", CommonName)) + coord_cartesian(xlim = c(extent(CS)[1], extent(CS)[2]), ylim = c(extent(CS)[3], extent(CS)[4] ))+
        geom_polygon(data = IE.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        geom_polygon(data = UK.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        geom_polygon(data = IM.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) +
        geom_polygon(data = FR.df,aes(long,lat,group=group), fill = "grey", color = "grey1", size = .25) + theme(aspect.ratio=1, plot.title=element_text(size=22), axis.text=element_text(size=15), axis.title = element_text(size = 20)) + labs(fill="Residuals") + xlab("Long") + ylab("Lat")
      # save plot
      ggsave(paste0(plot_name_sub,"RESIDUALS.png"), width = 15.91, height = 15.91, units = "cm")

      saveRDS(h.dat, file = paste0(plot_name_sub,"DATA.RDS")) # save data

      ####
      ###
      ##
      #


      # plot the covaraince strucutres for adult and juvenile

      d <- seq(0, MaxDist, 0.1)

      ifelse(j==2,
             {c(
               dat0 <- na.omit(RESULTS_BIND),

               dat <- data.frame( Maturity = c(1,2),
                           phi = c(mean(dat0[dat0$Maturity == 1,]$phi), mean(dat0[dat0$Maturity == 2,]$phi)),
                           kappa = c(mean(dat0[dat0$Maturity == 1,]$kappa), mean(dat0[dat0$Maturity == 2,]$kappa)),
                           sigmasq = c(mean(dat0[dat0$Maturity == 1,]$Sigmasq), mean(dat0[dat0$Maturity == 2,]$Sigmasq)),
                           tausq = c(mean(dat0[dat0$Maturity == 1,]$tausq), mean(dat0[dat0$Maturity == 2,]$tausq))
               ),


               png(  paste("__",CommonName, "Covariance Structure.png")   ),

               # Draw Curves for Juv and Adult in that year
               Juv <- geoR::matern(u = d, phi = dat$phi[1],  kappa = dat$kappa[1]),
               Adult <- geoR::matern(u = d, phi = dat$phi[2],  kappa = dat$kappa[2]),

               plot(c(0, max(d)), c(0,1), xlim = c(0,MaxDist), ylab = "covariance", xlab = "distance (km)", col = 0,
                    #main = paste(CommonName, "covaraince structure")
               ),
               lines(d, Juv, col = JUV, lwd = 3),
               lines(d, Adult, col = ADULT, lwd = 3),
               legend("top", c("Juvenile","Adult"), col = c(JUV, ADULT), lty = 1, lwd = 3),

               dev.off(),


               {png(  paste("__",CommonName, "variogram.png")   )
                 plot(c(0, max(d)), c(0,4.5), ylab = "Semivariance", xlab = "Distance (km)", col = 0)

                 # Draw Curves for Juv and Adult in that year
                 Juv <- (1-geoR::matern(u = d, phi = dat$phi[1],  kappa = dat$kappa[1])) * (dat$sigmasq[1]) + dat$tausq[1]
                 Adult <- (1-geoR::matern(u = d, phi = dat$phi[2],  kappa = dat$kappa[2])) * (dat$sigmasq[2]) + dat$tausq[2]

                 # lines(d, (dat$Sigmasq[1]- (Juv * dat$Sigmasq[1]) + dat$tausq[1]), col = JUV, lwd = 3)
                 lines(d, Juv, col = JUV, lwd = 3)
                 lines(d, Adult, col = ADULT, lwd = 3)
                 #
                 legend("bottom", c("Juvenile","Adult"), col = c(JUV, ADULT), lty = 1, lwd = 3)

                 dev.off()}

             )},
             #FALSE
             print(paste(CommonName, Maturity_selection$maturity_code[j],"complete"))
             )
      
      beep(2)   
      print(paste("Loop",f, "of", Loops, "for all species in Species_selection complete" ))
             }
      
      
    }
    
    
    
    
    
  }

# Combine all results into a table

Results.summary <- list()


for (R in 1:length(Species_selection$Species)){
setwd(paste0(Home,"/data/",Species_selection$Common[R] ))
  
spec <- read.csv("__RESULTS_CS.csv")  
spec.tmp <- cbind(rbind(spec[spec$Maturity == 1 ,][1:8][1,], spec[spec$Maturity == 2 ,][1:8][1,]),
rbind(colMeans(spec[spec$Maturity == 1 ,][9:35]), colMeans(spec[spec$Maturity == 2 ,][9:35])),
rbind( spec[spec$Maturity == 1 ,][35:44][1,],  spec[spec$Maturity == 2 ,][35:44][1,]))
Results.summary[[R]] <- spec.tmp


write.csv(spec.tmp, paste0("__",Species_selection$Common[R], " Results Summary.csv"), row.names=FALSE)

list.of.files <- list.files(getwd(), "__") # find the files that you want

try(dir.create(file.path(Home, "/Results/",Species_selection$Common[R] ), showWarnings = FALSE))
#setwd(file.path(Home, "/Results/",Species_selection$Common[R]))

# copy the files to the new folder
file.copy(list.of.files, file.path(Home,"Results",Species_selection$Common[R] ), overwrite = TRUE)
setwd(Home)}


(Final_Results_Summary <- do.call(rbind.data.frame, Results.summary)) # print to console



# Load previous results, combine with new results, remove duplicates and save 
ifelse(file.exists(file.path(Home,"Results","Analysis Summary.csv"))==TRUE,
# If TRUE
{
results.tmp <- read.csv((file.path(Home,"Results","Analysis Summary.csv")),stringsAsFactors = FALSE)
try({results.tmp$X.1 <- NULL
results.tmp$X <- NULL
Final_Results_Summary$X <- NULL})
results.tmp1 <- rbind(results.tmp, Final_Results_Summary) # Bind together previous results and new results
results_duplicates_removed <- results.tmp1[!duplicated(results.tmp1$practicalRange),] # remove duplicated rows
try(write.csv(results_duplicates_removed, file.path(Home,"Results","Analysis Summary.csv"), row.names=FALSE), silent = TRUE ) # write to file
  }, 
# If FALSE
{
try(Final_Results_Summary$X <- NULL)
try(write.csv(Final_Results_Summary, file.path(Home,"Results","Analysis Summary.csv"), row.names=FALSE), silent = TRUE ) } # Write to file
) 


# SD SUMMARY OF RESULTS
# tmp <- read.csv("C:/Users/Hello.Mckenna/OneDrive - GMIT/__MRes Thesis/PACKAGE/Results/Lesser spotted dogfish/__RESULTS_CS.CSV")
# 
# tmp.juv <- tmp[tmp$Maturity == 1,]
# (SD.juv <- round(data.frame(sd_phi = sd(tmp.juv$phi), sd_kappa = sd(tmp.juv$kappa), sd_Sigmasq = sd(tmp.juv$Sigmasq), sd_tausq = sd(tmp.juv$tausq)),2))
# 
# 
# tmp.adult <- tmp[tmp$Maturity == 2,]
# (SD.adult <- round(data.frame(sd_phi = sd(tmp.adult$phi), sd_kappa = sd(tmp.adult$kappa), sd_Sigmasq = sd(tmp.adult$Sigmasq), sd_tausq = sd(tmp.adult$tausq)),2))






# ANALYSIS COMPLETE
beep(8)
print(paste("ANALYSIS COMPLETE - See Results folder ", file.path(getwd())))



  }
