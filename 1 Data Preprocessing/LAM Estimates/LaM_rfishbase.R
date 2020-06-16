#####
#
# This script retreves records from rfishbase for speices in the list below#
#
####


library(rfishbase)

data <- read.csv("C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/__PAPER 1/Length at maturity estimates/LaM_Estimates.csv")

data$LengthAtMaturity <- NULL
data$LengthAtMaturityMale <- NA
data$LengthAtMaturityFemale <- NA
data$Lm <- NA


for (i in 1:nrow(data)){
try({  
a <- popgrowth(data[i,]$Species)$LmMale
b <- popgrowth(data[i,]$Species)$LmFemale
c <- popgrowth(data[i,]$Species)$Lm


a <- a[complete.cases(a)]
b <- b[complete.cases(b)]
c <- c[complete.cases(c)]

data$LengthAtMaturityMale[i] <- mean(a)
data$LengthAtMaturityFemale[i] <- mean(b)
data$Lm[i] <- mean(c)
data$mu[i] <- mean(c(a,b,c))


data$Source[i] <- "rfishbase"
print(paste(i ,"of", nrow(data)))

})
  
}


data[,c(2,3,8,9,10,11)]

data <- data[complete.cases( data$mu),]

write.csv(data, "C:/Users/Hello.Mckenna/OneDrive - GMIT/DATA/__PAPER 1/Length at maturity estimates/LaM_Estimates.csv")



