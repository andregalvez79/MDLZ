
##### RMISC #####
library(Rmisc)

info <- read.csv("C:\\Users\\OYO9864\\OneDrive - MDLZ\\C_Intervals\\brasil_h.csv", stringsAsFactors = F)
names(info)
str(info)
info$Statistical_Forecast <- as.numeric(info$Statistical_Forecast)
info$Period <- as.factor(info$Period)
info[is.na(info)] <- 0

info2 <- info[which(info$Statistical_Forecast != 0),] #separate only the ones that have value in forecast             
str(info2)

levelsP<- levels(info2$Period) #obtener vector de levels para subset

#saber de donde empezar el forloop
sum1<- xtabs(Statistical_Forecast~Period, info2)
names(which(sum1 !=0))[1]
rm(x)
x <- list()

#for loop for period CI BASED ON HITORIC DATA
for (i in 41:60) {
  temp <- info2[info2$Period == levelsP[i],] #subset data per period
  nam <- paste("Period", levelsP[i], sep = "_") #create a variable name with same pattern
  ci <- CI(temp$Statistical_Forecast, ci = 0.95) #do CI for each level
  x[[nam]] <-  ci
  
}

CI_Period<- as.data.frame(x) #convert to DF
CI_Period<- t(CI_Period) #tranpose






#for loop for period and family USING TONS TO GENERATE AN SD FOR BASIC CI

info2$Trend..tons. <-  as.numeric(info2$Trend..tons.)
info2$Sub.Family <-  as.factor(info2$Sub.Family)
levelsSF<- levels(info2$Sub.Family)
rm(y)
rm(x)
x <- list()
result <- data.frame(matrix(nrow = length(info2$Statistical_Forecast), ncol = 5))
names(result) <- names(y) 

for (i in 41:60) {
  temp <- info2[info2$Period == levelsP[i],] #subset data per period
  
  for (k in 1:length(temp$Statistical_Forecast)) {
  temp2 <- temp[temp$Sub.Family == levelsSF[k],]
  nam <- paste(levelsP[i], levelsSF[k] ,sep = "_") #create a variable name with same pattern
  y <- data.frame(tons = c(as.numeric(temp2$Trend..tons.)), mean = c(as.numeric(temp2$Statistical_Forecast)) )
  y$upper <-  as.numeric(y$mean + 3*(apply(y[, c("tons","mean")],1,sd)))
  y$lower <-  as.numeric(y$mean - 3*(apply(y[, c("tons","mean")],1,sd)))
  y$name <- as.character(nam)
  result[,1:5] <- rbind(y,result[i,])
  
  }
}

CI_Period_SF <- result




#FOR LOOP FOR SUB FAMILY AND PERIOD BASED ON HITORIC DATA SUBFAMILY
rm(x)
x <- list()
for (i in 1:length(info2$Sub.Family)) {
  temp <- info2[info2$Sub.Family == levelsSF[i],] #subset data per sub family
  ci <- CI(temp$Statistical_Forecast, ci = 0.95) #do CI for each level
  for (k in 41:60) {
    temp2 <- temp[temp$Period == levelsP[k],] #subset data per period
    nam <- paste(levelsP[k], levelsSF[i] ,sep = "_") #create a variable name with same pattern
    x[[nam]] <-  ci
  
  }
}

CI_Period_SF <- data.frame(matrix(unlist(x), ncol = max(lengths(x)), byrow = TRUE))
CI_Period_SF$names <- names(x)
names(CI_Period_SF)[names(CI_Period_SF) == "X1"] <- "upper"
names(CI_Period_SF)[names(CI_Period_SF) == "X2"] <- "mean"
names(CI_Period_SF)[names(CI_Period_SF) == "X3"] <- "lower"






#for loop for period AND IBPFAMILY with family CI BASED ON HISTORIC DATA
#ya esta ya que sacamos forecast a ese nivel.
names(info2)
str(info2)
levelsIBP <- levels(as.factor(info2$IBP.Family))
rm(x)
x <- list()
for (i in 1:length(info2$IBP.Family)) {
  temp <- info2[info2$IBP.Family == levelsIBP[i],] #subset data per sub family
  ci <- CI(temp$Statistical_Forecast, ci = 0.95) #do CI for each level
  for (k in 41:60) {
    temp2 <- temp[temp$Period == levelsP[k],] #subset data per period
    nam <- paste(levelsP[k], levelsIBP[i] ,sep = "_") #create a variable name with same pattern
    x[[nam]] <-  ci
    
  }
}
CI_Period_IBP<- as.data.frame(x) #convert to DF
CI_Period_IBP<- t(CI_Period_IBP) #tranpose




#for loop for period AND sub category with  CI BASED ON HISTORIC DATA
names(info2)
str(info2)

levelsSC <- levels(as.factor(info2$IBP.Sub.Category))

rm(x)
x <- list()
for (i in 1:length(info2$IBP.Family)) {
  temp <- info2[info2$IBP.Sub.Category == levelsSC[i],] #subset data per sub family
  for (k in 41:60) {
    temp2 <- temp[temp$Period == levelsP[k],] #subset data per period
    ci <- CI(temp2$Statistical_Forecast, ci = 0.95) #do CI for each level
    nam <- paste(levelsP[k], levelsSC[i] ,sep = "_") #create a variable name with same pattern
    x[[nam]] <-  ci
    
  }
}
CI_Period_SC<- as.data.frame(x) #convert to DF
CI_Period_SC<- t(CI_Period_SC) #tranpose




#for loop for period AND sub category with  CI BASED ON HISTORIC DATA
names(info2)
str(info2)

levelsIBPC<- levels(as.factor(info2$IBP.Category))

rm(x)
x <- list()
for (i in 1:length(info2$IBP.Family)) {
  temp <- info2[info2$IBP.Category == levelsIBPC[i],] #subset data per sub family
  for (k in 41:60) {
    temp2 <- temp[temp$Period == levelsP[k],] #subset data per period
    ci <- CI(temp2$Statistical_Forecast, ci = 0.95) #do CI for each level
    nam <- paste(levelsP[k], levelsIBPC[i] ,sep = "_") #create a variable name with same pattern
    x[[nam]] <-  ci
    
  }
}
CI_Period_IBPC<- as.data.frame(x) #convert to DF
CI_Period_IBPC<- t(CI_Period_IBPC) #tranpose




library(dplyr)
x <- list()
rm(result2)
result2 <- NULL
names(result2) <- names(y) 
info2$IBP.Category <- as.factor(info2$IBP.Category)

for (i in 1:length(levelsIBPC)) {
  temp <- info2[info2$IBP.Category == levelsIBPC[i],] #subset data per sub family
  x <- list()
  for (k in 41:60) {
    temp2 <- temp[temp$Period == levelsP[k],] #subset data per period
    nam <- paste(levelsP[k], levelsIBPC[i] ,sep = "_")
    y <- data.frame(tons = c(as.numeric(temp2$Trend..tons.)), mean = c(as.numeric(temp2$Statistical_Forecast)))
    y$mean <- mean(y$mean)
    y$tons <- mean(y$tons)
    y$upper <-  as.numeric(y$mean + mean(3*(apply(y[, c("tons","mean")],1,sd))))
    y$lower <-  as.numeric(y$mean - mean(3*(apply(y[, c("tons","mean")],1,sd))))
    y$name <- as.character(nam)
    y <- y[!duplicated(y$mean),]
    result2 <- rbind(result2, c(y$mean,y$tons,y$upper,y$lower, y$name))

    
  }
    

  
}
result2
result2 <- as.data.frame(result2)
library(reshape2)
long <- melt(CI_Period_IBPC2, id.vars = c("tons", "mean", "upper", "lower"))
CI_Period_IBPC2<- as.data.frame(x) #convert to DF
#CI_Period_IBPC2<- t(CI_Period_IBPC2)
 
