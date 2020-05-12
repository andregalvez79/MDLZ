################              ################
################ CORRELATIONS ################
################              ################

#########################################################################################

##if all ok significative, then just aggregate otherwise run function for problem family

install.packages("Hmisc")
library(Hmisc)

install.packages("PerformanceAnalytics")
library(PerformanceAnalytics)

#corr_mat.csv is a file made with all means and CI's of all leveles of forecasts.
crmat<- read.csv("C:\\Users\\OYO9864\\OneDrive - MDLZ\\C_Intervals\\corr_mat.csv", stringsAsFactors = F)
str(crmat)
cor1<- rcorr(as.matrix(crmat))
m <- as.data.frame(cor1$P)

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}


flattenCorrMatrix(cor1$r, cor1$P)


chart.Correlation(cor1$r, histogram=TRUE, pch=19)

col<- colorRampPalette(c("blue", "white", "red"))(20)
heatmap(x = cor1$r, col = col, symm = TRUE)


corrplot(cor1$r, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)



#######_____________________Funcion correlation______________________########

options(scipen = 1000)

info <- read.csv("C:\\Users\\OYO9864\\OneDrive - MDLZ\\C_Intervals\\brasil_h.csv", stringsAsFactors = F)


checkcorrs <- function(){
info6_1 = list()
info6_2 = list()
info6_3 = list()
info6_4 = list()

for (i in 1:length(levels(as.factor(info$IBP.Category)))){

  info2 <- info[info$Period >= 201905,]
  info3 <- info2[info2$Statistical_Forecast > 0,]
  info4 <- info3[info3$IBP.Category == levels(as.factor(info3$IBP.Category))[i],]
  #info4 <- droplevels(info4)
  #print(info4$IBP.Family[1])
  
 
  
  
  info4 <- droplevels(info4)
  
#para subfamilia
  
  if (i == 1) {
    for (j in 1: length(levels(as.factor(info4$IBP.Family)))) {
      
      info5 <- info4[info4$IBP.Family == levels(as.factor(info4$IBP.Family))[j],]
      info6_1[[j]] <- info5$Statistical_Forecast
      names(info6_1)[j] <- levels(as.factor(info4$IBP.Family))[j]
      max_length <- max(unlist(lapply (info6_1, FUN = length)))

    }
    info6_1 <- sapply (info6_1, function (x) {length (x) <- max_length; return (x)})
    correl_1 <- rcorr(info6_1)
    values_1 <- flattenCorrMatrix(correl_1$r, correl_1$P)
    
    if (values_1$p[j] >.05) {
      print(paste("Danger with correlation in:", values_1$row, "with",values_1$column))
    }else{print(paste0("Correlations are OK for: ALL"))}
    
  }else{if (i == 2) {
    for (j in 1: length(levels(as.factor(info4$IBP.Family)))) {

      info5 <- info4[info4$IBP.Family == levels(as.factor(info4$IBP.Family))[j],]
      info6_2[[j]] <- info5$Statistical_Forecast
      names(info6_2)[j] <- levels(as.factor(info4$IBP.Family))[j]
      
    }
    
    info6_2 <- sapply (info6_2, function (x) {length (x) <- max_length; return (x)})
    correl_2 <- rcorr(info6_2)
    values_2 <- flattenCorrMatrix(correl_2$r, correl_2$P)
    
    if (values_2$p[j] >.05) {
      print(paste("Danger with correlation in:", values_2$row, "with",values_2$column))
    }else{print(paste0("Correlations are OK for: ALL"))}
    
  }else{if (i == 3) {
    for (j in 1: length(levels(as.factor(info4$IBP.Family)))) {

      info5 <- info4[info4$IBP.Family == levels(as.factor(info4$IBP.Family))[j],]
      info6_3[[j]] <- info5$Statistical_Forecast
      names(info6_3)[j] <- levels(as.factor(info4$IBP.Family))[j]

    }
    
    info6_3 <- sapply (info6_3, function (x) {length (x) <- max_length; return (x)})
    correl_3 <- rcorr(info6_3)
    values_3 <- flattenCorrMatrix(correl_3$r, correl_3$P)
    
    if (values_3$p[j] >.05) {
      print(paste("Danger with correlation in:", values_3$row, "with",values_3$column))
    }else{print(paste0("Correlations are OK for: ALL"))}
    
    
  }else{
    for (j in 1: length(levels(as.factor(info4$IBP.Family)))) {
      info5 <- info4[info4$IBP.Family == levels(as.factor(info4$IBP.Family))[j],]
      info6_4[[j]] <- info5$Statistical_Forecast
      names(info6_4)[j] <- levels(as.factor(info4$IBP.Family))[j]

    }
    
    info6_4 <- sapply (info6_4, function (x) {length (x) <- max_length; return (x)})
    correl_4 <- rcorr(info6_4)
    values_4 <- flattenCorrMatrix(correl_4$r, correl_4$P)
    
    if (values_4$p[j] >.05) {
      print(paste("Danger with correlation in:", values_4$row, "with",values_4$column))
    }else{print(paste0("Correlations are OK for: ALL"))}
    
    
  }}}
}  
return(rbind(values_1,values_2,values_3,values_4))

}

checkcorrs() 

#igual agregar que te regrese una familia para que ese output se valla directo a la siguiente función




##### +++++++ Can't aggregate and/or have an idea of problem family? +++++++ #####
#########################################################################################

################                  ################
################ AVERAGE FUNCTION ################
################                  ################

#########################################################################################

# RUN from here

#read data sets:

#info: data with all categories sell in and forecasts
#fcast: data with forecast results at ibp family level
#err: data with forecast errors at ibp family level

fcast <- read.csv("C:\\Users\\OYO9864\\OneDrive - MDLZ\\C_Intervals\\IBPFamily.csv", stringsAsFactors = F)
err <- read.csv("C:\\Users\\OYO9864\\OneDrive - MDLZ\\C_Intervals\\IBPFamily_err.csv", stringsAsFactors = F)

##### check correlations #####

#make sure formatting is correct before function.
info$IBP.Family <- as.factor(info$IBP.Family)
info$Statistical_Forecast <- as.numeric(info$Statistical_Forecast)
info$Statistical_Forecast[is.na(info$Statistical_Forecast)] <- 0
levelsFam <- levels(info$IBP.Family)
levelsFam2 <- levels(as.factor(err$ï..family))


##### Main function #####
### n: es el número del nivel del factor de ibp family
### sup: intervalo superior de danger zone
### infe: limite inferior de danger zone
#ejemplo meanfcast(27,15,5)
# 27 representa el nivel 27 de levelFam, o sea Other desserts.
# y el danger zone es que por cada sub familia que este entre el 5 y el 15% de varianza se tomará como peligroso.
# por lo tanto la función arroja un promedio de forecast mean y CI's de los 3 mejores modelos.


meanfcast <-  function(n, sup, infe){


#temporal lists and variables needed
res <- list()
res2  <- list()
dang = NULL
ok = NULL
err2 <- 0
err3 <- 0
mins <- list()
mins2 <- list()
nam2_2 <- list()
nam3_2 <- list()
nam <-  list()
nam2 <-  list()
nam3 <-  list()
finList <- list()
mean_x <- list()
lower_x <- list()
upper_x <- list()
period_x <- list()
final_result <- list()


#first filtering of problem ibp family, and tab for proportions
info2 <- info[info$IBP.Family == levelsFam[n],]
tab1 <- aggregate(info2$Statistical_Forecast, by=list(Family=info2$Sub.Family), FUN=sum)



#generate proportions of share in variance of total var of forecast 
for (i in 1:length(tab1$x)) {
  res[i] = (tab1$x[i]/sum(tab1$x))*100
  res2[i] = tab1$Family[i]
  
  if (res[i]<= sup & res[i]>= infe) { #aqui está el rango de danger en este caso por prueba es de 15% al 1%
    dang[[i]] <- append(res[i], res2[i])
  }else{ok[[i]] <-  append(res[i], res2[i])
  print(paste0("Proportions are OK for: ", ok[[i]]))
  }
  
  if (is.null(dang) == FALSE) {
    print(paste0("Dangerous proportions are/is: ", dang[i]))
    var_fam<- gsub(" ", ".", info2$IBP.Family)
    
    #Da como output una lista de los mejores modelos por family.
    
    for (i in 1:length(levelsFam2)) {
      err2 <- err[err$ï..family == levelsFam2[i],] #para cada familia un subset
      nam_2 <- list()
      nam2_2 <- list()
      nam3_2 <- list()
      
      
      for (j in 1:length(levels(as.factor(err2$measure)))) { #para cada subset de familia y para cada modelo 
        #dame la variable de interés
        err3 <- err2[err2$measure == levels(as.factor(err2$measure))[j],] 
        mins2[j] <- err3$error[which.min(abs(err3$error))]
        nam_2[j] <- err3$models[which(mins2[j] == err3$error)]
        nam2_2[j] <- err3$measure[which(mins2[j] == err3$error)]
        nam3_2[j] <- err3$ï..family[which(mins2[j] == err3$error)]
      }
      #save it
      mins2 <- as.vector(unlist(mins2))
      nam_2 <- as.vector(unlist(nam_2))
      nam2_2 <- as.vector(unlist(nam2_2))
      nam3_2 <- as.vector(unlist(nam3_2))
      mins[[i]] <- mins2
      nam[[i]] <- nam_2
      nam2[[i]] <- nam2_2
      nam3[[i]] <- nam3_2
      finList[[i]] <- c(mins[i],nam[i], nam2[i], nam3[i])
      
    }
    
    #unlist list of lists
    flattenlist <- function(x){
      morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
      out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
      if(sum(morelists)){
        Recall(out)
      }else{
        return(out)
      }
    }
    
    outlist <- flattenlist(finList)
    
    numb <- grep(var_fam[1], outlist) #know which row is it
    templist<- outlist[(numb-3):numb] #choose the info
    
    x <- as.data.frame(matrix(nrow = 6 , ncol = 4))
    x[,1] <- as.data.frame(unlist(templist[1]))
    x[,2] <- as.data.frame(unlist(templist[2]))
    x[,3] <- as.data.frame(unlist(templist[3]))
    x[,4] <- as.data.frame(unlist(templist[4]))
    #generate df with best measures.
    x <- x[order(abs(x$V1)),] 
    print(paste0("Top models are: ", levels(x$V2)))
    
    #generate df for each model 
    if (length(levels(x$V2)) == 1) {
      fcast2_1 <- fcast[fcast$family == levels(as.factor(x$V4))[1],]
      fcast2_1 <- fcast2_1[fcast2_1$model == levels(as.factor(x$V2))[1],]
    }else{
      
    if (length(levels(x$V2))== 2) {
      fcast2_1 <- fcast[fcast$family == levels(as.factor(x$V4))[1],]
      fcast2_1 <- fcast2_1[fcast2_1$model == levels(as.factor(x$V2))[1],]
      fcast2_2 <- fcast[fcast$family == levels(as.factor(x$V4))[1],]
      fcast2_2 <- fcast2_2[fcast2_2$model == levels(as.factor(x$V2))[2],]
      
      }else{
        fcast2_1 <- fcast[fcast$family == levels(as.factor(x$V4))[1],]
        fcast2_1 <- fcast2_1[fcast2_1$model == levels(as.factor(x$V2))[1],]
        fcast2_2 <- fcast[fcast$family == levels(as.factor(x$V4))[1],]
        fcast2_2 <- fcast2_2[fcast2_2$model == levels(as.factor(x$V2))[2],]
        fcast2_3 <- fcast[fcast$family == levels(as.factor(x$V4))[1],]
        fcast2_3 <- fcast2_3[fcast2_3$model == levels(as.factor(x$V2))[3],]
      }}
        
      
      #generate mean of top 3 models
      for (g in 1:length(fcast2_1$mean)) {
        if (exists("fcast2_3") == T & exists("fcast2_2")==T & exists("fcast2_1")==T) {
          mean_x[g] <- mean(fcast2_1$mean[g],fcast2_2$mean[g],fcast2_3$mean[g])
          lower_x[g] <- mean(fcast2_1$lower[g],fcast2_2$lower[g],fcast2_3$lower[g])
          upper_x[g] <- mean(fcast2_1$upper[g],fcast2_2$upper[g],fcast2_3$upper[g])
          period_x[g] <- fcast2_1$ï..period[g]
        }else{

        if (exists("fcast2_3")==F & exists("fcast2_2")==T & exists("fcast2_1")==T) {
          mean_x[g] <- mean(fcast2_1$mean[g],fcast2_2$mean[g])
          lower_x[g] <- mean(fcast2_1$lower[g],fcast2_2$lower[g])
          upper_x[g] <- mean(fcast2_1$upper[g],fcast2_2$upper[g])
          period_x[g] <- fcast2_1$ï..period[g]
          
          }else{
            if (exists("fcast2_3")==F & exists("fcast2_2")==F & exists("fcast2_1")==T) {
            mean_x[g] <- fcast2_1$mean[g]
            lower_x[g] <- fcast2_1$lower[g]
            upper_x[g] <- fcast2_1$upper[g]
            period_x[g] <- fcast2_1$ï..period[g]}}
        }
        
        final_result[[g]] <-  c(mean_x[g], lower_x[g], upper_x[g], period_x[g])}
    }else{print(paste0("Proportions are OK for: ", ok[[i]]))
  }
}
#format output list into df
dd  <-  as.data.frame(matrix(unlist(final_result), nrow=length(unlist(final_result[1]))))
dd <- t(dd)
colnames(dd) <- c("mean","lower", "upper", "period")
dd <- as.data.frame(dd)
dd[,1:3] <- sapply(dd[,1:3], as.character)
dd[,1:3] <- sapply(dd[,1:3], as.numeric)
dd$family <- levelsFam[n]
return(dd)
}


##### Main function #####
### n: es el número del nivel del factor de ibp family
### sup: intervalo superior de danger zone
### infe: limite inferior de danger zone
#ejemplo: meanfcast(27,15,5)
# 27 representa el nivel 27 de levelFam, o sea Other Desserts.
# y el danger zone es: por cada sub familia que este entre el 5 y el 15% de varianza se tomará como peligroso.
# por lo tanto la función arroja un promedio de forecast mean y CI's de los 3 mejores modelos de la familia.


#### ++++ **** CHOOSE NUMBER OF FAMILY AND THRESHOLD **** ++++ ####
levelsFam

MyResult <- meanfcast(34, 15, 5)


#SAVE
write.csv(MyResult, "C:\\Users\\OYO9864\\Desktop\\Trident.csv")

levels(as.factor(info$Sub.Family))




