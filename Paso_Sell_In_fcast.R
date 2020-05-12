library(tidyverse)
library(tidyr)
library(AnomalyDetection)
library(forecast)
library(dplyr)
library(strucchange)
library(magrittr)
library(readxl)
library(scales)
library(reshape2)
library(devtools)
library(anomalize)
library(tibble)
library(tibbletime)
library(Metrics)
library(psych)
library(lubridate)
library(forecast)
library(forecastHybrid)
library(readr)
library(stringr)
library(purrr)
library(reshape2)
library(readxl)
library(readr)
library(stringr)
library(lubridate)
library(odbc)
library(data.table)
library(doBy)
library(lattice)
library(ggplot2)
library(mefa)
library(stringi)
library(detect)

######################################### INPUTS ##########################################
#Forecast period start                                                                    #
fechafcast =                                                                        #
#start of series (year)                                                                   #
inicio =                                                                              #
#end of series (year)                                                                     #
final =                                                                               #
#Sales organization                                                                       #
#NUMBER OF SALE ORGS                                                                      #
S_org_num =                                                                              #
S_org = c("", "")                                                                 #
#intial date of BWP or first date after the end of legacy data                            #
#####IMPORTANT: if Legacy data is NOT needed ADD NULL as the date: BWP_first = NULL       #
#else fromat: "YYYYMM" 
#VER QUE LAST BWP ESTE COMPLETO LOS MESES
#WACAM = - chile(201611) = 201501 BWP
#ARG, CHILE; URU = 201608 BWP
#brasil, MEX legacy NA
BWP_first =                                                                     #
#canales: if no channels <- NULL, else: c("MT", "TT")                                     #
canales <- c("", "")
#date of six months in the future from today for lifecycle: YYYYMM
six_months = 
#date of three months in the past from today for lifecycle: YYYYMM
three_months =                                                                     ##
###########################################################################################

#################### LOAD DATA ####################
#Please check format
options(scipen = 999999)
setwd("")
listcube<- read.csv("Input\\listcube.csv", stringsAsFactors = F)
hier <- read.csv("Input\\hier.csv", stringsAsFactors = F) #last hierarchy used for last forecast
ibp_la <- read.csv("Input\\ibp_la.csv", stringsAsFactors = F) #for formatflavor names
if (is.null(BWP_first)==F) {
  legacy<- read.csv("Input\\legacy.csv", stringsAsFactors = F)
}


colnames(listcube)<-c("Base_Code", "Customer_Level1","SKU","IBP_Family",
                      "Format_Flavor", "Category", "SubCategory","Period","Cat_fcast",
                      "SellIn", "Unit")

listcube=subset(listcube, substr(SKU,1,5) != "DUMMY")
listcube=subset(listcube, substr(Base_Code,1,5) != "DUMMY")
listcube$SKU <- as.numeric(listcube$SKU)
listcube$Period <- as.numeric(listcube$Period)
listcube$SellIn <- as.numeric(listcube$SellIn)

for (i in 1:length(listcube$Unit)) {
  
  if (listcube$Unit[i] == "G") {
    listcube$SellIn[i] <-listcube$SellIn[i]*1000
    listcube$Unit[i] <- "KG"
  }else if (listcube$Unit[i] == "GR") {
    listcube$SellIn[i] <-listcube$SellIn[i]*1000
    listcube$Unit[i] <- "KG"
  }else if (listcube$Unit[i] == "LB") {
    listcube$SellIn[i] <-listcube$SellIn[i]/2.20462442
    listcube$Unit[i] <- "KG"
  }else if (listcube$Unit[i] == "OZ") {
    listcube$SellIn[i] <-listcube$SellIn[i]/35.27396194
    listcube$Unit[i] <- "KG"
  }
}


if (levels(as.factor(listcube$Unit)) != "KG") {
  print("Please check Weights and add conversion factor in for loop above")
  
}



colnames(hier)<-c("SKU", "Detalle", "Base_Code","Format_Flavor", "Format_Flavor_Name","IBP_Family",
                  "IBP_Family_Name","IBP_Sub_Category","IBP_Sub_Category_Name","IBP_Category", "IBP_Category_Name",
                  "Peso_Neto_por_unidad_gr", "Rendimiento","LifeCycle_StatusActual",
                  "LifeCycle_StatusOld")

if (is.null(BWP_first)==F) {
  if (is.null(canales)==F) {
    colnames(legacy)<-c("SKU","Detail","CUSTOMER_LEVEL_1","PERIOD","SELLIN_KG","CANAL",           
                        "SELLIN_TONS")
  }else{legacy <- legacy[,c(1,2,4,5,7)]
  colnames(legacy)<-c("SKU","Detail","PERIOD","SELLIN_KG",           
                      "SELLIN_TONS")}}

if (S_org_num==1) {
  ibp_la <-  ibp_la[which(ibp_la$X.1 == S_org),]
  ibp_la <- ibp_la %>% distinct()
}else if(S_org_num==2) {
  ibp_la<-  ibp_la[which(ibp_la$X.1 == S_org[1] | ibp_la$X.1 == S_org[2]),]
  ibp_la <- ibp_la %>% distinct()
}else if(S_org_num==3){ibp_la<-  ibp_la[which(ibp_la$X.1 == S_org[1] | ibp_la$X.1 == S_org[2] | ibp_la$X.1 == S_org[3]),]
ibp_la <- ibp_la %>% distinct()}

names(ibp_la)[names(ibp_la) == "�.."] <- "SKU"
names(ibp_la)[names(ibp_la) == "X"] <- "SKU"
if (is.null(BWP_first)==T) {
  tryCatch(
    for (i in 1:50) {
      if (substr(listcube$SKU[i],8,14)== "0000000") {
        print("CHECK SKU FORMAT IN LISTCUBE")
      }  
      else if(substr(hier$SKU[i],8,14)== "0000000") {
        print("CHECK SKU FORMAT IN HIERARCHY")
      }
      else if(substr(ibp_la$SKU[i+1],8,14)== "0000000") {
        print("CHECK SKU FORMAT IN IBP_LA")
      }else{print("SKU's FORMAT OK")}
    },
    error = print("SKU's FORMAT OK"))
}else{tryCatch(
  for (i in 1:50) {
    if (substr(listcube$SKU[i],8,14)== "0000000") {
      print("CHECK SKU FORMAT IN LISTCUBE")
    }  
    else if(substr(hier$SKU[i],8,14)== "0000000") {
      print("CHECK SKU FORMAT IN HIERARCHY")
    }
    else if(substr(ibp_la$SKU[i+1],8,14)== "0000000") {
      print("CHECK SKU FORMAT IN IBP_LA")
    }
    else if(substr(legacy$SKU[i],8,14)== "0000000") {
      print("CHECK SKU FORMAT IN LEGACY")
    }else{print("SKU's FORMAT OK")}
  },
  error = print("SKU's FORMAT OK"))}


#LOAD BWP
con <- dbConnect(odbc(),
                 dsn = "",
                 uid = "",
                 pwd = "")

sql_query <- read_file("Input\\Query.txt")
if (S_org_num>1) {
  sql_query <- gsub(pattern = "SalesOrg_replace", replace = S_org[1] ,sql_query)
  sql_query <- gsub(pattern = "Other_SalesOrg", replace = S_org[2] ,sql_query)
  }else{sql_query <- gsub(pattern = "SalesOrg_replace", replace = S_org ,sql_query)}

sql_query <- gsub(pattern = "HotPeriod_Superior_<=", replace = as.character(fechafcast-1),sql_query)

if (is.null(BWP_first)) {
  sql_query <- gsub(pattern = "ColdPeriod_Inferior_>=", replace = "201701",sql_query)
  
}else{sql_query <- gsub(pattern = "ColdPeriod_Inferior_>=", replace = as.character(BWP_first),sql_query)

}


BWP<- odbc::dbGetQuery(con,sql_query)
#PARA MEXICO
#BWP <- BWP %>% filter(is.na(BASECODE)==F)
#BWP <- BWP %>% filter(PERIOD != "000000")

write.csv(BWP,"Output\\bwp.csv")
names(BWP)[names(BWP) == "MATERIAL"] <- "SKU"
BWP$SKU <- gsub("(^|[^0-9])0+", "\\1", BWP$SKU, perl = TRUE)

#####add channels as MT and TT and formating
if (is.null(canales)==F) {
  
  if (S_org_num==1) {
    
  for (i in 1:length(BWP$BASECODE)) {
    if(BWP$CUSTOMER_LEVEL_1[i] == paste0(substr(S_org,1,2),"0001")){BWP$canal[i] <- canales[1]
    }else{BWP$canal[i] <- canales[2]}
  }
  }else{
    for (i in 1:length(BWP$BASECODE)) {
      if(BWP$CUSTOMER_LEVEL_1[i] == paste0(substr(S_org[1],1,2),"0001")){BWP$canal[i] <- canales[1]
      }else{BWP$canal[i] <- canales[2]}}
  }
  }

listcube$SellIn <- as.numeric(listcube$SellIn)
listcube$Cat_fcast <- as.numeric(listcube$Cat_fcast)
listcube$SellIn[is.na(listcube$SellIn)] <- 0
listcube$Cat_fcast[is.na(listcube$Cat_fcast)] <- 0

if (is.null(canales)==T) {
  listcube<- aggregate(.~ SKU + Base_Code +IBP_Family +Format_Flavor + 
                         Category + SubCategory + Period + Unit, listcube[,-c(2)], sum)
}



if (is.null(canales)==F) {
  if (S_org_num==1) {
    
  for (i in 1:length(listcube$SKU)) {
    if(listcube$Customer_Level1[i] == paste0(substr(S_org,1,2),"0001")){listcube$canal[i] <- canales[1]
    }else{listcube$canal[i] <- canales[2]}
  }
  BWPagg2 <- summaryBy(SELLIN_KG ~ SKU  + WEIGHT +BU_Desc +CAT_Desc +FAM_Desc+
                         SUBFAM_Desc+BASECODE+ MAT_Desc + PERIOD +canal ,data =BWP ,FUN=sum)
}
  else{
  for (i in 1:length(listcube$SKU)) {
  if(listcube$Customer_Level1[i] == paste0(substr(S_org[1],1,2),"0001")){listcube$canal[i] <- canales[1]
  }else{listcube$canal[i] <- canales[2]}
}
  BWPagg2 <- summaryBy(SELLIN_KG ~ SKU  + WEIGHT +BU_Desc +CAT_Desc +FAM_Desc+
                         SUBFAM_Desc+BASECODE+ MAT_Desc + PERIOD +canal ,data =BWP ,FUN=sum)}
  
  }else{BWPagg2 <- summaryBy(SELLIN_KG ~ SKU  + WEIGHT +BU_Desc +CAT_Desc +FAM_Desc+
                             SUBFAM_Desc+BASECODE+ MAT_Desc + PERIOD ,data =BWP ,FUN=sum)}



listcube=subset(listcube, substr(SKU,1,5) != "DUMMY")
listcube=subset(listcube, substr(Base_Code,1,5) != "DUMMY")
listcube$SKU <- as.character(listcube$SKU)
listcube$Period <- as.character(listcube$Period)

if (is.null(canales)==F) {
  listcubeagg <- summaryBy(SellIn ~ SKU + Base_Code +IBP_Family +Format_Flavor + 
                             Category + SubCategory + Period + canal ,data =listcube ,FUN=sum)
  checkBWPAPO<- merge(BWPagg2[,c("SKU","BASECODE","PERIOD","SELLIN_KG.sum", "canal")],listcubeagg[,c("SKU","Base_Code","Period","SellIn.sum","canal")], by.x = c("SKU", "BASECODE", "PERIOD", "canal"),
                      by.y = c("SKU", "Base_Code", "Period", "canal"))
  names(checkBWPAPO)[names(checkBWPAPO) == "SellIn.sum"] <- "SellIn"
}else if (is.null(canales)==T) {
  listcubeagg <- summaryBy(SellIn ~ SKU + Base_Code +IBP_Family +Format_Flavor + 
                             Category + SubCategory + Period ,data =listcube ,FUN=sum)
  checkBWPAPO<- merge(BWPagg2[,c("SKU","BASECODE","PERIOD","SELLIN_KG.sum")],listcubeagg[,c("SKU","Base_Code","Period","SellIn.sum")], by.x = c("SKU", "BASECODE", "PERIOD"),
                      by.y = c("SKU", "Base_Code", "Period"))
  names(checkBWPAPO)[names(checkBWPAPO) == "SellIn.sum"] <- "SellIn"
}


fn1<- function(date){
  ao1 <- subset(checkBWPAPO, PERIOD <= date & PERIOD >= substr(date,1,4))
  ao1$difs_total <- ao1$SELLIN_KG.sum - ao1$SellIn
  difs<- sum(ao1$difs_total/1000)
  perc<- difs/sum(ao1$SELLIN_KG.sum/1000)*100
  return(print(paste0("Listcube difference until period:", date, " corresponds to: ", round(difs, 2), " tons, which represent:", round(perc,2),"% vs.BWP")))
}
###################CHECKS BWP LISTCUBE#################

ultimotres<- list(substr(fechafcast-100,1,4), substr(fechafcast-200,1,4),substr(fechafcast-300,1,4))
for (i in 1:length(ultimotres)) {
  ao1 <- subset(checkBWPAPO, substr(PERIOD,1,4) == ultimotres[[i]])
  ao1$difs_total <- ao1$SELLIN_KG.sum - ao1$SellIn
  difs<- sum(ao1$difs_total/1000)
  perc<- difs/sum(ao1$SELLIN_KG.sum/1000)*100
  print(paste0("Listcube difference in year:", ultimotres[[i]], " corresponds to: ", round(difs, 2), " tons, which represent:", round(perc,2),"% vs.BWP"))
  if (i-length(ultimotres) == 0) {
    fn1(fechafcast)
  }
}

############################################################
#check basecodes old hierarchy vs. listcube
listcube_temp<- summaryBy(SellIn ~ SKU + Base_Code +IBP_Family +Format_Flavor + 
                            Category + SubCategory  + canal ,data =listcube ,FUN=sum)
checkBC = merge(listcube_temp[, c("SKU", "Base_Code")], 
                hier[, c("SKU", "Base_Code")], all = T)
duplicates<- checkBC[duplicated(checkBC$SKU),]
duplicates<- na.omit(duplicates)
BClist<- merge(listcube_temp[,c(1,2)], duplicates[1], all.y =T)
BClist <- na.omit(BClist)
colnames(BClist) <- c("SKU","BC_List_Cube")
BChier<- merge(hier[,c(1,3)], duplicates[1], all.y =T)
colnames(BChier) <- c("SKU","BC_Hierarchy")
BCdifs<- merge(BClist, BChier, by = "SKU")
BCdifs2<- BCdifs[!duplicated(BCdifs$SKU),]
BCdifs3 = NULL
BCdifs4 = NULL
BCdifs5 = NULL
for (i in 1:length(BCdifs2$SKU)) {
  if (as.character(BCdifs2$BC_List_Cube)[i] != as.character(BCdifs2$BC_Hierarchy)[i] | 
      is.na(BCdifs2$BC_Hierarchy[i]==T)){
    BCdifs3[i] <- BCdifs2$SKU[i]
    BCdifs4[i] <- BCdifs2$BC_List_Cube[i]
    BCdifs5[i] <- BCdifs2$BC_Hierarchy[i]
  }
  
}

BCdifs3 <-  as.data.frame(BCdifs3)
names(BCdifs3)[names(BCdifs3) == "BCdifs3"] <- "SKU"
BCdifs3<- BCdifs3[!duplicated(BCdifs3$SKU),]
BCdifs3 <-  as.data.frame(BCdifs3)
BCdifs3 <- na.omit(BCdifs3) 
names(BCdifs3)[names(BCdifs3) == "BCdifs3"] <- "SKU"

BCdifs4<- merge(BCdifs3, BCdifs2, by = "SKU", all.x = T)

if (length(BCdifs4$SKU)!=0) {
  print("CHECK DIFFERENCES IN BASECODES APO VS. HIERARCHY")
  print(BCdifs4)
  write.csv(BCdifs4,"Output\\APOvsHier_BC_difs.csv")
}else{print("NO BASECODE DIFFERENCES BETWEEN APO AND HIERARCHY")}


###################CHECKS BWP HIERARCHY#################
length(hier$SKU)

BWPagg <- summaryBy(SELLIN_KG ~ SKU  + WEIGHT, data = BWP ,FUN=sum)
BWPagg <- unique(BWPagg[,-3])


difs_peso = merge(hier[,c(1,12)], BWPagg, by = "SKU", all.x = T)
difs_peso$WEIGHT <- as.numeric(difs_peso$WEIGHT)
difs_peso$Peso_Neto_por_unidad_gr <-  as.numeric(difs_peso$Peso_Neto_por_unidad_gr)
difs_peso$difs <- 0
for (i in 1:length(difs_peso$SKU)) {
  if (is.na(difs_peso$Peso_Neto_por_unidad_gr[i])==T) {
    difs_peso$Peso_Neto_por_unidad_gr[i] = "Weight not found in Hierarchy"
  }
  if (is.na(difs_peso$WEIGHT[i])==T){
    difs_peso$WEIGHT[i] = "Weight not found in BWP"
  }
  if (difs_peso$Peso_Neto_por_unidad_gr[i] != "Weight not found in Hierarchy" &
      difs_peso$WEIGHT[i] != "Weight not found in BWP") {
    difs_peso$difs[i] = as.numeric(difs_peso$Peso_Neto_por_unidad_gr[i]) - as.numeric(difs_peso$WEIGHT[i])
  }
  
}

difs_peso_2 <-  difs_peso[which(difs_peso$Peso_Neto_por_unidad_gr == "Weight not found in Hierarchy" |
                                  difs_peso$WEIGHT == "Weight not found in BWP" | 
                                  abs(difs_peso$difs) > 1) ,]
print("Differences in weight between hierarchy and BWP are: ")
difs_peso_2

write.csv(difs_peso_2,"Output\\BWPvsHier_weight_difs.csv")
#################merge listcube BWP################
listcube$SKU <- as.character(listcube$SKU)

listcubeagg <- summaryBy(SellIn ~ SKU + IBP_Family + Format_Flavor + Category +
                           SubCategory + Base_Code, data = listcube ,FUN=sum)

listcubeagg <- listcubeagg %>% distinct()

BWPagg <- summaryBy(SELLIN_KG ~ SKU  + WEIGHT + BU_Desc + CAT_Desc + FAM_Desc +
                      SUBFAM_Desc + BASECODE + MAT_Desc, data = BWP ,FUN=sum)

BWPagg <- BWPagg %>% distinct()



cruce1<- merge(BWPagg, listcubeagg, by = "SKU" ,all = T)
cruce1 <- cruce1 %>% distinct()
hier$SKU <- as.character(hier$SKU)
cruce2<- merge(hier[1:15], cruce1, by = "SKU" ,all = T)
names(cruce2)


#fill in data
for (i in 1:length(cruce2$SKU)) {
  if (is.na(cruce2$Detalle[i])==TRUE) {
    cruce2$Base_Code.x[i] <-  cruce2$Base_Code.y[i]
    cruce2$Format_Flavor.x[i] <- cruce2$Format_Flavor.y[i]
    cruce2$IBP_Family.x[i] <- cruce2$IBP_Family.y[i]
    cruce2$IBP_Category_Name[i] <- cruce2$BU_Desc[i]
    cruce2$IBP_Sub_Category_Name[i] <- cruce2$CAT_Desc[i]
    cruce2$IBP_Family_Name[i] <- cruce2$FAM_Desc[i]
    cruce2$Detalle[i] <- cruce2$MAT_Desc[i]
    cruce2$Peso_Neto_por_unidad_gr[i] <- cruce2$WEIGHT[i]
  }
}
for (i in 1:length(cruce2$SKU)) {
  if (is.na(cruce2$IBP_Category[i])==TRUE) {
    cruce2$IBP_Category[i] <- cruce2$Category[i]
    cruce2$IBP_Sub_Category[i] <- cruce2$SubCategory[i]
  }
}

for (i in 1:length(cruce2$SKU)) {
  if (is.na(cruce2$SELLIN_KG.sum[i])==TRUE) {
    cruce2$SELLIN_KG.sum[i] <- cruce2$SellIn.sum[i]
  }
}

for (i in 1:length(cruce2$SKU)) {
  if (is.na(cruce2$SellIn.sum[i])==TRUE) {
    cruce2$SellIn.sum[i] <- cruce2$SELLIN_KG.sum[i]
  }
}

for (i in 1:length(cruce2$SKU)) {
  if (is.na(cruce2$Peso_Neto_por_unidad_gr[i])==TRUE) {
    cruce2$Peso_Neto_por_unidad_gr[i] <- cruce2$WEIGHT[i]
  }
}
cruce2$SELLIN_KG.sum[is.na(cruce2$SELLIN_KG.sum)] <- 0
cruce2$SellIn.sum[is.na(cruce2$SellIn.sum)] <- 0

newdata <- cruce2[1:15]
newsku <-  newdata[which(is.na(newdata$Format_Flavor_Name) == T),]
oldsku <-  newdata[which(is.na(newdata$Format_Flavor_Name) == F),]


#PASTE NEW SKU INFO
doble<- merge(newsku, ibp_la, by = "SKU", all.x = T)

for (i in 1:length(doble$SKU)) {
  doble$Detalle <- doble$X2
  doble$IBP_Family_Name <- doble$X12
  doble$IBP_Sub_Category <- doble$X11
  doble$IBP_Sub_Category_Name <- doble$X14
  doble$IBP_Category_Name <- doble$X16
  doble$IBP_Category.x <- doble$X15
  doble$Format_Flavor_Name <- doble$X4
  doble$Base_Code.x <- doble$X.3
  
}

newsku <- doble[1:15]
newsku <- newsku %>% distinct()
names(newsku)[names(newsku) == "Base_Code.x"] <- "Base_Code"
names(oldsku)[names(oldsku) == "Base_Code.x"] <- "Base_Code"
newsku=subset(newsku, substr(Base_Code,1,5) != "DUMMY")
#be sure names have no extra sapaces
allsku <- rbind(newsku, oldsku)
names(allsku)[names(allsku) == "Base_Code.x"] <- "Base_Code"
trim <- function (x) gsub("^\\s+|\\s+$", "", x)
allsku$IBP_Family_Name <- trim(allsku$IBP_Family_Name)
allsku$IBP_Sub_Category_Name <- trim(allsku$IBP_Sub_Category_Name)
allsku$IBP_Category_Name <- trim(allsku$IBP_Category_Name)
allsku <- allsku %>% distinct()

############### CVC LOGICAL CHECK ########################
sku_base <- summaryBy(Base_Code ~ SKU ,data = allsku ,FUN=length)
sku_ff <- summaryBy(Format_Flavor.x ~ SKU ,data =allsku ,FUN=length)
base_fam <- aggregate(data=allsku, IBP_Family_Name ~ Base_Code, function(x) length(unique(x)))
ff_fam <- aggregate(data=allsku, IBP_Family_Name ~ Format_Flavor_Name, function(x) length(unique(x)))
fam_subc <- aggregate(data=allsku, IBP_Sub_Category_Name ~ IBP_Family_Name, function(x) length(unique(x)))
subc_cat <- aggregate(data=allsku, IBP_Category_Name ~ IBP_Sub_Category_Name, function(x) length(unique(x)))

ch1<- sku_base[which(sku_base[,2]>1),]
ch2<-sku_ff[which(sku_ff[,2]>1),]
ch3<-ff_fam[which(ff_fam[,2]>1),]
ch4<-fam_subc[which(fam_subc[,2]>1),]
ch5<-subc_cat[which(subc_cat[,2]>1),]
ch6<-base_fam[which(base_fam[,2]>1),]


if (nrow(ch1[1]) == 0) {
  print("SKU-Basecode CVC is correct")
}else if (nrow(ch1[1]) != 0) {
  for (i in 1:length(ch1[,1])) {
    print("Check SKU-Basecode")
    print(allsku[which(allsku$SKU == ch1[i,1]),3][1])
  }
}

if (nrow(ch2[1]) == 0) {
  print("SKU-Format Flavor CVC is correct")
}else if (nrow(ch2[1]) != 0) {
  for (i in 1:length(ch2[,1])) {
    print("Check SKU-Format Flavor")
    print(allsku[which(allsku$SKU == ch2[i,1]),5][1])
  }
}

if (nrow(ch3[1]) == 0) {
  print("Format Flavor - IBP Family CVC is correct")
}else if (nrow(ch3[1]) != 0) {
  for (i in 1:length(ch3[,1])) {
    print("Check Format Flavor - IBP Family")
    print(allsku[which(allsku$Format_Flavor_Name == ch3[i,1]),7][1])
  }
}
if (nrow(ch6[1]) == 0) {
  print("Base Code - IBP Family CVC is correct")
}else if (nrow(ch6[1]) != 0) {
  for (i in 1:length(ch6[,1])) {
    print("Check Base Code - IBP Family")
    print(allsku[which(allsku$Base_Code == ch6[i,1]),7][1])
  }
}
if (nrow(ch4[1]) == 0) {
  print("IBP Family - Sub Category CVC is correct")
}else if (nrow(ch4[1]) != 0) {
  for (i in 1:length(ch4[,1])) {
    print("Check IBP Family - Sub Category")
    print(allsku[which(allsku$IBP_Family_Name == ch4[i,1]),9][1])
  }
}
if (nrow(ch5[1]) == 0) {
  print("Sub Category - Category CVC is correct")
}else if (nrow(ch5[1]) != 0) {
  for (i in 1:length(ch5[,1])) {
    print("Check Sub Category - Category")
    print(allsku[which(allsku$IBP_Sub_Category_Name == ch5[i,1]),11][1])
  }
}

#check weight and yield per Basecode.


for (i in 1:length(levels(as.factor(allsku$Base_Code)))) {
  temp1 <- subset(allsku, Base_Code == levels(as.factor(allsku$Base_Code))[i])
  if (length(temp1$SKU)>=2) {
    temp1$zpeso <- scale(as.numeric(temp1$Peso_Neto_por_unidad_gr), scale = T, center = T)
    temp1$zpeso[is.na(temp1$zpeso)] <- 0
    
    for (k in 1:length(temp1$zpeso)) {
      if (temp1$zpeso[k] > 2) {
        print(paste0("This SKU's weight: ", temp1$Peso_Neto_por_unidad_gr[k], " has more than 2 S.D. from it's mean, please check Basecode: ",temp1$Base_Code[k] ))
      }else{print("This Basecode's weights are correct")}
    }
    
  }
}

for (i in 1:length(levels(as.factor(allsku$Base_Code)))) {
  temp1 <- subset(allsku, Base_Code == levels(as.factor(allsku$Base_Code))[i])
  if (length(temp1$SKU)>=2) {
    temp1$zrend <- scale(as.numeric(temp1$Rendimiento), scale = T, center = T)
    temp1$zrend[is.na(temp1$zrend)] <- 0
    for (k in 1:length(temp1$zrend)) {
      if (temp1$zrend[k] > 2.5) {
        print(paste0("This SKU's yield: ", temp1$Rendimiento[k], " has more than 2 S.D. from it's mean, please check Basecode: ",temp1$Base_Code[k] ))
      }else{print("All Basecode's yields are correct")}
      
    }
  }
}

names(allsku)[names(allsku) == "IBP_Family.x"] <- "IBP_Family"
names(allsku)[names(allsku) == "Format_Flavor.x"] <- "Format_Flavor"
########################################################
#########################################################
#########################################################
print("STOP")
###########################################################
##########################################################
#################### BY HAND ##############################
#check details, add weights from ecc.
#NOTE: if weight is NA SKU should have only forecast and should be found in ECC and not in BWP with history
write.csv(allsku, "Output\\jerarquia_list.csv")
hier1 <- read.csv("Input\\jerarquia_list_1.csv", stringsAsFactors = F)

##################################################################


#################################################
#Create the input to run lifecycle status algorithms
listcube_fcast=subset(listcube, as.numeric(Period) >= fechafcast)
sum(listcube$Cat_fcast/1000)
sum(listcube_fcast$Cat_fcast/1000)
listcubeagg2 <- summaryBy(Cat_fcast ~ SKU + Base_Code + Period + canal, data = listcube_fcast ,FUN=sum)
listcubeagg2<- na.omit(listcubeagg2)
names(listcubeagg2)[names(listcubeagg2) == "Base_Code"] <- "BASECODE"
names(listcubeagg2)[names(listcubeagg2) == "Period"] <- "PERIOD"
names(listcubeagg2)[names(listcubeagg2) == "Cat_fcast.sum"] <- "SELLIN_KG.sum"
if (is.null(canales)==T) {
  basefcast<- merge(BWPagg2, listcubeagg2, by = c("BASECODE", "SKU", "PERIOD","SELLIN_KG.sum"), all = T)
  
}else{basefcast<- merge(BWPagg2, listcubeagg2, by = c("BASECODE", "SKU", "PERIOD","SELLIN_KG.sum","canal"), all = T)
}
basefcast<- basefcast[order(basefcast$SKU, decreasing = TRUE),]  
basefcast$BASECODE<- fill.na(basefcast$BASECODE)
basefcast <- merge(basefcast,hier1[,c("SKU", "Base_Code")], by = "SKU", all= T)
for (i in 1:length(basefcast$Base_Code)) {
  if (is.na(basefcast$Base_Code)[i]==T) {
    basefcast$Base_Code[i]<- basefcast$BASECODE[i]
    
  }
  
}

######################################################
if (is.null(BWP_first) == T & is.null(canales) == F) {
  
  basefcast$SELLIN_TONS <- basefcast$SELLIN_KG.sum/1000
  data = c(inicio:final)
  Period = matrix(nrow=length(data), ncol=12)
  for (i in 1:length(data)) {
    Period[i,] <- sequence(12) + rep(as.numeric(paste0(data[i],"01"))-1, 12)
  }
  
  
  flattenlist <- function(x){
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){
      Recall(out)
    }else{
      return(out)
    }
  }
  
  PERIOD <- flattenlist(lapply(seq_len(ncol(Period)), function(i) Period[,i]))
  PERIOD <- unlist(PERIOD, recursive = FALSE)
  PERIOD <- as.data.frame(PERIOD)
  
  PERIOD$PERIOD <- as.character(PERIOD$PERIOD)
  basefcast$PERIOD <- as.character(basefcast$PERIOD)
  basefcast<- basefcast[!is.na(basefcast["PERIOD"]),]
  canal <- canales
  canal <- as.data.frame(canal)
  
  trial3 <- basefcast[,c("SKU","Base_Code")]
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3 <- merge(trial3,PERIOD)
  trial3 <- merge(trial3,canal)
  trial3 <- merge(trial3,basefcast[,c("SKU","canal", "Base_Code","PERIOD","SELLIN_TONS")], by = c("SKU", "canal", "Base_Code","PERIOD"), all=T)
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3$SELLIN_TONS <- as.numeric(as.character(trial3$SELLIN_TONS))
  trial3$SELLIN_TONS[is.na(trial3$SELLIN_TONS) == T] <- 0
  
  basefcastagg <- summaryBy(SELLIN_TONS ~ Base_Code + PERIOD, data = trial3, FUN=sum)
  basefcastagg_wide <- spread(basefcastagg, Base_Code, SELLIN_TONS.sum)
  basefcastagg_wide[is.na(basefcastagg_wide)] <- 0
}else if (is.null(canales) ==F & is.null(BWP_first) == F) {
  
  names(legacy)[names(legacy) == "CANAL"] <- "canal"
  legacy <-  legacy[which(legacy$PERIOD <= BWP_first),]
  legacy <- merge(legacy, hier1[,c("SKU","Base_Code")], by="SKU")
  names(basefcast)
  basefcast$SELLIN_TONS <- basefcast$SELLIN_KG.sum/1000
  basefcast <-  basefcast[which(basefcast$PERIOD >= BWP_first),] 
  basefcast2<- merge(basefcast, legacy, by = c("Base_Code", "SKU", "PERIOD","SELLIN_TONS", "canal"), all = T)
  
  data = c(inicio:final)
  Period = matrix(nrow=length(data), ncol=12)
  for (i in 1:length(data)) {
    Period[i,] <- sequence(12) + rep(as.numeric(paste0(data[i],"01"))-1, 12)
  }
  
  
  flattenlist <- function(x){
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){
      Recall(out)
    }else{
      return(out)
    }
  }
  
  PERIOD <- flattenlist(lapply(seq_len(ncol(Period)), function(i) Period[,i]))
  PERIOD <- unlist(PERIOD, recursive = FALSE)
  PERIOD <- as.data.frame(PERIOD)
  
  PERIOD$PERIOD <- as.character(PERIOD$PERIOD)
  basefcast2$PERIOD <- as.character(basefcast2$PERIOD)
  basefcast2<- basefcast2[!is.na(basefcast2["PERIOD"]),]
  canal <- canales
  canal <- as.data.frame(canal)
  
  trial3 <- basefcast2[,c("SKU","Base_Code")]
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3 <- merge(trial3,PERIOD)
  trial3 <- merge(trial3,canal)
  trial3 <- merge(trial3,basefcast2[,c("SKU","canal", "Base_Code","PERIOD","SELLIN_TONS")], by = c("SKU", "canal", "Base_Code","PERIOD"), all=T)
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3$SELLIN_TONS <- as.numeric(as.character(trial3$SELLIN_TONS))
  trial3$SELLIN_TONS[is.na(trial3$SELLIN_TONS) == T] <- 0
  
  
  basefcastagg <- summaryBy(SELLIN_TONS ~ Base_Code + PERIOD, data = trial3, FUN=sum)
  basefcastagg_wide <- spread(basefcastagg, Base_Code, SELLIN_TONS.sum)
  basefcastagg_wide[is.na(basefcastagg_wide)] <- 0
}else if (is.null(canales) ==T & is.null(BWP_first) == F) {
  
  
  legacy <-  legacy[which(legacy$PERIOD < BWP_first),]
  legacy <- merge(legacy, hier1[,c("SKU", "Base_Code")], by="SKU")
  
  names(legacy)
  basefcast$SELLIN_TONS <- basefcast$SELLIN_KG.sum/1000
  basefcast <-  basefcast[which(basefcast$PERIOD >= BWP_first),] 
  basefcast2<- merge(basefcast, legacy, by = c("Base_Code", "SKU", "PERIOD","SELLIN_TONS"), all = T)
  
  
  data = c(inicio:final)
  Period = matrix(nrow=length(data), ncol=12)
  for (i in 1:length(data)) {
    Period[i,] <- sequence(12) + rep(as.numeric(paste0(data[i],"01"))-1, 12)
  }
  
  
  flattenlist <- function(x){
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){
      Recall(out)
    }else{
      return(out)
    }
  }
  
  PERIOD <- flattenlist(lapply(seq_len(ncol(Period)), function(i) Period[,i]))
  PERIOD <- unlist(PERIOD, recursive = FALSE)
  PERIOD <- as.data.frame(PERIOD)
  
  PERIOD$PERIOD <- as.character(PERIOD$PERIOD)
  basefcast2$PERIOD <- as.character(basefcast2$PERIOD)
  basefcast2<- basefcast2[!is.na(basefcast2["PERIOD"]),]
  
  trial3 <- basefcast2[,c("SKU", "Base_Code")]
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3 <- merge(trial3,PERIOD)
  trial3 <- merge(trial3,basefcast2[,c("SKU", "Base_Code","PERIOD","SELLIN_TONS")], by = c("SKU", "Base_Code","PERIOD"), all.x=T)
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3$SELLIN_TONS <- as.numeric(as.character(trial3$SELLIN_TONS))
  trial3$SELLIN_TONS[is.na(trial3$SELLIN_TONS) == T] <- 0
  
  
  basefcastagg <- summaryBy(SELLIN_TONS ~ Base_Code + PERIOD, data = trial3, FUN=sum)
  basefcastagg_wide <- spread(basefcastagg, Base_Code, SELLIN_TONS.sum)
  basefcastagg_wide[is.na(basefcastagg_wide)] <- 0
}else if (is.null(canales) ==T & is.null(BWP_first) == T) {
  
  names(basefcast)
  basefcast$SELLIN_TONS <- basefcast$SELLIN_KG.sum/1000
  
  
  data = c(inicio:final)
  Period = matrix(nrow=length(data), ncol=12)
  for (i in 1:length(data)) {
    Period[i,] <- sequence(12) + rep(as.numeric(paste0(data[i],"01"))-1, 12)
  }
  
  
  flattenlist <- function(x){
    morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
    out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
    if(sum(morelists)){
      Recall(out)
    }else{
      return(out)
    }
  }
  
  PERIOD <- flattenlist(lapply(seq_len(ncol(Period)), function(i) Period[,i]))
  PERIOD <- unlist(PERIOD, recursive = FALSE)
  PERIOD <- as.data.frame(PERIOD)
  
  PERIOD$PERIOD <- as.character(PERIOD$PERIOD)
  basefcast$PERIOD <- as.character(basefcast$PERIOD)
  basefcast<- basefcast[!is.na(basefcast["PERIOD"]),]
  trial3 <- basefcast[,c("SKU", "Base_Code")]
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3 <- merge(trial3,PERIOD)
  trial3 <- merge(trial3,basefcast[,c("SKU", "Base_Code","PERIOD","SELLIN_TONS")], by = c("SKU", "Base_Code","PERIOD"), all=T)
  trial3 <- trial3 %>% dplyr::filter(!(SKU==""))
  trial3 <- trial3 %>% distinct()
  trial3$SELLIN_TONS <- as.numeric(as.character(trial3$SELLIN_TONS))
  trial3$SELLIN_TONS[is.na(trial3$SELLIN_TONS) == T] <- 0
  
  
  basefcastagg <- summaryBy(SELLIN_TONS ~ Base_Code + PERIOD, data = trial3, FUN=sum)
  basefcastagg_wide <- spread(basefcastagg, Base_Code, SELLIN_TONS.sum)
  basefcastagg_wide[is.na(basefcastagg_wide)] <- 0
}

#############################################################
#lifecycle status algorithms 
Timeanalysis<-function(Data){
  columnas<-ncol(Data)
  filas<-nrow(Data)
  nombre<-colnames(Data)
  Resultado<-matrix(c(1:columnas*11)*0,nrow=columnas, ncol=11)
  colnames(Resultado)<-c("Ceros", "consecutivos", "Positivos", "Negativos", "Var inicio","Var medio","Var final", "0 inicio","0 medio","0 final", "Tipo")
  rownames(Resultado)<-nombre
  
  control<-floor(filas/3)
  
  
  DataTransformada<-as.matrix(apply(Data,c(1,2),function (x) {
    if (x>0){x<-1} else {x<-0}}))
  
  MatrizdeDeltas<-rbind(c(1:columnas)*0,DataTransformada[2:filas,]-DataTransformada[1:(filas-1),])
  
  for (i in 1:columnas){
    
    Resultado[i,1]<-sum(DataTransformada[,i]==0)
    
    Resultado[i,3]<-sum(MatrizdeDeltas[,i]==1)
    
    Resultado[i,4]<-sum(MatrizdeDeltas[,i]==-1)
    
    Resultado[i,5]<-sum(MatrizdeDeltas[1:control,i]!=0)
    
    Resultado[i,6]<-sum(MatrizdeDeltas[(control+1):(filas-control),i]!=0)
    
    Resultado[i,7]<-sum(MatrizdeDeltas[(filas-control+1):filas,i]!=0)
    
    Data.t<-data.table(DataTransformada[,i])
    Data.t[, isMidPoint := FALSE]
    value<-DataTransformada[,i]
    Data.t <- Data.t[!is.na(value)][(c(FALSE, !value[-(1:2)], FALSE) & c(!value[-(length(value))], FALSE) & c(FALSE, !value[-length(value)])), isMidPoint := TRUE]
    
    Resultado[i,2]<-sum(Data.t[,2]==TRUE)  
    
    Resultado[i,8]<-sum(Data.t[1:control,2]==TRUE)  
    Resultado[i,9]<-sum(Data.t[(control+1):(filas-control),2]==TRUE)  
    Resultado[i,10]<-sum(Data.t[(filas-control+1):filas,2]==TRUE)
    
    if(Resultado[i,1]==filas){Resultado[i,11]<-"VACIO"}else{if(Resultado[i,1]==0){Resultado[i,11]<-"LLENO"}else
    {if (Resultado[i,3]==Resultado[i,4]&Resultado[i,2]==0){Resultado[i,11]<-"LLENO"}else {
      if(Resultado[i,3]>Resultado[i,4]){Resultado[i,11]<-"SUBIDA"} 
      if(Resultado[i,3]<Resultado[i,4]&Resultado[i,4]>1){Resultado[i,11]<-"BAJADA INTERMITENTE"}
      if(Resultado[i,3]<Resultado[i,4]&Resultado[i,4]<2){Resultado[i,11]<-"BAJADA"}
      if(Resultado[i,3]==Resultado[i,4]){Resultado[i,11]<-"INTERMITENTE"} 
    }}}
  }
  return(Resultado)
}

BaseCodeLifeCycle<-function(Data,primerdatoforecast){
  
  columnas<-ncol(Data)
  filas<-nrow(Data)
  nombre<-colnames(Data)
  Resultado<-matrix(c(1:columnas*2)*0,nrow=columnas, ncol=2)
  colnames(Resultado)<-c("LifeCycle_Status", "History Forecast")
  rownames(Resultado)<-nombre
  
  backwards<-Timeanalysis(Data[1:(primerdatoforecast-1),])
  forward<-Timeanalysis(Data[primerdatoforecast:filas,])
  pivot<-paste(backwards[,11],forward[,11])
  
  for (i in 1:columnas){
    switch(pivot[i]
           ,"VACIO VACIO"={Resultado[i,1]<-"NO HIST AND FCAST"}
           ,"VACIO LLENO"={Resultado[i,1]<-"NPD PHASE IN"}
           ,"VACIO SUBIDA"={Resultado[i,1]<-"NPD PHASE IN"}
           ,"VACIO BAJADA"={Resultado[i,1]<-"IN & OUT"}
           ,"VACIO BAJADA INTERMITENTE"={Resultado[i,1]<-"IN & OUT"}
           ,"VACIO INTERMITENTE"={Resultado[i,1]<-"IN & OUT"}
           ,"LLENO VACIO"={Resultado[i,1]<-"PHASE OUT"}
           ,"LLENO LLENO"={ if (sum(Data[1:(primerdatoforecast-1),i]>0)>12) {Resultado[i,1]<-"REGULAR"}else{Resultado[i,1]<-"NPD"}}
           ,"LLENO SUBIDA"={Resultado[i,1]<-"REVIEW"}
           ,"LLENO BAJADA"={Resultado[i,1]<-"PHASE OUT"}
           ,"LLENO BAJADA INTERMITENTE"={Resultado[i,1]<-"PHASE OUT"}
           ,"LLENO INTERMITENTE"={Resultado[i,1]<-"REVIEW"}
           ,"SUBIDA VACIO"={Resultado[i,1]<-"REVIEW"}
           ,"SUBIDA LLENO"={if (sum(Data[1:(primerdatoforecast-1),i]>0)>12) {Resultado[i,1]<-"REGULAR"}else{Resultado[i,1]<-"NPD"}}
           ,"SUBIDA SUBIDA"={Resultado[i,1]<-"REVIEW"}
           ,"SUBIDA BAJADA"={Resultado[i,1]<-"IN & OUT"}
           ,"SUBIDA BAJADA INTERMITENTE"={Resultado[i,1]<-"IN & OUT"}
           ,"SUBIDA INTERMITENTE"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA VACIO"={Resultado[i,1]<-"DESLISTED REGULAR"}
           ,"BAJADA LLENO"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA SUBIDA"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA BAJADA"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA BAJADA INTERMITENTE"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA INTERMITENTE"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA INTERMITENTE VACIO"={Resultado[i,1]<-"DESLISTED REGULAR"}
           ,"BAJADA INTERMITENTE LLENO"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA INTERMITENTE SUBIDA"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA INTERMITENTE BAJADA"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA INTERMITENTE BAJADA INTERMITENTE"={Resultado[i,1]<-"REVIEW"}
           ,"BAJADA INTERMITENTE INTERMITENTE"={Resultado[i,1]<-"REVIEW"}
           ,"INTERMITENTE VACIO"={Resultado[i,1]<-"DESLISTED IN & OUT"}
           ,"INTERMITENTE LLENO"={Resultado[i,1]<-"REVIEW"}
           ,"INTERMITENTE SUBIDA"={Resultado[i,1]<-"REVIEW"}
           ,"INTERMITENTE BAJADA"={Resultado[i,1]<-"REVIEW"}
           ,"INTERMITENTE BAJADA INTERMITENTE"={Resultado[i,1]<-"DESLISTED IN & OUT"}
           ,"INTERMITENTE INTERMITENTE"={Resultado[i,1]<-"IN & OUT"}
    )
    Resultado[i,2]<-pivot[i]
  }
  
  return(Resultado)
}

#######
#formating for new algorithm that helps with reviews
######
basefcastagg_wide2 <-  subset(basefcastagg_wide, PERIOD <= as.character(six_months))
basefcastagg_wide2 <-  subset(basefcastagg_wide2, PERIOD >= as.character(as.numeric(substr(fechafcast,1,4))-3))
Timeanalysis(basefcastagg_wide2)
resultado <- BaseCodeLifeCycle(basefcastagg_wide2, which(basefcastagg_wide2[,1]>fechafcast-1)[1])
names(hier1)[names(hier1) == "Life.Cycle.Actual"] <- "LifeCycle_StatusActual"
hieragg <- hier1[,c("Base_Code","LifeCycle_StatusActual")]
hieragg2 <- hier1[,c("Base_Code","LifeCycle_StatusActual", "SKU")]
hieragg <- hieragg %>% distinct()
resultado <-resultado[-1,]
resultado <- as.data.frame(resultado)
resultado$Base_Code <- row.names(as.data.frame(resultado))
resultado <-resultado[,-2]


lifecycle <- merge(resultado, hieragg, by ="Base_Code" ,all = T)
lifecycle$Base_Code <- trim(lifecycle$Base_Code)
lifecycle$LifeCycle_Status <- trim(lifecycle$LifeCycle_Status)
lifecycle$LifeCycle_StatusActual <- trim(lifecycle$LifeCycle_StatusActual)

lifecycle2 <- merge(resultado, hieragg2, by ="Base_Code" ,all = T)
lifecycle2$Base_Code <- trim(lifecycle2$Base_Code)
lifecycle2$LifeCycle_Status <- trim(lifecycle2$LifeCycle_Status)
lifecycle2$LifeCycle_StatusActual <- trim(lifecycle2$LifeCycle_StatusActual)

sku_base <- summaryBy(Base_Code ~ SKU ,data = lifecycle2 ,FUN=length)
ch1<- sku_base[which(sku_base[,2]>1),]
if (nrow(ch1[1]) == 0) {
  print("SKU-Basecode CVC is correct")
}else if (nrow(ch1[1]) != 0) {
  for (i in 1:length(ch1[,1])) {
    print("Check SKU-Basecode")
    print(hier1[which(hier1$SKU == ch1[i,1]),2])
  }
}


for (i in 2:length(lifecycle$Base_Code)) {
  if (lifecycle$Base_Code[i] == lifecycle$Base_Code[i-1] &
      is.na(lifecycle$LifeCycle_StatusActual[i-1])==T) 
  {lifecycle$LifeCycle_StatusActual[i-1] <- lifecycle$LifeCycle_StatusActual[i]
  }else if (lifecycle$Base_Code[i] == lifecycle$Base_Code[i-1] 
            & is.na(lifecycle$LifeCycle_StatusActual[i])==T)
  {lifecycle$LifeCycle_StatusActual[i] <- lifecycle$LifeCycle_StatusActual[i-1]}
}



lifecycle <- lifecycle %>% distinct()
lifecycle<- lifecycle[which(lifecycle$Base_Code != "PERIOD"),]


lifecycle$LifeCycle_Status <-  as.character(lifecycle$LifeCycle_Status)

#if NA old status
for (i in 1:length(lifecycle$LifeCycle_Status)) {
  
  if (is.na(lifecycle$LifeCycle_Status[i])==T) {
    lifecycle$LifeCycle_Status[i] <-  as.character(lifecycle$LifeCycle_StatusActual[i])
    
  }
}

#change "no information" to "no history no forecast"
for (i in 1:length(lifecycle$LifeCycle_Status)) {
  
  if (lifecycle$LifeCycle_Status[i] == "NO INFORMATION"&
      is.na(lifecycle$LifeCycle_StatusActual[i])== F) {
    lifecycle$LifeCycle_Status[i] <-  "NO HIST AND FCAST"
    
  }
}

for (i in 1:length(lifecycle$LifeCycle_Status)) {
  
  if (lifecycle$LifeCycle_StatusActual[i] == "DELISTED" &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F) {
    lifecycle$LifeCycle_StatusActual[i] <-  "DESLISTED"
    
  }
}
#if regular and is a "lesser" status change back to regular
for (i in 1:length(lifecycle$LifeCycle_Status)) {
  
  if (lifecycle$LifeCycle_StatusActual[i] == "REGULAR" & lifecycle$LifeCycle_Status[i] == "NPD" &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "REGULAR" & lifecycle$LifeCycle_Status[i] == "IN & OUT" 
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "REGULAR" & lifecycle$LifeCycle_Status[i] == "NPD PHASE IN" 
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "REGULAR" & lifecycle$LifeCycle_Status[i] == "NO HIST AND FCAST"
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F) {
    lifecycle$LifeCycle_Status[i] <-  "REGULAR"
    
    
  }
}

#if Deslisted and changed to a "higher" status keep deslisted
for (i in 1:length(lifecycle$LifeCycle_Status)) {
  
  if (lifecycle$LifeCycle_StatusActual[i] == "DESLISTED" & lifecycle$LifeCycle_Status[i] == "PHASE OUT" 
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "DESLISTED" & lifecycle$LifeCycle_Status[i] == "IN & OUT" 
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "DESLISTED" & lifecycle$LifeCycle_Status[i] == "NPD PHASE IN"
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "DESLISTED" & lifecycle$LifeCycle_Status[i] == "NO HIST AND FCAST"
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F|
      lifecycle$LifeCycle_StatusActual[i] == "DESLISTED" & lifecycle$LifeCycle_Status[i] == "REGULAR"
      &
      is.na(lifecycle$LifeCycle_StatusActual[i])== F) {
    lifecycle$LifeCycle_Status[i] <-  "DESLISTED"
  }
}

#ONLY REVIEW
lifecycle_review <-  lifecycle[which(lifecycle$LifeCycle_Status == "REVIEW"),]

#If already phase out in ast keep it
for (i in 1:length(lifecycle_review$LifeCycle_Status)) {
  if (lifecycle_review$LifeCycle_StatusActual[i]=="PHASE OUT") {
    lifecycle_review$LifeCycle_Status[i] <-  "PHASE OUT"
  }
}
#If already deslisted in ast keep it
for (i in 1:length(lifecycle_review$LifeCycle_Status)) {
  if (lifecycle_review$LifeCycle_StatusActual[i]=="DESLISTED" |lifecycle_review$LifeCycle_StatusActual[i]=="DELISTED") {
    lifecycle_review$LifeCycle_Status[i] <-  "DESLISTED"
  }
}


#df for plot
basefcastagg_plot <- merge(lifecycle_review, basefcastagg, by.x="Base_Code",by.y="Base_Code", all.x = T) 
basefcastagg_plot$Base_Code <- as.factor(basefcastagg_plot$Base_Code)
basefcastagg_plot <- droplevels(basefcastagg_plot)
basefcastagg_plot2 <- merge(lifecycle, basefcastagg, by.x="Base_Code",by.y="Base_Code", all.x = T) 
basefcastagg_plot2$Base_Code <- as.factor(basefcastagg_plot2$Base_Code)
basefcastagg_plot2 <- droplevels(basefcastagg_plot2)
basefcastagg_plot<- basefcastagg_plot[order(basefcastagg_plot$PERIOD, decreasing = F),]
#############################
#this algorithm tests the reviews and adds status
####keep testing. 
#basefcastagg_plot
x <- matrix(NA, nrow=length(levels(basefcastagg_plot$Base_Code)),ncol = 2) %>% data.frame()
for (i in 1:length(levels(basefcastagg_plot$Base_Code))) {
  if (is.na(basefcastagg_plot$LifeCycle_StatusActual[i])==T) {
    basefcastagg_plot$LifeCycle_StatusActual[i] <- "NEW"
  }
  data=subset(basefcastagg_plot, Base_Code == levels(as.factor(basefcastagg_plot$Base_Code))[i])
  data2=subset(data, PERIOD == as.character(fechafcast))
  
  if (substr(fechafcast,5,6)== 12) {
    data3 = subset(data, PERIOD == as.character(paste0(as.numeric(substr(fechafcast,1,4))+1, "01")))
  }else{data3 = subset(data, PERIOD == as.character(fechafcast+1))}
  
  data4 = data[(which(data[,4]>fechafcast)[1])-2:(which(data[,4]>fechafcast-100)[1]),]
  data5 = subset(data, PERIOD >= as.character(fechafcast))
  if (data$LifeCycle_Status == "REVIEW" & data$LifeCycle_StatusActual == "NEW") {
    x[i,1] <- "MANUAL REVIEW"
    x[i,2] = as.character(data2$Base_Code)
  }
  else if (data$LifeCycle_Status == "DESLISTED") {
    x[i,1] <- "DESLISTED"
    x[i,2] = as.character(data2$Base_Code)
  }
  else if (data$LifeCycle_Status == "PHASE OUT") {
    x[i,1] <- "PHASE OUT"
    x[i,2] = as.character(data2$Base_Code)
  }
  else if (data$LifeCycle_Status == "REVIEW") {
    
    if (data2$SELLIN_TONS <= 0 & data2$LifeCycle_StatusActual == "PHASE OUT" | data3$SELLIN_TONS <=0 & data2$LifeCycle_StatusActual == "PHASEOUT") {
      x[i,1] <- "DESLISTED"
      x[i,2] = as.character(data2$Base_Code)
    }else{x[i,1] <- "PHASE OUT"
    x[i,2] = as.character(data2$Base_Code)}
    if (data2$SELLIN_TONS <= 0 & data2$LifeCycle_StatusActual == "REGULAR" & data3$SELLIN_TONS <=0 & data2$LifeCycle_StatusActual == "REGULAR") {
      x[i,1] <- "PHASE OUT"
      x[i,2] <-as.character(data2$Base_Code)
    }else{    x[i,1] <- "REGULAR"
    x[i,2] <-as.character(data2$Base_Code)}
    
    if(data$LifeCycle_StatusActual == "NPD" 
       & length(which(data4$SELLIN_TONS.sum[1:12] > 0))>=12 & data2$SELLIN_TONS >0) {
      x[i,1] <- "REGULAR"
      x[i,2] <-as.character(data2$Base_Code)
    }
    else if (data$LifeCycle_StatusActual == "NPD" 
             & length(which(data4$SELLIN_TONS[1:12] > 0))<=12) {
      x[i,1] <- "NPD"
      x[i,2] <-as.character(data2$Base_Code)
    }
    if (data$LifeCycle_StatusActual == "IN & OUT" &
        length(which(data4$SELLIN_TONS[1:12] > 0))<=12 ) {
      x[i,1] <- "IN & OUT"
      x[i,2] <-as.character(data2$Base_Code)
    }
    else if (data$LifeCycle_StatusActual == "IN & OUT" &
             length(which(data4$SELLIN_TONS[1:12] > 0))>=12) {
      x[i,1] <- "REGULAR"
      x[i,2] <-as.character(data2$Base_Code)
    }
    if (data$LifeCycle_StatusActual == "NPD PHASE IN" & length(which(data4$SELLIN_TONS[1:12] > 0))<=12 & #que tenga menos de 12 meses de venta y forecast lleno
        length(which(data5$SELLIN_TONS[1:length(data5$SELLIN_TONS)] > 0))>= length(data5$SELLIN_TONS) |
        is.na(data$LifeCycle_StatusActual) == T & length(which(data4$SELLIN_TONS[1:12] > 0))<=12 & #que tenga menos de 12 meses de venta y forecast lleno
        length(which(data5$SELLIN_TONS[1:length(data5$SELLIN_TONS)] > 0))>= length(data5$SELLIN_TONS) |
        data$LifeCycle_StatusActual == "PHASE IN" & length(which(data4$SELLIN_TONS[1:12] > 0))<=12 & #que tenga menos de 12 meses de venta y forecast lleno
        length(which(data5$SELLIN_TONS[1:length(data5$SELLIN_TONS)] > 0))>= length(data5$SELLIN_TONS) |
        data$LifeCycle_StatusActual == "NPD PHASE IN" & length(which(data4$SELLIN_TONS[1:12] > 0))<=12 & #que tenga menos de 12 meses de venta y forecast lleno
        length(which(data5$SELLIN_TONS[1:length(data5$SELLIN_TONS)] > 0))){
      x[i,1] <- "NPD PHASE IN"
      x[i,2] <-as.character(data2$Base_Code)
    }
  }
}

#############################################################################
#visual review if needed. just change the basecode
x
P <- ggplot(data=subset(basefcastagg_plot2, Base_Code == "CR0249"), 
            aes(x=as.character(PERIOD), y=as.numeric(SELLIN_TONS.sum), group=Base_Code))+ geom_point() +geom_line() 
P + theme(axis.text.x = element_text(angle = 90, hjust = 1))


#results with hierarchy
results2 <- as.data.frame(x)
names(results2)[names(results2) == "X1"] <- "LifeCycle_Status"
manual<- results2[which(results2[,1]=="MANUAL REVIEW"),]
manual<- list(manual$X2)

#########################################################################
#########################################################################
###### "manual" #####

#to change status by hand add status in list "newstatus" in order as it apears in "manual"
if (length(manual[[1]])>0) {
  
  
  newstatus <- list('PHASE OUT',
                    'IN & OUT',
                    'IN & OUT',
                    'REGULAR',
                    'REGULAR',
                    'REGULAR',
                    'REGULAR',
                    'PHASE OUT',
                    'PHASE OUT',
                    'REGULAR',
                    'IN & OUT',
                    'REGULAR',
                    'DESLISTED',
                    'REGULAR',
                    'REGULAR',
                    'IN & OUT',
                    'REGULAR',
                    'REGULAR',
                    'PHASE OUT',
                    'NPD',
                    'PHASE OUT',
                    'REGULAR',
                    'NPD')
  for (i in 1:length(manual)) {
    results2[which(results2[,2] == manual[i]),1] <- newstatus[i]
  }
}
results2
##########################################################################
#merge results
lifecycle2<- merge(lifecycle, results2, by.x = "Base_Code", by.y = "X2", all.x=T)

for (i in 1:length(lifecycle2$Base_Code)) {
  if (is.na(lifecycle2$LifeCycle_Status.y[i])==FALSE) {
    lifecycle2$LifeCycle_Status.x[i] <-  lifecycle2$LifeCycle_Status.y[i]}
}

hier1 <- merge(hier1, lifecycle2[,c(1,2)], by = "Base_Code", all.x=T)

###########################
#aqui sobreescribe lo regular?

for (i in 2:length(hier1$Base_Code)) {
  
  if (is.na(hier1$LifeCycle_StatusActual[i]) == T & hier1$Base_Code[i] == hier1$Base_Code[i-1] &
      hier1$LifeCycle_Status.x[i] == hier1$LifeCycle_Status.x[i-1]) {
    hier1$LifeCycle_StatusActual[i] <-  hier1$LifeCycle_Status.x[i]
    
  }else if (is.na(hier1$LifeCycle_StatusActual[i-1]) == T & hier1$Base_Code[i] == hier1$Base_Code[i-1] &
            hier1$LifeCycle_Status.x[i-1] == hier1$LifeCycle_Status.x[i]) {
    hier1$LifeCycle_StatusActual[i-1] <-  hier1$LifeCycle_Status.x[i-1]
  }
}


hier1[is.na(hier1)] <- "NO HIST AND FCAST"

#########################################
#grammar errors changed

for (i in 1:length(hier1$LifeCycle_StatusActual)) {
  
  if (hier1$LifeCycle_StatusActual[i] == "DELISTED") {
    hier1$LifeCycle_StatusActual[i] <-  "DESLISTED"
    
  } else if (hier1$LifeCycle_StatusActual[i] == "DELISTED IN & OUT") {
    hier1$LifeCycle_StatusActual[i] <-  "DESLISTED IN & OUT"
  }else if (hier1$LifeCycle_StatusActual[i] == "DELISTED REGULAR") {
    hier1$LifeCycle_StatusActual[i] <-  "DESLISTED REGULAR"
  }
}

for (i in 1:length(hier1$LifeCycle_StatusActual)) {
  
  if (hier1$LifeCycle_Status.x[i] == "DELISTED") {
    hier1$LifeCycle_Status.x[i] <-  "DESLISTED"
    
  } else if (hier1$LifeCycle_Status.x[i] == "DELISTED IN & OUT") {
    hier1$LifeCycle_Status.x[i] <-  "DESLISTED IN & OUT"
  }else if (hier1$LifeCycle_Status.x[i] == "DELISTED REGULAR") {
    hier1$LifeCycle_Status.x[i] <-  "DESLISTED REGULAR"
  }
}


#if no history and no forecast, but was desliested keep delisted
for (i in 1:length(hier1$LifeCycle_StatusActual)) {
  if (substr(hier1$LifeCycle_StatusActual,1,9)[i] == "DESLISTED" &
      hier1$LifeCycle_Status.x[i]== "NO HIST AND FCAST") {
    hier1$LifeCycle_Status.x[i] <- hier1$LifeCycle_StatusActual[i]
  }
}


#check past status with new status for check
for (i in 1:length(hier1$Base_Code)) {
  if (hier1$LifeCycle_StatusActual[i] != hier1$LifeCycle_Status.x[i] &
      substr(hier1$LifeCycle_StatusActual,1,9)[i] != substr(hier1$LifeCycle_Status.x,1,9)[i] &
      substr(hier1$LifeCycle_StatusActual,1,3)[i] !=
      substr(hier1$LifeCycle_Status.x,1,3)[i]) {
    hier1$match[i] <- "DOUBLE CHECK"
  }else{hier1$match[i] <- "OK"}
}
checkme<- unique(subset(hier1[,c("Base_Code", "LifeCycle_StatusActual", "LifeCycle_Status.x", "match")], match == "DOUBLE CHECK"))

checkme2 <- subset(checkme, !(Base_Code %in% x$X2))
checkme2

write.csv(checkme2, "Output\\checkme2.csv")
############################################################
############################################################
#in order, write "OK", if the status is OK, otherwise Write down the new status in the list.

P <- ggplot(data=subset(basefcastagg_plot2, Base_Code == "CR0781"), 
            aes(x=as.character(PERIOD), y=as.numeric(SELLIN_TONS.sum), group=Base_Code))+ geom_point() +geom_line() 
P + theme(axis.text.x = element_text(angle = 90, hjust = 1))

length(checkme2$Base_Code)
manual<- list('OK',
              'OK',
              'DESLISTED',
              'DESLISTED',
              'OK',
              'IN & OUT',
              'DESLISTED',
              'DESLISTED',
              'DESLISTED',
              'DESLISTED',
              'DESLISTED',
              'OK',
              'DESLISTED',
              'DESLISTED',
              'DESLISTED',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'OK',
              'IN & OUT',
              'DESLISTED',
              'DESLISTED',
              'DESLISTED',
              'DESLISTED',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT',
              'IN & OUT')


length(manual)

checkme2$manual <- unlist(manual)
if (length(manual[[1]])>0) {
  for (i in 1:length(checkme2$Base_Code)) {
    if (checkme2$manual[i] != "OK") {
      checkme2$LifeCycle_Status.x[i] <- checkme2$manual[i]
      
    }
  }
}


checkme3 <- merge(checkme, checkme2, all = T)
checkme3 <-  checkme3[which(is.na(checkme3$manual) == F),]
hier1_1 <- merge(hier1, checkme3[,c("Base_Code", "LifeCycle_Status.x")], by= c("Base_Code"), all.x= T)
hier1_1 <- distinct(hier1_1)
sku_base <- summaryBy(Base_Code ~ SKU ,data = hier1_1 ,FUN=length)
ch1<- sku_base[which(sku_base[,2]>1),]
if (nrow(ch1[1]) == 0) {
  print("SKU-Basecode CVC is correct")
}else if (nrow(ch1[1]) != 0) {
  for (i in 1:length(ch1[,1])) {
    print("Check SKU-Basecode Go Back To Manual list above")
    print(hier1[which(hier1$SKU == ch1[i,1]),2])
    print(hier1[which(hier1$SKU == ch1[i,1]),1])
  }
}



#formating
hier1 <- merge(hier1_1, checkme3[,c("Base_Code", "LifeCycle_Status.x")], by= c("Base_Code"), all.x= T)



for (i in 1:length(hier1$Base_Code)) {
  if (hier1$match[i]=="DOUBLE CHECK" & is.na(hier1$LifeCycle_Status.x.y)[i]==F) {
    hier1$LifeCycle_Status.x.x[i] <- hier1$LifeCycle_Status.x.y[i]
  }
}

hier1 <- unique(as.data.frame(hier1[,1:17]))

for (i in 2:length(hier1$Base_Code)) {
  
  if (is.na(hier1$LifeCycle_StatusActual[i]) == T & hier1$Base_Code[i] == hier1$Base_Code[i-1] &
      hier1$LifeCycle_Status.x.x[i] == hier1$LifeCycle_Status.x.x[i-1]) {
    hier1$LifeCycle_StatusActual[i] <-  hier1$LifeCycle_Status.x.x[i]
    
  }else if (is.na(hier1$LifeCycle_StatusActual[i-1]) == T & hier1$Base_Code[i] == hier1$Base_Code[i-1] &
            hier1$LifeCycle_Status.x.x[i-1] == hier1$LifeCycle_Status.x.x[i]) {
    hier1$LifeCycle_StatusActual[i-1] <-  hier1$LifeCycle_Status.x.x[i-1]
  }
}


sku_base <- summaryBy(Base_Code ~ SKU ,data = hier1 ,FUN=length)
ch1<- sku_base[which(sku_base[,2]>1),]
if (nrow(ch1[1]) == 0) {
  print("SKU-Basecode CVC is correct")
}else if (nrow(ch1[1]) != 0) {
  for (i in 1:length(ch1[,1])) {
    print("Check SKU-Basecode")
    print(hier1[which(hier1$SKU == ch1[i,1]),2])
  }
}


names(hier1)[names(hier1) == "LifeCycle_StatusActual"] <- "LifeCycle_Status_Old"
names(hier1)[names(hier1) == "LifeCycle_StatusOld"] <- "LifeCycle_Status_Old_Old"
names(hier1)[names(hier1) == "LifeCycle_Status.x.x"] <- "LifeCycle_Status_Actual"

#write.csv(hier1,"Output\\hierarchy_final.csv")
hier1<- read.csv("Output\\hierarchy_final.csv")
#########################################
#for History
hist1<- merge(trial3, hier1, by = "SKU",all.x=T)

if (is.null(canales)==F) {
  History <- summaryBy(SELLIN_TONS ~ IBP_Family_Name + Format_Flavor_Name + Base_Code.y + LifeCycle_Status_Actual +
                         SKU + Detalle + PERIOD + canal, data = hist1, FUN=sum)
  colnames(History) <-  c ("IBP_Family",	"IBP_Format_Flavor",	"BASECODE",	"LIFECYCLE_STATUS",
                           "SKU",	"DETALLE_SKU",	"PERIODO_MDLZ",	"CANAL", "SELLIN_TONS.sum")
}else{
  History <- summaryBy(SELLIN_TONS ~ IBP_Family_Name + Format_Flavor_Name + Base_Code.y + LifeCycle_Status_Actual +
                         SKU + Detalle + PERIOD, data = hist1, FUN=sum)
  colnames(History) <-  c ("IBP_Family",	"IBP_Format_Flavor",	"BASECODE",	"LIFECYCLE_STATUS",
                           "SKU",	"DETALLE_SKU",	"PERIODO_MDLZ", "SELLIN_TONS.sum")
}


write.csv(History, "Output\\History.csv")

names(hier1)
#hier1<-read.csv("Output\\hierarchy_final.csv")
hier1<- hier1[,c(2,3,1,4,5,6,7,8,9,10,11,12,13,14,16)]
Hist <- History
Hier <- hier1

######################################################################################################################################################################
######################################################################################################################################################################
######################################################################################################################################################################

#####################################################################################
##############                                                                      #
# Parameters #                                                                      #
#############                                                                       #
#integer between 0 and 1. Sensibilidad de limpiezas                                 #
sensibilidad =                                                                   #
#limpieza 4                                                                         #
desviacion =                                                                       #
ventanas =                                                                         #
#
################                                                                    #
# Rightsizing #                                                                     #
###############                                                                     #                                                                                    #
#last three months for rightisizing                                                 #
tres_meses =                                                                  #
mes_hoy =                                                                     #
#############                                                                       #
# Forecast #                                                                        #
############                                                                        #
#number of windows                                                                  #
ventana_fcast =                                                                   #
#PC cores to compute forecast                                                       #
cores =                                                                            #
#window size                                                                        #
tamano_ventana =                                                                   #
#series frequency                                                                   #
mensual =                                                                         #
#start date of the series                                                           #
inicio = c(,)                                                                 #
#####################################################################################


#cambio a historia limpia
Hist$LIFECYCLE_STATUS <- as.factor(Hist$LIFECYCLE_STATUS)
Hist_reg <-  subset(Hist, LIFECYCLE_STATUS == "REGULAR")
Hist_PO <-  subset(Hist, LIFECYCLE_STATUS == "PHASE OUT")
HistComp <- merge(Hist_reg, Hist_PO, all =T)
#HistComp <-  subset(HistComp, PERIODO_MDLZ <= 201908)

#condicional para nombre de canal
if (!is.null(HistComp$CANAL)== TRUE) {
  
  colnames(HistComp) <- c("IBP_Family",	"IBP_Format_Flavor",	"Basecode",	"LIFECYCLE_STATUS",
                          "SKU", "DETALLE_SKU", "Periodo", "Canal", "SellIn")
  
}else{colnames(HistComp) <- c("IBP_Family",	"IBP_Format_Flavor",	"Basecode",	"LIFECYCLE_STATUS",
                              "SKU", "DETALLE_SKU", "Periodo", "SellIn")}


#------------------------------------------------
##correr rightsizing

# script para identificar cual es el sku activo dentro de un basecode
# se requiere de un data frame con las sigueintes columnas
# basecode->el base code
# sku-> el # de sku
# promedio-> La venta promedio de los ultimos 3 meses
# activo-> una columna de 0 la cual el algoritmo colocar 1 si es que este es el ultimo sku al cual se debe ajustar los otros
# encerada que sirva de indicador de si es el actual o no
ruta <- HistComp[HistComp$Periodo  >= tres_meses,] #cambiar a fecha para promedio de tres meses
ruta <- ruta[ruta$Periodo  <= mes_hoy,] #cambiar a fecha para promedio de tres meses
if (is.null(HistComp$Canal)) {
  Data <- aggregate(SellIn ~ Basecode + SKU , ruta, mean)
}else{Data <- aggregate(SellIn ~ Basecode + SKU , ruta[,-8], mean)}

Data$Activo <- 0
colnames(Data) <- c("Basecode",	"SKU",	"Promedio",	"Activo")

auxiliar<-as.character(Data$Basecode)[!duplicated(Data$Basecode)]
tamano<-length(auxiliar)

for (i in 1:tamano){
  pivot<-Data[Data$Basecode==auxiliar[i],]
  sku<-pivot$SKU[pivot$Promedio==max(pivot$Promedio)]
  Data$Activo[Data$SKU==sku]=1
}

HistComp <- merge(x = HistComp, y = Data[ , c("Activo", "SKU")], by = "SKU", all.x=TRUE)
HistComp$Activo[is.na(HistComp$Activo)] <- 0

#--------------------------------------------------
###agregar peso neto por unidad si es que no viene en pre tool
#hierarchy de excel

colnames(Hier) <- c("SKU",	"Detalle SKU",	"Basecode",
                    "Format Flavor", "IBP Format Flavor", "IBP Family Code",
                    "IBP Family", "IBP SUb Category", "IBP SUb Category Name",
                    "IBP Category", "IBP Category Name", "Peso", "Rendimiento",
                    "LifeCycle anterior", "LifeCycle nuevo")

HistComp <- merge(x = HistComp, y = Hier[ , c("Peso", "SKU", "Rendimiento")], by = "SKU", all=TRUE)
Hist_reg <-  subset(HistComp, LIFECYCLE_STATUS == "REGULAR")
Hist_PO <-  subset(HistComp, LIFECYCLE_STATUS == "PHASE OUT")
HistComp <- merge(Hist_reg, Hist_PO, all = T)


#---------------------------------------------------------------
##agregar peso y renidmiento al cual ajustar 
HistComp$Peso  <- gsub(",", "", HistComp$Peso, fixed = TRUE)
HistComp$Rendimiento  <- gsub(",", "",HistComp$Rendimiento, fixed = TRUE)
HistComp$Peso <- as.numeric(HistComp$Peso)
HistComp$Rendimiento <- as.numeric(HistComp$Rendimiento)
HistComp[is.na(HistComp)] <- 0

rights<- HistComp[HistComp$Activo == 1,]

Peso_Ajustar<- rights %>% 
  group_by(Basecode) %>% 
  dplyr::summarise(Peso_Ajustar = mean(Peso))

HistComp <- merge(HistComp, Peso_Ajustar , by = "Basecode", all.x = T)

Rend_Ajustar<- rights %>% 
  group_by(Basecode) %>% 
  dplyr::summarise(Rend_Ajustar = mean(Rendimiento))

HistComp <- merge(HistComp, Rend_Ajustar , by = "Basecode", all.x = T)

#--------------------------------------------------------------
##sell in comparable
#(sell in *  peso al cual ajustar)/peso * rend/rend ajust si Rend = 1, else:sell in *  peso al cual ajustar)/peso

for (i in 1:length(HistComp$Basecode)) {
  if (HistComp$Rend_Ajustar[i] != 0) {
    HistComp$SellIn_Comp[i] = ((HistComp$SellIn[i]*HistComp$Peso_Ajustar[i])/HistComp$Peso[i])*(HistComp$Rendimiento[i]/HistComp$Rend_Ajustar[i])
    
  }else{HistComp$SellIn_Comp[i] = (HistComp$SellIn[i]*HistComp$Peso_Ajustar[i])/HistComp$Peso[i]}
}

#revisar la suma de sell in vs comp. si dif muy grande algo est mal.
HistComp2 <- subset(HistComp, Periodo <= mes_hoy)
sum(HistComp2$SellIn)
sum(HistComp2$SellIn_Comp)
summary(as.numeric(HistComp2$Periodo))
if (is.null(HistComp2$Canal)==T) {
  HistComp2<- HistComp2[,c(3,4,1,5,2,6,9,10,12,11,13,7,8,14)]
}else{HistComp2<- HistComp2[,c(3,4,1,5,2,6,10,11,13,12,14,7,8,9,15)]}



write.csv(HistComp2, "Output\\Hist_comp.csv")
#-------------------------------------------------------------------
##Historia Limpia

###hacer un algortim que cheque la diferencia entre sell in y sell in comparable, y ver si hay errores grandes,
#esto puede ser porque el peso esta mal de algn SKU

if (is.null(canales)==F) {
  colnames(HistComp2) <-  c("IBP_Family", "IBP_Format_Flavor","Basecode", "LIFECYCLE_STATUS", "SKU", "DETALLE_SKU",
                            "Activo" ,"Peso", "Peso_Ajustar", "Rendimiento", "Rend_Ajustar", "Periodo", "Canal", "SellIn", "SellIn_Comp")
  
}else{colnames(HistComp2) <-  c("IBP_Family", "IBP_Format_Flavor","Basecode", "LIFECYCLE_STATUS", "SKU", "DETALLE_SKU",
                                "Activo" ,"Peso", "Peso_Ajustar", "Rendimiento", "Rend_Ajustar", "Periodo", "SellIn", "SellIn_Comp")
}


str(HistComp2)
HistComp2$Periodo <- trim(HistComp2$Periodo)
HistComp2<-  HistComp2[which(HistComp2$Periodo <= mes_hoy),]
if (!is.null(HistComp2$Canal)== TRUE) {
  HistComp2 <- aggregate(SellIn_Comp ~ Basecode + Canal +IBP_Family +
                           IBP_Format_Flavor + Periodo, HistComp2, sum)
}else{HistComp2 <- aggregate(SellIn_Comp ~ Basecode +IBP_Family +
                               Periodo, HistComp2, sum)}

if (!is.null(HistComp2$Canal)== TRUE) {
  HistComp2$clave <- paste(HistComp2$Basecode,HistComp2$Canal, sep = "_")
  data_wide <- spread(HistComp2[,c(-1,-2,-3,-4)], clave, SellIn_Comp)
}else{
  data_wide <- HistComp2[,c(-2)] %>% 
    group_by(Basecode, Periodo) %>% 
    mutate(grouped_id = row_number())
  data_wide <- data_wide %>% 
    spread(Basecode, SellIn_Comp) %>% 
    select(-grouped_id)}

data_wide<-na.omit(data_wide)
data_wide<-as.data.frame(data_wide)
data_wide<-  data_wide[which(data_wide$Periodo <= mes_hoy),]
if (inicio[2]>=10) {
  data_wide<-  data_wide[which(data_wide$Periodo >= as.numeric(paste0(inicio[1],inicio[2]))),]
  
}else{data_wide<-  data_wide[which(data_wide$Periodo >= as.numeric(paste0(inicio[1],"0",inicio[2]))),]}
write.csv(data_wide, "Output\\data_wide.csv")


#### function to clean history ####

### it might be nedded to install the AnomalyDetection package through github
### data_base it is a data frame where the first collumn is the period and all collumns have the same number of observations
### sensitivity is the significance level for the outlier detection
### the output of the function is a list with 4 data frames
# 1º data frame regards structural breaks 
# 2º data frame regards outliers
# 3º data frame is the loess decompostion
# 4º data frame is the reconstructed series

clean_history_oficial <- function (data_base, 
                                   sensitivity = sensibilidad){
  df <- data_base
  #### create data.frames that will store outputs ####
  num_row <- nrow(data_base)
  num_col <- ncol(data_base) - 1
  breaks_df <- matrix(data = 0, nrow = num_row, ncol = num_col) 
  outliers_df <- matrix(data = 0, nrow = num_row, ncol = num_col)
  loess_df <- matrix(data = NA, nrow = num_row, ncol = num_col)
  cleansed_df <- data_base
  
  #### creates ts list ####
  ts_list <- list()
  for(i in 1:num_col){
    x <- df[,(i+1)]
    ts_list[i] <- ts(x,frequency = 12) %>% list()
    
  }
  
  for(i in 1:num_col){
    act_serie <- ts_list[[i]]
    position <- strucchange::breakpoints(act_serie~1)
    position <- position$breakpoints
    num_outliers <- length(position)
    if (num_outliers >0){
      for(j in 1:num_outliers){
        pos <- position[j]
        breaks_df[pos,i] <- 1
      }
    }
  }
  
  
  #### fills outliers data.frame ####
  for(i in 1:num_col){
    act_serie <- AnomalyDetection::AnomalyDetectionVec(data_base[,(i+1)],
                                                       max_anoms=0.3,
                                                       alpha = sensitivity,
                                                       period=12,
                                                       direction='both',
                                                       plot=FALSE)
    outliers_position <- act_serie$anoms$index
    num_outliers <- length(outliers_position)
    if(  num_outliers >0){
      for(j in 1:num_outliers){
        pos <- outliers_position[j]
        outliers_df[pos,i] <- 1
      }
    }
  }
  
  #### fills loess data.frame and celansed history data.frame ####
  pr <- .95
  for(i in 1:num_col){
    q  <- quantile(ts_list[[i]], c(1-pr, pr))
    ts_list[[i]] <- squish(ts_list[[i]], q)
  }
  
  for(i in 1:num_col){
    partial <- stl(ts_list[[i]],11)
    loess_df[,i] <- c(partial$time.series[,2]+partial$time.series[,1])
  }
  
  #### formatt data.frames ####
  loess_df <- data.frame(df[,1],loess_df)
  outliers_df <- data.frame(df[,1],outliers_df)
  breaks_df <- data.frame(df[,1],breaks_df)
  colnames(loess_df) <- colnames(df)
  colnames(outliers_df) <- colnames(df)
  colnames(breaks_df) <- colnames(df)
  
  #### create cleansed data.frame ####
  
  for(i in 1:num_col){
    for(j in 1:num_row){
      check_outlier <- outliers_df[j,(i+1)] 
      
      if(check_outlier == 1){
        cleansed_df[j,(i+1)] <- loess_df[j,(i+1)]
      }  
      
    }
  }
  
  resultado <- list(breaks_df,outliers_df,loess_df,cleansed_df)
  return(resultado)
  
}

clean_history_quantile <- function (data_base, 
                                    sensitivity = sensibilidad){
  
  df <- data_base
  #### create data.frames that will store outputs ####
  num_row <- nrow(data_base)
  num_col <- ncol(data_base) - 1
  breaks_df <- matrix(data = 0, nrow = num_row, ncol = num_col) 
  outliers_df <- matrix(data = 0, nrow = num_row, ncol = num_col)
  loess_df <- matrix(data = NA, nrow = num_row, ncol = num_col)
  cleansed_df <- data_base
  
  #### creates ts list ####
  ts_list <- list()
  for(i in 1:num_col){
    x <- df[,(i+1)]
    ts_list[i] <- ts(x,frequency = 12) %>% list()
    
  }
  
  for(i in 1:num_col){
    act_serie <- ts_list[[i]]
    position <- strucchange::breakpoints(act_serie~1)
    position <- position$breakpoints
    num_outliers <- length(position)
    if (num_outliers >0){
      for(j in 1:num_outliers){
        pos <- position[j]
        breaks_df[pos,i] <- 1
      }
    }
  }
  
  
  
  #### fills outliers data.frame ####
  for(i in 1:num_col){
    act_serie <- AnomalyDetectionVec(data_base[,(i+1)],
                                     max_anoms=0.3,
                                     alpha = sensitivity,
                                     period=12,
                                     direction='both',
                                     plot=FALSE)
    outliers_position <- act_serie$anoms$index
    num_outliers <- length(outliers_position)
    if(  num_outliers >0){
      for(j in 1:num_outliers){
        pos <- outliers_position[j]
        outliers_df[pos,i] <- 1
      }
    }
  }
  
  #### fills loess data.frame and celansed history data.frame ####
  
  
  pr <- .95
  for(i in 1:num_col){
    q  <- quantile(ts_list[[i]], c(1-pr, pr))
    ts_list[[i]] <- squish(ts_list[[i]], q)
  }
  
  for(i in 1:num_col){
    partial <- stl(ts_list[[i]],11)
    loess_df[,i] <- c(partial$time.series[,2]+partial$time.series[,1])
  }
  
  #### formatt data.frames ####
  loess_df <- data.frame(df[,1],loess_df)
  outliers_df <- data.frame(df[,1],outliers_df)
  breaks_df <- data.frame(df[,1],breaks_df)
  colnames(loess_df) <- colnames(df)
  colnames(outliers_df) <- colnames(df)
  colnames(breaks_df) <- colnames(df)
  
  #### create cleansed data.frame ####
  
  for(i in 1:num_col){
    for(j in 1:num_row){
      check_outlier <- outliers_df[j,(i+1)] 
      
      if(check_outlier == 1){
        cleansed_df[j,(i+1)] <- loess_df[j,(i+1)]
      }  
      
    }
  }
  
  resultado <- list(breaks_df,outliers_df,loess_df,cleansed_df)
  return(resultado)
  
}



clean_history_iterative<-function(Data, 
                                  sensitivity=sensibilidad){
  
  #parametros generales
  Data<-as.data.frame(Data)
  Resultado<-Data
  nombres<-names(Data)
  m<-length(Data[1,])
  n<-length(Data[,1])
  ao<-round(Data[1,1]/100,0)
  mes<-Data[1,1]-ao*100
  fechas<-as.data.frame(seq(from=as.Date(paste(ao,mes,"01",sep="-")),length=n,by="months"))
  
  
  #loop por columnas
  
  for(i in 2:m){
    numero_outliers=1
    dta_partial<-cbind(fechas,Data[,i])
    names(dta_partial)<-nombres[c(1,i)]
    info<-tbl_df(dta_partial)
    diff<-0.1
    #loop de deteccion
    while (numero_outliers>0&diff<=sensitivity){
      df_parcial<-as.data.frame(info%>%time_decompose(nombres[i],method = "stl")%>%anomalize(remainder,method = "gesd")%>%time_recompose())
      numero_outliers<-length(df_parcial$anomaly[df_parcial$anomaly=="Yes"])
      if (numero_outliers==0) {
        Resultado[,i]<-df_parcial$observed
      }else{
        ajuste<-c(1:numero_outliers)*0
        df_parcial$remainder[df_parcial$anomaly=="Yes"]<-ajuste
        Resultado[,i]<-df_parcial$season+df_parcial$trend+df_parcial$remainder
        dta_partial<-cbind(fechas,Resultado[,i])
        names(dta_partial)<-nombres[c(1,i)]
        info<-tbl_df(dta_partial)
        b<-Resultado[,i]-Data[,i]
        diff<-length(b[b!=0])/n
        
      }
    }
    print(i)
  }
  return(Resultado)
}

pre_limpieza<-function(Data,desviaciones,ventana){
  
  #parametros generales
  Data<-as.data.frame(Data)
  Resultado<-Data
  nombres<-names(Data)
  columna<-length(Data[1,])
  fila<-length(Data[,1])
  LCL<-Data*0
  MediaMovil<-Data*0
  DesviacionStd<-Data*0
  UCL<-Data*0
  
  #Construyo los valores: Media Movil y Desviacion para cada ventana en cada serie. Con ellos los lmites se compara y asigna
  
  for (j in 2:columna){
    for (i in (ventana+1):fila){
      MediaMovil[i,j]<-mean(c(Resultado[(i-ventana):(i-1),j],Data[i,j]))
      DesviacionStd[i,j]<-sd(Resultado[(i-ventana):(i-1),j])
      UCL[i,j]<-MediaMovil[i,j]+desviaciones*DesviacionStd[i,j]
      LCL[i,j]<-max(MediaMovil[i,j]-desviaciones*DesviacionStd[i,j],0)
      if (Data[i,j]>LCL[i,j]) { 
        if (Data[i,j]<UCL[i,j]){}else{Resultado[i,j]<- UCL[i,j]}
      }else{Resultado[i,j]<- LCL[i,j]}
    }
  }
  
  #Regresan resultados
  return(Resultado)
}

#### OFICIAL VARIANTE 1 ####

###read data, run function, extract relevant info.
data1 <- data_wide
data1 <- sapply(data1, as.numeric )
data1<- as.data.frame(data1)
data2 <- clean_history_oficial(data1)
data3 <- data2[4]

#transform data to reshape it to copy and paste easily in excel.
data3<- as.data.frame(data3[1])
data3$Period2 <- factor(data3$Periodo)
data_long_oficial <- reshape2::melt(data3, id.vars = c("Periodo" , "Period2"))


#### QUANTIL FUNCTION VARIANTE 2 ####
data2 <- clean_history_quantile(data1)
data3 <- data2[4]
data3<- as.data.frame(data3[1])
data3$Period2 <- factor(data3$Periodo)
data_long_quantile <- reshape2::melt(data3, id.vars = c("Periodo" , "Period2"))


#### ITERATIVE FUNCTION VARIANTE 3 ####
data1[data1 == 0] <- 0.0001
data2 <- clean_history_iterative(data1)
data2$Period2 <- factor(data2$Period)
data_long_iterative <- reshape2::melt(data2, id.vars = c("Periodo" , "Period2"))


#### PRELIMIPIEZA + OFICIAL VARIANTE 4 ####
###read data, run function, extract most relevant info.
data2 <- pre_limpieza(data1,desviacion,ventanas) #3,3 POR DEFAULT
data3 <- clean_history_oficial(data2)
data4 <- data3[4]

#transform data to reshape it to copy and paste easily in excel.
data4<- as.data.frame(data4[1])
data4$Period2 <- factor(data4$Periodo)
data_long_pre_limpieza <- reshape2::melt(data4, id.vars = c("Periodo" , "Period2"))


if (!is.null(HistComp2$Canal)== TRUE) {
  
  #change names for merge
  colnames(data_long_oficial)[4] <- "Variante_1"
  colnames(data_long_oficial)[3] <- "clave"
  colnames(data_long_quantile)[4] <- "Variante_2"
  colnames(data_long_quantile)[3] <- "clave"
  colnames(data_long_iterative)[4] <- "Variante_3"
  colnames(data_long_iterative)[3] <- "clave"
  colnames(data_long_pre_limpieza)[4] <- "Variante_4"
  colnames(data_long_pre_limpieza)[3] <- "clave"
  
  
  Hist_Limpia <- merge(x = HistComp2, y = data_long_oficial[,c(1,3,4)], by = c("clave", "Periodo"), all.x=TRUE)
  Hist_Limpia <- merge(x = Hist_Limpia, y = data_long_quantile[,c(1,3,4)], by = c("clave", "Periodo"), all.x=TRUE)
  Hist_Limpia <- merge(x = Hist_Limpia, y = data_long_iterative[,c(1,3,4)], by = c("clave", "Periodo"), all.x=TRUE)
  Hist_Limpia <- merge(x = Hist_Limpia, y = data_long_pre_limpieza[,c(1,3,4)], by = c("clave", "Periodo"), all.x=TRUE)
}else{#change names for merge
  colnames(data_long_oficial)[4] <- "Variante_1"
  colnames(data_long_oficial)[3] <- "Basecode"
  colnames(data_long_quantile)[4] <- "Variante_2"
  colnames(data_long_quantile)[3] <- "Basecode"
  colnames(data_long_iterative)[4] <- "Variante_3"
  colnames(data_long_iterative)[3] <- "Basecode"
  colnames(data_long_pre_limpieza)[4] <- "Variante_4"
  colnames(data_long_pre_limpieza)[3] <- "Basecode"
  
  
  Hist_Limpia <- merge(x = HistComp2, y = data_long_oficial[,c(1,3,4)], by = c("Basecode", "Periodo"), all.x=TRUE)
  Hist_Limpia <- merge(x = Hist_Limpia, y = data_long_quantile[,c(1,3,4)], by = c("Basecode", "Periodo"), all.x=TRUE)
  Hist_Limpia <- merge(x = Hist_Limpia, y = data_long_iterative[,c(1,3,4)], by = c("Basecode", "Periodo"), all.x=TRUE)
  Hist_Limpia <- merge(x = Hist_Limpia, y = data_long_pre_limpieza[,c(1,3,4)], by = c("Basecode", "Periodo"), all.x=TRUE)
  
}

#choice de limpieza!
if (inicio[2]>=10) {
  Hist_Limpia<-  Hist_Limpia[which(Hist_Limpia$Periodo >= as.numeric(paste0(inicio[1],inicio[2]))),]
  
}else{Hist_Limpia<-  Hist_Limpia[which(Hist_Limpia$Periodo >= as.numeric(paste0(inicio[1],"0",inicio[2]))),]}

levelsCH <- levels(as.factor(Hist_Limpia$Canal))
levelsIBP <- levels(as.factor(Hist_Limpia$IBP_Family))
result <- data.frame(matrix(nrow = length(Hist_Limpia$IBP_Family), ncol = 4))
xl <- list()
xlx <- list()
x <- list()
Dif1 <- NULL
Dif2 <- NULL
Dif3 <- NULL
Dif4 <- NULL
for (i in 1:length(levels(as.factor(Hist_Limpia$IBP_Family)))) {
  temp <- Hist_Limpia[Hist_Limpia$IBP_Family == levelsIBP[i],] #subset data
  bestlist <- list()
  typelist <- list()
  typelist2 <- list()
  familylist <- list()
  timelist <-  list()
  finlist <- list()
  finlist2 <- list()
  if (!is.null(Hist_Limpia$Canal)== TRUE) {
    for (k in 1:length(levels(as.factor(Hist_Limpia$Canal)))) {
      temp2 <- temp[temp$Canal == levelsCH[k],]
      
      temp2$SellIn_Comp[temp2$SellIn_Comp == 0] <- runif(temp2$SellIn_Comp, 0, .00001)
      
      rmse1<- rmse(temp2$SellIn_Comp, temp2$Variante_1)
      rmse2<- rmse(temp2$SellIn_Comp, temp2$Variante_2)
      rmse3<- rmse(temp2$SellIn_Comp, temp2$Variante_3)
      rmse4<- rmse(temp2$SellIn_Comp, temp2$Variante_4)
      mase1 <- mase(temp2$SellIn_Comp, temp2$Variante_1)
      mase2 <- mase(temp2$SellIn_Comp, temp2$Variante_2)
      mase3 <- mase(temp2$SellIn_Comp, temp2$Variante_3)
      mase4 <- mase(temp2$SellIn_Comp, temp2$Variante_4)
      #bias computes the average amount by which actual is greater than predicted
      bias1 <- bias(temp2$SellIn_Comp, temp2$Variante_1)
      bias2 <- bias(temp2$SellIn_Comp, temp2$Variante_2)
      bias3 <- bias(temp2$SellIn_Comp, temp2$Variante_3)
      bias4 <- bias(temp2$SellIn_Comp, temp2$Variante_4)
      
      #LA Trident eleccion = 1      #si agregamos media de sell in comp elige 3 que no est chida.
      mrmse<- mean(abs(c(rmse1,rmse2,rmse3,rmse4)))
      best2 <- min(abs(c(rmse1-mrmse,rmse2-mrmse,rmse3-mrmse,rmse4-mrmse)))
      mmase<- mean(abs(c(mase1,mase2,mase3,mase4)))
      best3 <- min(abs(c(mase1-mmase,mase2-mmase,mase3-mmase,mase4-mmase)))
      mbias<- mean(abs(c(bias1,bias2,bias3,bias4)))
      best4 <- max(abs(c(bias1-mbias,bias2-mbias,bias3-mbias,bias4-mbias)))
      
      best1_1 <- sum(abs(c(rmse1-mrmse,mase1-mmase,bias1-mbias)))
      best2_2 <- sum(abs(c(rmse2-mrmse,mase2-mmase,bias2-mbias)))
      best3_3 <- sum(abs(c(rmse3-mrmse,mase3-mmase,bias3-mbias)))
      best4_4 <- sum(abs(c(rmse4-mrmse,mase4-mmase,bias4-mbias)))
      
      
      #si la dif de esa media abs es menor a .01 ok, else imprimir choose en vez de variante.
      temp2$clave_fam <- paste0(temp2$IBP_Family[k],"_",temp2$Canal[k])
      
      if (best2 == rmse1-mrmse & best3 == mase1-mmase & best4 == bias1-mbias) {
        bestlist <- temp2$Variante_1
        typelist <- "Variante_1"
        vari = "Variante_1"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
        
      }else if (best2 == rmse2-mrmse & best3 == mase2-mmase & best4 == bias2-mbias) {
        bestlist <- temp2$Variante_2
        typelist <- "Variante_2"
        vari = "Variante_2"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
        
      }else if (best2 == rmse3-mrmse & best3 == mase3-mmase & best4 == bias3-mbias) {
        bestlist <- temp2$Variante_1
        typelist <- "Variante_3"
        vari = "Variante_3"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
        
      }else if (best2 == rmse1-mrmse & best3 == mase1-mmase & best4 != bias1-mbias | 
                best2 == rmse1-mrmse & best3 != mase1-mmase & best4 == bias1-mbias |
                best2 != rmse1-mrmse & best3 == mase1-mmase & best4 == bias1-mbias) {
        bestlist <- temp2$Variante_1
        typelist <- "Variante_1"
        vari = "Variante_1"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
        
      }else if (best2 == rmse2-mrmse & best3 == mase2-mmase & best4 != bias2-mbias | 
                best2 == rmse2-mrmse & best3 != mase2-mmase & best4 == bias2-mbias |
                best2 != rmse2-mrmse & best3 == mase2-mmase & best4 == bias2-mbias) {
        bestlist <- temp2$Variante_2
        typelist <- "Variante_2"
        vari = "Variante_2"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
        
      }else if (best2 == rmse3-mrmse & best3 == mase3-mmase & best4 != bias3-mbias | 
                best2 == rmse3-mrmse & best3 != mase3-mmase & best4 == bias3-mbias |
                best2 != rmse3-mrmse & best3 == mase3-mmase & best4 == bias3-mbias) {
        bestlist <- temp2$Variante_1
        typelist <- "Variante_3"
        vari = "Variante_3"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
      }else{
        bestlist <- temp2$Variante_4
        typelist <- "Variante_4"
        vari = "Variante_4"
        familylist[k] <- temp2$clave_fam
        timelist <- temp2$Periodo
      }
      
      #segundo check por diferencias de meidas entre mismas series
      
      if (abs(best1_1)^2 < abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_1") {
        typelist2 <- "Variante_1"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if(abs(best1_1)^2 < abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari != "Variante_1"){
        typelist2 <- paste0("Variente_1"," - Check ",vari)
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if(abs(best1_1)^2 < abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_4"){
        typelist2 <- "Variante_4 - Check Variante_1"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_3") {
        typelist2 <- "Variante_3 - Check Variante_1"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_1"){
        typelist2 <- "Varante_1 - Check Variante_3"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_1") {
        typelist2 <- "Variente_1 - Check Variante_4"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best3_3)^2 < abs(best4_4)^2 & vari == "Variante_3") {
        typelist2 <- "Variante_3"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best3_3)^2 < abs(best4_4)^2 & vari != "Variante_3") {
        typelist2 <- paste0("Variente_3"," - Check ",vari)
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best1_1)^2 < abs(best3_3)^2 & abs(best3_3)^2 > abs(best4_4)^2 & vari == "Variante_3") {
        typelist2 <- "Variante_3 - Check Variante_4"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if(abs(best1_1)^2 > abs(best3_3)^2 & abs(best3_3)^2 < abs(best4_4)^2 & vari == "Variante_3") {
        typelist2 <- "Variante_3 - Check Variante_1"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if(abs(best4_4)^2 < abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_4"){
        typelist2 <- "Variante_4"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best4_4)^2 < abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari != "Variante_4") {
        typelist2 <- paste0("Variente_1"," - Check ",vari)
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best4_4)^2 < abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_4") {
        typelist2 <- "Variante_4 - Check Variante_1"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }else if (abs(best4_4)^2 > abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_4") {
        typelist2 <- "Variante_4 - Check Variante_3"
        finlist[[k]] <-  c(typelist,familylist, bestlist, timelist)
      }
      
      
      xl[[i]] <- typelist
      xlx[[i]] <- typelist2
      x[[i]] <- familylist
      finlist2[[i]] <- finlist
      #print(xl)
    }
  }else{
    temp$SellIn_Comp[temp$SellIn_Comp == 0] <- runif(temp$SellIn_Comp, 0, .00001)
    
    rmse1<- rmse(temp$SellIn_Comp, temp$Variante_1)
    rmse2<- rmse(temp$SellIn_Comp, temp$Variante_2)
    rmse3<- rmse(temp$SellIn_Comp, temp$Variante_3)
    rmse4<- rmse(temp$SellIn_Comp, temp$Variante_4)
    mase1 <- mase(temp$SellIn_Comp, temp$Variante_1)
    mase2 <- mase(temp$SellIn_Comp, temp$Variante_2)
    mase3 <- mase(temp$SellIn_Comp, temp$Variante_3)
    mase4 <- mase(temp$SellIn_Comp, temp$Variante_4)
    #bias computes the average amount by which actual is greater than predicted
    bias1 <- bias(temp$SellIn_Comp, temp$Variante_1)
    bias2 <- bias(temp$SellIn_Comp, temp$Variante_2)
    bias3 <- bias(temp$SellIn_Comp, temp$Variante_3)
    bias4 <- bias(temp$SellIn_Comp, temp$Variante_4)
    
    
    mrmse<- mean(abs(c(rmse1,rmse2,rmse3,rmse4)))
    best2 <- min(abs(c(rmse1-mrmse,rmse2-mrmse,rmse3-mrmse,rmse4-mrmse)))
    mmase<- mean(abs(c(mase1,mase2,mase3,mase4)))
    best3 <- min(abs(c(mase1-mmase,mase2-mmase,mase3-mmase,mase4-mmase)))
    mbias<- mean(abs(c(bias1,bias2,bias3,bias4)))
    best4 <- max(abs(c(bias1-mbias,bias2-mbias,bias3-mbias,bias4-mbias)))
    
    best1_1 <- sum(abs(c(rmse1-mrmse,mase1-mmase,bias1-mbias)))
    best2_2 <- sum(abs(c(rmse2-mrmse,mase2-mmase,bias2-mbias)))
    best3_3 <- sum(abs(c(rmse3-mrmse,mase3-mmase,bias3-mbias)))
    best4_4 <- sum(abs(c(rmse4-mrmse,mase4-mmase,bias4-mbias)))
    
    if (best2 == rmse1-mrmse & best3 == mase1-mmase & best4 == bias1-mbias) {
      bestlist <- temp$Variante_1
      typelist <- "Variante_1"
      vari = "Variante_1"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
      
    }else if (best2 == rmse2-mrmse & best3 == mase2-mmase & best4 == bias2-mbias) {
      bestlist <- temp$Variante_2
      typelist <- "Variante_2"
      vari = "Variante_2"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
      
    }else if (best2 == rmse3-mrmse & best3 == mase3-mmase & best4 == bias3-mbias) {
      bestlist <- temp$Variante_1
      typelist <- "Variante_3"
      vari = "Variante_3"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
      
    }else if (best2 == rmse1-mrmse & best3 == mase1-mmase & best4 != bias1-mbias | 
              best2 == rmse1-mrmse & best3 != mase1-mmase & best4 == bias1-mbias |
              best2 != rmse1-mrmse & best3 == mase1-mmase & best4 == bias1-mbias) {
      bestlist <- temp$Variante_1
      typelist <- "Variante_1"
      vari = "Variante_1"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
      
    }else if (best2 == rmse2-mrmse & best3 == mase2-mmase & best4 != bias2-mbias | 
              best2 == rmse2-mrmse & best3 != mase2-mmase & best4 == bias2-mbias |
              best2 != rmse2-mrmse & best3 == mase2-mmase & best4 == bias2-mbias) {
      bestlist <- temp$Variante_2
      typelist <- "Variante_2"
      vari = "Variante_2"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
      
    }else if (best2 == rmse3-mrmse & best3 == mase3-mmase & best4 != bias3-mbias | 
              best2 == rmse3-mrmse & best3 != mase3-mmase & best4 == bias3-mbias |
              best2 != rmse3-mrmse & best3 == mase3-mmase & best4 == bias3-mbias) {
      bestlist <- temp$Variante_1
      typelist <- "Variante_3"
      vari = "Variante_3"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
    }else{
      bestlist <- temp$Variante_4
      typelist <- "Variante_4"
      vari = "Variante_4"
      familylist[i] <- temp$IBP_Family[[i]]
      timelist <- temp$Periodo
    }
    
    #segundo checi por diferencias de meidas entre mismas series
    
    if (abs(best1_1)^2 < abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_1") {
      typelist2 <- "Variante_1"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if(abs(best1_1)^2 < abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari != "Variante_1"){
      typelist2 <- paste0("Variente_1"," - Checi ",vari)
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if(abs(best1_1)^2 < abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_4"){
      typelist2 <- "Variante_4 - Check Variante_1"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_3") {
      typelist2 <- "Variante_3 - Check Variante_1"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_1"){
      typelist2 <- "Varante_1 - Check Variante_3"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_1") {
      typelist2 <- "Variente_1 - Check Variante_4"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best3_3)^2 < abs(best4_4)^2 & vari == "Variante_3") {
      typelist2 <- "Variante_3"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best1_1)^2 > abs(best3_3)^2 & abs(best3_3)^2 < abs(best4_4)^2 & vari != "Variante_3") {
      typelist2 <- paste0("Variente_3"," - Check ",vari)
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best1_1)^2 < abs(best3_3)^2 & abs(best3_3)^2 > abs(best4_4)^2 & vari == "Variante_3") {
      typelist2 <- "Variante_3 - Check Variante_4"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if(abs(best1_1)^2 > abs(best3_3)^2 & abs(best3_3)^2 < abs(best4_4)^2 & vari == "Variante_3") {
      typelist2 <- "Variante_3 - Check Variante_1"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if(abs(best4_4)^2 < abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_4"){
      typelist2 <- "Variante_4"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best4_4)^2 < abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari != "Variante_4") {
      typelist2 <- paste0("Variente_1"," - Check ",vari)
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best4_4)^2 < abs(best3_3)^2 & abs(best1_1)^2 < abs(best4_4)^2 & vari == "Variante_4") {
      typelist2 <- "Variante_4 - Check Variante_1"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }else if (abs(best4_4)^2 > abs(best3_3)^2 & abs(best1_1)^2 > abs(best4_4)^2 & vari == "Variante_4") {
      typelist2 <- "Variante_4 - Check Variante_3"
      finlist[[i]] <-  c(typelist,familylist, bestlist, timelist)
    }
    
    
    xl[[i]] <- typelist
    xlx[[i]] <- typelist2
    x[[i]] <- familylist[[i]]
    finlist2[[i]] <- finlist
  }
  
}


flattenlist <- function(x){
  morelists <- sapply(x, function(xprime) class(xprime)[1]=="list")
  out <- c(x[!morelists], unlist(x[morelists], recursive=FALSE))
  if(sum(morelists)){
    Recall(out)
  }else{
    return(out)
  }
}

if (!is.null(Hist_Limpia$Canal)== TRUE){
  
  xl2 <- flattenlist(xl)
  x2 <- flattenlist(x)
  xl3 <- flattenlist(xlx)
  
  result1 <- t(as.data.frame(x2))
  result1 <- as.data.frame(result1)
  rownames(result1) <- NULL
  result1$family_canal <-  as.character(result1$V1)
  result2 <- t(as.data.frame(xl2))
  result2 <- as.data.frame(result2)
  rownames(result2) <- NULL
  result3 <- t(as.data.frame(xl3))
  result3 <- as.data.frame(result3)
  rownames(result3) <- NULL
  
  
  names(result3)[names(result2) == "V1"] <- "Resultado"
  #extract everything except last 3 chars.
  
  fam <- NULL
  dat <- result1
  toDelete <- seq(1, nrow(dat), 2)
  dat <- dat[ toDelete ,]
  for (i in 1:length(dat$family_canal)) {
    fam[i] <- substr(dat$family_canal[i], 1, nchar(dat$family_canal[i])-3)
  }
  
  result3$family <- fam
  
  result3
  write.csv(result3, "Output\\sugerencias_limpiezas.csv")
}else{
  xl3<- flattenlist(xlx)
  xl2 <- flattenlist(xl)
  x2 <- flattenlist(x)
  
  result1 <- t(as.data.frame(x2))
  result1 <- as.data.frame(result1)
  rownames(result1) <- NULL
  result1$IBP_Family <-  as.character(result1$V1)
  result2 <- t(as.data.frame(xl2))
  result2 <- as.data.frame(result2)
  rownames(result2) <- NULL
  result3 <- t(as.data.frame(xl3))
  result3 <- as.data.frame(result3)
  rownames(result3) <- NULL
  
  
  
  #extract everything except last 3 chars.
  
  result_tot<- cbind(result1[,2], result2, result3)
  colnames(result_tot) <- c("IBP_Family","Check1","Check2")
  
  result_tot 
  write.csv(result_tot, "Output\\sugerencias_limpiezas.csv")
}


write.csv(Hist_Limpia, "Output\\Hist_Limpia.csv")



#dos opciones djar este check o ir directo a la siguinete sugernecia o sea variante 1 
#y as alimentar la base para cleansed df

#--------------------------------------------------------------------------------------
###base for forecast
###si sigue funcionando se necesita hacer un algoritmo que automaticamente te eliga el sell in limpio 
#dependiendo de la limpieza elegida por el algoritmo

####################################################################
####################################################################

#RUNING BOTTOM UP? "YES" OR "NO" ###################################

                            BU <- "YES"

#####################################################################
#####################################################################

if (BU == "YES") {
  cleansed_df <- read.csv("", stringsAsFactors = F)
}else{cleansed_df <- read.csv("", stringsAsFactors = F)}

##################################################################################################
#Removes families with zeros and with less than 18 obs that are zeros (zero = 0.0001) and constants

cleansed_df[cleansed_df== 0] <- 0.0001
cleansed_df$A_DUMMY <- 0.0001
cleansed_df <- cleansed_df[,c(1,as.numeric(length(colnames(cleansed_df))),2:(as.numeric(length(colnames(cleansed_df)))-1))]
cleansed_df_long <- gather(cleansed_df, Family, SI, 2:length(cleansed_df), factor_key=TRUE)
cleansed_df_long$Family = factor(cleansed_df_long$Family,
                                 levels(cleansed_df_long$Family)[c(as.numeric(length(colnames(cleansed_df[,-1]))),
                                                                   1:as.numeric(length(colnames(cleansed_df[,-1]))-1))])

#si es constante el valor separalo y ponle el mimo n�mero como fcast
x <- list()
cte <- list()
for (i in 1:length(levels(cleansed_df_long$Family))) {
  temp1 <- subset(cleansed_df_long, Family == levels(cleansed_df_long$Family)[i])
  last18<- tail(temp1$SI, n = 18)
  if (length(temp1$SI) != length(temp1$SI[temp1$SI==0.0001]) | length(temp1$SI)-length(temp1$SI[temp1$SI==0.0001])== 0){ 
    if (length(which(last18==0.0001))>=18){
      x[i] <- temp1$Family[1]
      
    }
    if (sum(last18)/18 == last18[1] & last18[1] != 0.0001) {
      cte[i] <- as.character(temp1$Family[1])
    }
  }else if (length(temp1$SI) == length(temp1$SI[temp1$SI==0.0001]) & length(temp1$SI[temp1$SI==0.0001]) != 0) {
    x[i] <- temp1$Family[1]
    
  }
}


x <- x[!unlist(lapply(x, is.null))]

for (k in 1:length(x)) {
  if (k==1) {
    cleansed_df <- cleansed_df[,-c(x[[k]])]
  }else if (k>1) {
    cleansed_df <- cleansed_df[,-c(x[[k]]-k+1)]
  }
}

if (length(cte)>0) {
  cte_family <- list()
  cte <- cte[!unlist(lapply(cte, is.null))]
  cleansed_df_clean <- cleansed_df[ , -which(names(cleansed_df) %in% cte)]
}else{cleansed_df_clean <- cleansed_df}



coffee_time_hybrid <- function(ts_used,
                               freq = 12,
                               n_forecast = 24,
                               w_size = 3, 
                               num_win = 1,
                               fixed_size = TRUE,
                               transf =NULL,
                               n_cores = 7){
  
  final_window <- length(ts_used) - w_size - num_win + 1
  
  cv_list <- forecastHybrid::tsPartition(x = ts_used,
                                         rolling = fixed_size,
                                         windowSize = final_window,
                                         maxHorizon = w_size)
  
  
  cv_results <- list()
  
  for(i in 1:length(cv_list)){
    train <- ts_used[cv_list[[i]]$trainIndices] %>% ts(frequency = freq)
    test <- ts_used[cv_list[[i]]$testIndices] %>% ts(frequency = freq) %>% as.vector()
    test_model <- forecastHybrid::hybridModel(y = train,
                                              weights = "insample.errors",
                                              lambda = transf,
                                              num.cores = n_cores)
    fc <- forecast(test_model,
                   h=w_size,
                   lambda = transf)
    arima_error <- forecast::accuracy(fc$auto.arima,test) %>% data.frame()
    ets_error <- forecast::accuracy(fc$ets,test) %>% data.frame()
    theta_error <- forecast::accuracy(fc$thetam,test) %>% data.frame()
    neural_error <- forecast::accuracy(fc$nnetar,test) %>% data.frame()
    tbats_error <- forecast::accuracy(fc$tbats,test) %>% data.frame()
    stl_error <- forecast::accuracy(fc$stlm,test) %>% data.frame()
    hybrid_error <- forecast::accuracy(fc,test) %>% data.frame()
    
    cv_results[i] <- list(arima_error,
                          ets_error,
                          theta_error,
                          neural_error,
                          tbats_error,
                          stl_error,
                          hybrid_error) %>%
      list()
    
  }
  
  
  
  
  final_model <- forecastHybrid::hybridModel(y = ts_used,
                                             weights = "insample.errors",
                                             lambda = transf,
                                             num.cores = n_cores)
  final_fc <- forecast(final_model,
                       h=n_forecast,
                       lambda = transf)
  arima <- final_fc$auto.arima%>% data.frame()
  ets <- final_fc$ets %>% data.frame()
  theta <- final_fc$thetam %>% data.frame()
  neural <- final_fc$nnetar %>% data.frame()
  tbats <- final_fc$tbats %>% data.frame()
  stl <- final_fc$stlm %>% data.frame()
  hybrid <- final_fc %>% data.frame()
  
  forecast_res <- list(arima,
                       ets,
                       theta,
                       neural,
                       tbats,
                       stl,
                       hybrid)
  
  results <- list(cv_results, forecast_res)
  
  return(results)
}



results_hybrid <- list()
for(i in 2:ncol(cleansed_df_clean)){
  print(colnames(cleansed_df_clean)[i])
  ts_used <- cleansed_df_clean[,i] %>% ts(frequency = mensual, start = inicio) #cambiar fecha
  #cambiar ventanas
  results_hybrid[i-1] <- coffee_time_hybrid(ts_used = ts_used, num_win = ventana_fcast,
                                            n_cores = cores, w_size = tamano_ventana) %>% list()
  
}

#### arrange errors for excel - aggregate and put in long format ####

n_cv <- results_hybrid[[1]][[1]] %>% length()
n_fam <- length(results_hybrid)

results_hybrid[[1]][[1]][[3]][[7]][2,]

training_error <- matrix(NA,nrow = n_cv, ncol = 7) %>% data.frame()
test_error <- matrix(NA,nrow = n_cv, ncol = 7) %>% data.frame()
mean_traning_error <- matrix(NA,nrow = 7, ncol = 7) %>% data.frame()
mean_test_error <- matrix(NA,nrow = 7, ncol = 6) %>% data.frame()
colnames(mean_traning_error) <- colnames(results_hybrid[[1]][[1]][[1]][[1]])
colnames(mean_test_error) <- colnames(results_hybrid[[1]][[1]][[1]][[1]])[-7]

mean_errors <- list()
for (i in 1:n_fam){
  for(k in 1:7){
    for (j in 1:n_cv){
      training_error[j,] <- results_hybrid[[i]][[1]][[j]][[k]][1,]
      test_error[j,] <- results_hybrid[[i]][[1]][[j]][[k]][2,]
    }
    mean_traning_error[k
                       ,] <- purrr::map_dbl(training_error,mean)
    mean_test_error[k,] <- purrr::map_dbl(test_error,mean)
  }
  mean_errors[i] <- list(cbind(mean_traning_error,
                               family = rep(colnames(cleansed_df_clean)[i+1], times = 7),
                               models = c("ARIMA","ETS","THETA","NEURAL","TBATS","STL","HYBRID")),
                         cbind(mean_test_error,
                               family = rep(colnames(cleansed_df_clean)[i+1], times = 7),
                               models = c("ARIMA","ETS","THETA","NEURAL","TBATS","STL","HYBRID"))) %>% list()
}

long_train_errors <- matrix(NA,nrow = 1, ncol = 9) %>% data.frame()
long_test_errors <- matrix(NA,nrow = 1, ncol = 8) %>% data.frame()
colnames(long_train_errors) <- colnames(mean_errors[[1]][[1]])
colnames(long_test_errors) <- colnames(mean_errors[[1]][[2]])

for(i in 1:n_fam){
  long_train_errors <- rbind(long_train_errors,mean_errors[[i]][[1]])
  long_test_errors <- rbind(long_test_errors,mean_errors[[i]][[2]])
}
long_test_errors <- long_test_errors %>% 
  tidyr::drop_na() %>% 
  tidyr::gather(key = measure, value = error, -c(family,models))

long_train_errors <- long_train_errors %>% 
  tidyr::drop_na() %>% 
  tidyr::gather(key = measure, value = error, -c(family,models))


#### arrange forecasts for excel - aggregate and put in long format ####

forecast_final <- matrix(NA, nrow=1,ncol = 6) %>% data.frame()

models  <-  c("ARIMA","ETS","THETA","NEURAL","TBATS","STL","HYBRID")
colnames(forecast_final) <- c("period","family","model","mean","lower","upper")

for(i in 1 :n_fam){
  for(j in 1:7){
    aux_final <- results_hybrid[[i]][[2]][[j]][,c(1,4,5)]
    aux_final <- cbind(period = rownames(results_hybrid[[i]][[2]][[j]]),
                       family = rep(colnames(cleansed_df_clean)[i+1],times = 24),
                       model = rep(models[j],times = 24),
                       aux_final)
    colnames(aux_final) <- c("period","family","model","mean","lower","upper")
    forecast_final <- rbind(forecast_final,aux_final)
  }  
}

forecast_final <- forecast_final %>% tidyr::drop_na()



results_hybrid[[4]]


#### create history and cleansed history for excel reporting ####

#Read data and change names for excel formats

long_test_errors$family  <- gsub(".", " ", long_test_errors$family, fixed = TRUE)
long_test_errors$family  <- as.factor(long_test_errors$family)
levels(long_test_errors$family)
library(plyr)
long_test_errors$family<- revalue(long_test_errors$family, c("LA Large Tablets   90G "="LA Large Tablets (>90G)",
                                                             "LA Large Tablets   90G _MT"="LA Large Tablets (>90G)_MT",
                                                             "LA Large Tablets   90G _TT"="LA Large Tablets (>90G)_TT",
                                                             "LA Small Tablets   40G "="LA Small Tablets (<40G)",
                                                             "LA Small Tablets   40G _MT"="LA Small Tablets_MT (<40G)",
                                                             "LA Small Tablets   40G _TT"="LA Small Tablets_TT (<40G)",
                                                             "LA Tita   Rhodesia"="LA Tita & Rhodesia",
                                                             "LA Puddings   Cakes" = "LA Puddings & Cakes",
                                                             "Clorets Gum Pellet   12S_FF_MT" = "Clorets Gum Pellet - 12S_FF_MT",
                                                             "Clorets Gum Pellet   12S_FF_TT" = "Clorets Gum Pellet - 12S_FF_TT",
                                                             "Clorets Gum Pellet   12S_FF_WHS" = "Clorets Gum Pellet - 12S_FF_WHS",
                                                             "Clorets Gum Pellet   2S_FF_MT" = "Clorets Gum Pellet - 2S_FF_MT",
                                                             "Clorets Gum Pellet   2S_FF_TT" = "Clorets Gum Pellet - 2S_FF_TT",
                                                             "Clorets Gum Pellet   2S_FF_WHS" = "Clorets Gum Pellet - 2S_FF_WHS",
                                                             "Clorets Gum Pellet   30S_FF_MT" = "Clorets Gum Pellet - 30S_FF_MT",
                                                             "Clorets Gum Pellet   30S_FF_TT" = "Clorets Gum Pellet - 30S_FF_TT",
                                                             "Clorets Gum Pellet   30S_FF_WHS" = "Clorets Gum Pellet - 30S_FF_WHS",
                                                             "Gelatin   Pouch_FF_MT" = "Gelatin - Pouch_FF_MT",
                                                             "Gelatin   Pouch_FF_TT" =  "Gelatin - Pouch_FF_TT",
                                                             "Gelatin   Pouch_FF_WHS" = "Gelatin - Pouch_FF_WHS",
                                                             "Halls Base   Display_FF_MT" = "Halls Base - Display_FF_MT",
                                                             "Halls Base   Display_FF_TT" = "Halls Base - Display_FF_TT",
                                                             "Halls Base   Display_FF_WHS" = "Halls Base - Display_FF_WHS",
                                                             "Halls Base   Creamy Display_FF_MT" = "Halls Base - Creamy Display_FF_MT",
                                                             "Halls Base   Creamy Display_FF_TT" = "Halls Base - Creamy Display_FF_TT",
                                                             "Halls Base   Creamy Display_FF_WHS" = "Halls Base - Creamy Display_FF_WHS",
                                                             "Oreo slug   Pack_FF_MT" = "Oreo slug - Pack_FF_MT",
                                                             "Oreo slug   Pack_FF_TT" = "Oreo slug - Pack_FF_TT",
                                                             "Oreo slug   Pack_FF_WHS" = "Oreo slug - Pack_FF_WHS",
                                                             "Trident Base Pellet   Xcare_FF_MT" = "Trident Base Pellet - Xcare_FF_MT",
                                                             "Trident Base Pellet   Xcare_FF_TT" = "Trident Base Pellet - Xcare_FF_TT",
                                                             "Trident Base Pellet   Xcare_FF_WHS" = "Trident Base Pellet - Xcare_FF_WHS",
                                                             "Trident Centre Filled Pellet   Box_FF_MT" = "Trident Centre Filled Pellet - Box_FF_MT", 
                                                             "Trident Centre Filled Pellet   Box_FF_TT" = "Trident Centre Filled Pellet - Box_FF_TT",
                                                             "Trident Centre Filled Pellet   Box_FF_WHS" = "Trident Centre Filled Pellet - Box_FF_WHS",
                                                             "Trident Slab   18S_FF_MT" = "Trident Slab - 18S_FF_MT",
                                                             "Trident Slab   18S_FF_TT" = "Trident Slab - 18S_FF_TT",
                                                             "Trident Slab   18S_FF_WHS" = "Trident Slab - 18S_FF_WHS",
                                                             "Trident Slab   6S_FF_MT" = "Trident Slab - 6S_FF_MT",
                                                             "Trident Slab   6S_FF_TT" = "Trident Slab - 6S_FF_TT",
                                                             "Trident Slab   6S_FF_WHS" = "Trident Slab - 6S_FF_WHS",
                                                             "Bubbaloo Candy   Lolipop_FF_MT" = "Bubbaloo Candy - Lolipop_FF_MT",
                                                             "Bubbaloo Candy   Lolipop_FF_TT" = "Bubbaloo Candy - Lolipop_FF_TT",
                                                             'Dentyne Base Pellet   Ice_FF_MT'= 'Dentyne Base Pellet - Ice_FF_MT',
                                                             'Dentyne Base Pellet   Ice_FF_TT'= 'Dentyne Base Pellet - Ice_FF_TT',
                                                             'Mantecol   Impulso_FF_MT'= 'Mantecol - Impulso_FF_MT',
                                                             'Mantecol   Impulso_FF_TT'= 'Mantecol - Impulso_FF_TT',
                                                             'Mantecol   Planificado_FF_MT'= 'Mantecol - Planificado_FF_MT',
                                                             'Mantecol   Planificado_FF_TT'='Mantecol - Planificado_FF_TT',
                                                             'Milka Large Tablets   Imported 100G_FF_MT' = 'Milka Large Tablets - Imported 100G_FF_MT',
                                                             'Milka Large Tablets   Imported 100G_FF_TT'= 'Milka Large Tablets - Imported 100G_FF_TT',
                                                             'Milka Large Tablets   Imported 300G_FF_MT'= 'Milka Large Tablets - Imported 300G_FF_MT',
                                                             'Milka Large Tablets   Imported 300G_FF_TT'= 'Milka Large Tablets - Imported 300G_FF_TT',
                                                             'Milka Large Tablets   Other_FF_MT'= 'Milka Large Tablets - Other_FF_MT',
                                                             'Milka Large Tablets   Other_FF_TT'= 'Milka Large Tablets - Other_FF_TT',
                                                             'Milka Medium Tablets   Other_FF_MT'= 'Milka Medium Tablets - Other_FF_MT',
                                                             'Milka Medium Tablets   Other_FF_TT'= 'Milka Medium Tablets - Other_FF_TT',
                                                             'Toblerone Large Tablets   More Than 100 Gr_FF_MT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_MT',
                                                             'Toblerone Large Tablets   More Than 100 Gr_FF_TT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_TT',
                                                             'Toblerone Large Tablets   Up To 100 Gr_FF_MT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_MT', 
                                                             'Toblerone Large Tablets   Up To 100 Gr_FF_TT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_TT',
                                                             'Trident Slab   Other_FF_MT'= 'Trident Slab - Other_FF_MT',
                                                             'Trident Slab   Other_FF_TT'= 'Trident Slab - Other_FF_TT',
                                                             "Gelatin   Light   Zero_FF_MT" = "Gelatin - Light / Zero_FF_MT",
                                                             "Milka Wafer   Chocopause_FF_TT"  = "Milka Wafer - Chocopause_FF_TT",
                                                             "Gelatin   Light   Zero_FF_TT" = "Gelatin - Light / Zero_FF_TT",
                                                             "Milka Wafer   Chocopause_FF_MT"  = "Milka Wafer - Chocopause_FF_MT",
                                                             "Halls Base   Single Piece_FF_TT" = "Halls Base - Single Piece_FF_TT",
                                                             "Halls Base   Single Piece_FF_MT" = "Halls Base - Single Piece_FF_MT",
                                                             "CDM Large Tablets   Other_FF_TT" = "CDM Large Tablets - Other_FF_TT",
                                                             "CDM Large Tablets   Other_FF_MT" = "CDM Large Tablets - Other_FF_MT"))

long_train_errors$family  <- gsub(".", " ", long_train_errors$family, fixed = TRUE)
long_train_errors$family  <- as.factor(long_train_errors$family)
levels(long_train_errors$family)
long_train_errors$family<- revalue(long_train_errors$family, c("LA Large Tablets   90G "="LA Large Tablets (>90G)",
                                                               "LA Large Tablets   90G _MT"="LA Large Tablets (>90G)_MT",
                                                               "LA Large Tablets   90G _TT"="LA Large Tablets (>90G)_TT",
                                                               "LA Small Tablets   40G "="LA Small Tablets (<40G)",
                                                               "LA Small Tablets   40G _MT"="LA Small Tablets_MT (<40G)",
                                                               "LA Small Tablets   40G _TT"="LA Small Tablets_TT (<40G)",
                                                               "LA Tita   Rhodesia"="LA Tita & Rhodesia",
                                                               "LA Puddings   Cakes" = "LA Puddings & Cakes",
                                                               "Clorets Gum Pellet   12S_FF_MT" = "Clorets Gum Pellet - 12S_FF_MT",
                                                               "Clorets Gum Pellet   12S_FF_TT" = "Clorets Gum Pellet - 12S_FF_TT",
                                                               "Clorets Gum Pellet   12S_FF_WHS" = "Clorets Gum Pellet - 12S_FF_WHS",
                                                               "Clorets Gum Pellet   2S_FF_MT" = "Clorets Gum Pellet - 2S_FF_MT",
                                                               "Clorets Gum Pellet   2S_FF_TT" = "Clorets Gum Pellet - 2S_FF_TT",
                                                               "Clorets Gum Pellet   2S_FF_WHS" = "Clorets Gum Pellet - 2S_FF_WHS",
                                                               "Clorets Gum Pellet   30S_FF_MT" = "Clorets Gum Pellet - 30S_FF_MT",
                                                               "Clorets Gum Pellet   30S_FF_TT" = "Clorets Gum Pellet - 30S_FF_TT",
                                                               "Clorets Gum Pellet   30S_FF_WHS" = "Clorets Gum Pellet - 30S_FF_WHS",
                                                               "Gelatin   Pouch_FF_MT" = "Gelatin - Pouch_FF_MT",
                                                               "Gelatin   Pouch_FF_TT" =  "Gelatin - Pouch_FF_TT",
                                                               "Gelatin   Pouch_FF_WHS" = "Gelatin - Pouch_FF_WHS",
                                                               "Halls Base   Display_FF_MT" = "Halls Base - Display_FF_MT",
                                                               "Halls Base   Display_FF_TT" = "Halls Base - Display_FF_TT",
                                                               "Halls Base   Display_FF_WHS" = "Halls Base - Display_FF_WHS",
                                                               "Halls Base   Creamy Display_FF_MT" = "Halls Base - Creamy Display_FF_MT",
                                                               "Halls Base   Creamy Display_FF_TT" = "Halls Base - Creamy Display_FF_TT",
                                                               "Halls Base   Creamy Display_FF_WHS" = "Halls Base - Creamy Display_FF_WHS",
                                                               "Oreo slug   Pack_FF_MT" = "Oreo slug - Pack_FF_MT",
                                                               "Oreo slug   Pack_FF_TT" = "Oreo slug - Pack_FF_TT",
                                                               "Oreo slug   Pack_FF_WHS" = "Oreo slug - Pack_FF_WHS",
                                                               "Trident Base Pellet   Xcare_FF_MT" = "Trident Base Pellet - Xcare_FF_MT",
                                                               "Trident Base Pellet   Xcare_FF_TT" = "Trident Base Pellet - Xcare_FF_TT",
                                                               "Trident Base Pellet   Xcare_FF_WHS" = "Trident Base Pellet - Xcare_FF_WHS",
                                                               "Trident Centre Filled Pellet   Box_FF_MT" = "Trident Centre Filled Pellet - Box_FF_MT", 
                                                               "Trident Centre Filled Pellet   Box_FF_TT" = "Trident Centre Filled Pellet - Box_FF_TT",
                                                               "Trident Centre Filled Pellet   Box_FF_WHS" = "Trident Centre Filled Pellet - Box_FF_WHS",
                                                               "Trident Slab   18S_FF_MT" = "Trident Slab - 18S_FF_MT",
                                                               "Trident Slab   18S_FF_TT" = "Trident Slab - 18S_FF_TT",
                                                               "Trident Slab   18S_FF_WHS" = "Trident Slab - 18S_FF_WHS",
                                                               "Trident Slab   6S_FF_MT" = "Trident Slab - 6S_FF_MT",
                                                               "Trident Slab   6S_FF_TT" = "Trident Slab - 6S_FF_TT",
                                                               "Trident Slab   6S_FF_WHS" = "Trident Slab - 6S_FF_WHS",
                                                               "Bubbaloo Candy   Lolipop_FF_MT" = "Bubbaloo Candy - Lolipop_FF_MT",
                                                               "Bubbaloo Candy   Lolipop_FF_TT" = "Bubbaloo Candy - Lolipop_FF_TT",
                                                               'Dentyne Base Pellet   Ice_FF_MT'= 'Dentyne Base Pellet - Ice_FF_MT',
                                                               'Dentyne Base Pellet   Ice_FF_TT'= 'Dentyne Base Pellet - Ice_FF_TT',
                                                               'Mantecol   Impulso_FF_MT'= 'Mantecol - Impulso_FF_MT',
                                                               'Mantecol   Impulso_FF_TT'= 'Mantecol - Impulso_FF_TT',
                                                               'Mantecol   Planificado_FF_MT'= 'Mantecol - Planificado_FF_MT',
                                                               'Mantecol   Planificado_FF_TT'='Mantecol - Planificado_FF_TT',
                                                               'Milka Large Tablets   Imported 100G_FF_MT' = 'Milka Large Tablets - Imported 100G_FF_MT',
                                                               'Milka Large Tablets   Imported 100G_FF_TT'= 'Milka Large Tablets - Imported 100G_FF_TT',
                                                               'Milka Large Tablets   Imported 300G_FF_MT'= 'Milka Large Tablets - Imported 300G_FF_MT',
                                                               'Milka Large Tablets   Imported 300G_FF_TT'= 'Milka Large Tablets - Imported 300G_FF_TT',
                                                               'Milka Large Tablets   Other_FF_MT'= 'Milka Large Tablets - Other_FF_MT',
                                                               'Milka Large Tablets   Other_FF_TT'= 'Milka Large Tablets - Other_FF_TT',
                                                               'Milka Medium Tablets   Other_FF_MT'= 'Milka Medium Tablets - Other_FF_MT',
                                                               'Milka Medium Tablets   Other_FF_TT'= 'Milka Medium Tablets - Other_FF_TT',
                                                               'Toblerone Large Tablets   More Than 100 Gr_FF_MT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_MT',
                                                               'Toblerone Large Tablets   More Than 100 Gr_FF_TT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_TT',
                                                               'Toblerone Large Tablets   Up To 100 Gr_FF_MT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_MT', 
                                                               'Toblerone Large Tablets   Up To 100 Gr_FF_TT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_TT',
                                                               'Trident Slab   Other_FF_MT'= 'Trident Slab - Other_FF_MT',
                                                               'Trident Slab   Other_FF_TT'= 'Trident Slab - Other_FF_TT',
                                                               "Gelatin   Light   Zero_FF_MT" = "Gelatin - Light / Zero_FF_MT",
                                                               "Milka Wafer   Chocopause_FF_TT"  = "Milka Wafer - Chocopause_FF_TT",
                                                               "Gelatin   Light   Zero_FF_TT" = "Gelatin - Light / Zero_FF_TT",
                                                               "Milka Wafer   Chocopause_FF_MT"  = "Milka Wafer - Chocopause_FF_MT",
                                                               "Halls Base   Single Piece_FF_TT" = "Halls Base - Single Piece_FF_TT",
                                                               "Halls Base   Single Piece_FF_MT" = "Halls Base - Single Piece_FF_MT",
                                                               "CDM Large Tablets   Other_FF_TT" = "CDM Large Tablets - Other_FF_TT",
                                                               "CDM Large Tablets   Other_FF_MT" = "CDM Large Tablets - Other_FF_MT"))

forecast_final$family  <- gsub(".", " ", forecast_final$family, fixed = TRUE)
forecast_final$family  <- as.factor(forecast_final$family)
levels(forecast_final$family)
forecast_final$family<- revalue(forecast_final$family, c("LA Large Tablets   90G "="LA Large Tablets (>90G)",
                                                         "LA Large Tablets   90G _MT"="LA Large Tablets (>90G)_MT",
                                                         "LA Large Tablets   90G _TT"="LA Large Tablets (>90G)_TT",
                                                         "LA Small Tablets   40G "="LA Small Tablets (<40G)",
                                                         "LA Small Tablets   40G _MT"="LA Small Tablets_MT (<40G)",
                                                         "LA Small Tablets   40G _TT"="LA Small Tablets_TT (<40G)",
                                                         "LA Tita   Rhodesia"="LA Tita & Rhodesia",
                                                         "LA Puddings   Cakes" = "LA Puddings & Cakes",
                                                         "Clorets Gum Pellet   12S_FF_MT" = "Clorets Gum Pellet - 12S_FF_MT",
                                                         "Clorets Gum Pellet   12S_FF_TT" = "Clorets Gum Pellet - 12S_FF_TT",
                                                         "Clorets Gum Pellet   12S_FF_WHS" = "Clorets Gum Pellet - 12S_FF_WHS",
                                                         "Clorets Gum Pellet   2S_FF_MT" = "Clorets Gum Pellet - 2S_FF_MT",
                                                         "Clorets Gum Pellet   2S_FF_TT" = "Clorets Gum Pellet - 2S_FF_TT",
                                                         "Clorets Gum Pellet   2S_FF_WHS" = "Clorets Gum Pellet - 2S_FF_WHS",
                                                         "Clorets Gum Pellet   30S_FF_MT" = "Clorets Gum Pellet - 30S_FF_MT",
                                                         "Clorets Gum Pellet   30S_FF_TT" = "Clorets Gum Pellet - 30S_FF_TT",
                                                         "Clorets Gum Pellet   30S_FF_WHS" = "Clorets Gum Pellet - 30S_FF_WHS",
                                                         "Gelatin   Pouch_FF_MT" = "Gelatin - Pouch_FF_MT",
                                                         "Gelatin   Pouch_FF_TT" =  "Gelatin - Pouch_FF_TT",
                                                         "Gelatin   Pouch_FF_WHS" = "Gelatin - Pouch_FF_WHS",
                                                         "Halls Base   Display_FF_MT" = "Halls Base - Display_FF_MT",
                                                         "Halls Base   Display_FF_TT" = "Halls Base - Display_FF_TT",
                                                         "Halls Base   Display_FF_WHS" = "Halls Base - Display_FF_WHS",
                                                         "Halls Base   Creamy Display_FF_MT" = "Halls Base - Creamy Display_FF_MT",
                                                         "Halls Base   Creamy Display_FF_TT" = "Halls Base - Creamy Display_FF_TT",
                                                         "Halls Base   Creamy Display_FF_WHS" = "Halls Base - Creamy Display_FF_WHS",
                                                         "Oreo slug   Pack_FF_MT" = "Oreo slug - Pack_FF_MT",
                                                         "Oreo slug   Pack_FF_TT" = "Oreo slug - Pack_FF_TT",
                                                         "Oreo slug   Pack_FF_WHS" = "Oreo slug - Pack_FF_WHS",
                                                         "Trident Base Pellet   Xcare_FF_MT" = "Trident Base Pellet - Xcare_FF_MT",
                                                         "Trident Base Pellet   Xcare_FF_TT" = "Trident Base Pellet - Xcare_FF_TT",
                                                         "Trident Base Pellet   Xcare_FF_WHS" = "Trident Base Pellet - Xcare_FF_WHS",
                                                         "Trident Centre Filled Pellet   Box_FF_MT" = "Trident Centre Filled Pellet - Box_FF_MT", 
                                                         "Trident Centre Filled Pellet   Box_FF_TT" = "Trident Centre Filled Pellet - Box_FF_TT",
                                                         "Trident Centre Filled Pellet   Box_FF_WHS" = "Trident Centre Filled Pellet - Box_FF_WHS",
                                                         "Trident Slab   18S_FF_MT" = "Trident Slab - 18S_FF_MT",
                                                         "Trident Slab   18S_FF_TT" = "Trident Slab - 18S_FF_TT",
                                                         "Trident Slab   18S_FF_WHS" = "Trident Slab - 18S_FF_WHS",
                                                         "Trident Slab   6S_FF_MT" = "Trident Slab - 6S_FF_MT",
                                                         "Trident Slab   6S_FF_TT" = "Trident Slab - 6S_FF_TT",
                                                         "Trident Slab   6S_FF_WHS" = "Trident Slab - 6S_FF_WHS",
                                                         "Bubbaloo Candy   Lolipop_FF_MT" = "Bubbaloo Candy - Lolipop_FF_MT",
                                                         "Bubbaloo Candy   Lolipop_FF_TT" = "Bubbaloo Candy - Lolipop_FF_TT",
                                                         'Dentyne Base Pellet   Ice_FF_MT'= 'Dentyne Base Pellet - Ice_FF_MT',
                                                           'Dentyne Base Pellet   Ice_FF_TT'= 'Dentyne Base Pellet - Ice_FF_TT',
                                                           'Mantecol   Impulso_FF_MT'= 'Mantecol - Impulso_FF_MT',
                                                           'Mantecol   Impulso_FF_TT'= 'Mantecol - Impulso_FF_TT',
                                                           'Mantecol   Planificado_FF_MT'= 'Mantecol - Planificado_FF_MT',
                                                           'Mantecol   Planificado_FF_TT'='Mantecol - Planificado_FF_TT',
                                                           'Milka Large Tablets   Imported 100G_FF_MT' = 'Milka Large Tablets - Imported 100G_FF_MT',
                                                           'Milka Large Tablets   Imported 100G_FF_TT'= 'Milka Large Tablets - Imported 100G_FF_TT',
                                                           'Milka Large Tablets   Imported 300G_FF_MT'= 'Milka Large Tablets - Imported 300G_FF_MT',
                                                           'Milka Large Tablets   Imported 300G_FF_TT'= 'Milka Large Tablets - Imported 300G_FF_TT',
                                                           'Milka Large Tablets   Other_FF_MT'= 'Milka Large Tablets - Other_FF_MT',
                                                           'Milka Large Tablets   Other_FF_TT'= 'Milka Large Tablets - Other_FF_TT',
                                                           'Milka Medium Tablets   Other_FF_MT'= 'Milka Medium Tablets - Other_FF_MT',
                                                           'Milka Medium Tablets   Other_FF_TT'= 'Milka Medium Tablets - Other_FF_TT',
                                                           'Toblerone Large Tablets   More Than 100 Gr_FF_MT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_MT',
                                                           'Toblerone Large Tablets   More Than 100 Gr_FF_TT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_TT',
                                                           'Toblerone Large Tablets   Up To 100 Gr_FF_MT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_MT', 
                                                           'Toblerone Large Tablets   Up To 100 Gr_FF_TT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_TT',
                                                           'Trident Slab   Other_FF_MT'= 'Trident Slab - Other_FF_MT',
                                                           'Trident Slab   Other_FF_TT'= 'Trident Slab - Other_FF_TT',
                                                         "Gelatin   Light   Zero_FF_MT" = "Gelatin - Light / Zero_FF_MT",
                                                         "Milka Wafer   Chocopause_FF_TT"  = "Milka Wafer - Chocopause_FF_TT",
                                                         "Gelatin   Light   Zero_FF_TT" = "Gelatin - Light / Zero_FF_TT",
                                                         "Milka Wafer   Chocopause_FF_MT"  = "Milka Wafer - Chocopause_FF_MT",
                                                         "Halls Base   Single Piece_FF_TT" = "Halls Base - Single Piece_FF_TT",
                                                         "Halls Base   Single Piece_FF_MT" = "Halls Base - Single Piece_FF_MT",
                                                         "CDM Large Tablets   Other_FF_TT" = "CDM Large Tablets - Other_FF_TT",
                                                         "CDM Large Tablets   Other_FF_MT" = "CDM Large Tablets - Other_FF_MT"
                                                         
))


test_errors <- spread(long_test_errors, models, error)
train_errors <- spread(long_train_errors, models, error)
forecast_final2 <- spread(forecast_final[,-c(5,6)], model, mean)

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

forecast_final2$year <-  substrRight(forecast_final2$period, 4)

forecast_final2$month <- substr(forecast_final2$period, 1,3) 

forecast_final2$month_num <-substrRight(paste0(0,match(forecast_final2$month,month.abb)),2)
forecast_final2$period2 <- paste0(forecast_final2$year,forecast_final2$month_num)

test_errors<- test_errors[,c(1,2,3,4,9,6,8,7,5)]
train_errors<- train_errors[,c(1,2,3,4,9,6,8,7,5)]
forecast_final2<- forecast_final2[,c(1,2,3,4,9,6,8,7,5,10,11,12,13)]

levels(forecast_final2$family)

if (BU == "YES") {
  write.csv(test_errors, "Deliveries\\tool\\Input\\test_errors_BU.csv")
  write.csv(train_errors, "Deliveries\\tool\\Input\\train_errors_BU.csv")
  write.csv(forecast_final2, "Deliveries\\tool\\Input\\forecast_final_BU.csv")
  
}else{write.csv(test_errors, "Output\\test_errors.csv")
  write.csv(train_errors, "Output\\train_errors.csv")
  write.csv(forecast_final2, "Output\\forecast_final.csv")}



#################################################################
#################################################################
#BOTTOM UP


BU<- forecast_final2[order(forecast_final2$period2, decreasing = F),] 
BU_E<- test_errors[order(test_errors$measure, decreasing = F),] 

ARIMA_points <- 0
ETS_points <- 0
HYBRID_points <- 0
NEURAL_points <- 0
STL_points <- 0
TBATS_points <- 0
THETA_points <- 0
eleccion_mod <- list()
eleccion_fam <- list()
for (k in 1:length(levels(BU$family))) {
  subs1 <- subset(BU, levels(BU$family) == levels(BU$family)[k])
  subs2 <- subset(BU_E, levels(BU_E$family) == levels(BU_E$family)[k])
  t1 <- summaryBy(. ~ family,data =subs1[1:12,2:9] ,FUN=sum)
  t2 <- summaryBy(. ~ family,data =subs1[13:24,2:9] ,FUN=sum)
  ARIMA_change <- abs((t1$ARIMA.sum/t2$ARIMA.sum)-1)
  ETS_change <- abs((t1$ETS.sum/t2$ETS.sum)-1)
  HYBRID_change <- abs((t1$HYBRID.sum/t2$HYBRID.sum)-1)
  NEURAL_change <- abs((t1$NEURAL.sum/t2$NEURAL.sum)-1)
  STL_change <- abs((t1$STL.sum/t2$STL.sum)-1)
  TBATS_change <- abs((t1$TBATS.sum/t2$TBATS.sum)-1)
  THETA_change <-abs((t1$THETA.sum/t2$THETA.sum)-1)
  mins<- min(ARIMA_change,ETS_change,HYBRID_change,NEURAL_change,STL_change,TBATS_change,THETA_change)
  maxs<- max(ARIMA_change,ETS_change,HYBRID_change,NEURAL_change,STL_change,TBATS_change,THETA_change)
  ARIMA_TREND <- ((ARIMA_change-mins)/(maxs-mins))*-3
  ETS_TREND <- ((ETS_change-mins)/(maxs-mins))*-3
  HYBRID_TREND <- ((HYBRID_change-mins)/(maxs-mins))*-3
  NEURAL_TREND <- ((NEURAL_change-mins)/(maxs-mins))*-3
  STL_TREND <- ((STL_change-mins)/(maxs-mins))*-3
  TBATS_TREND <- ((TBATS_change-mins)/(maxs-mins))*-3
  THETA_TREND <-((THETA_change-mins)/(maxs-mins))*-3
  min_MAE<- colnames(subs2)[which(abs(subs2[1,3:9]) == min(abs(subs2[1,3:9])))+2]
  min_MAPE<- colnames(subs2)[which(abs(subs2[2,3:9]) == min(abs(subs2[2,3:9])))+2]
  min_MASE<- colnames(subs2)[which(abs(subs2[3,3:9]) == min(abs(subs2[3,3:9])))+2]
  min_ME<- colnames(subs2)[which(abs(subs2[4,3:9]) == min(abs(subs2[4,3:9])))+2]
  min_MPE<- colnames(subs2)[which(abs(subs2[5,3:9]) == min(abs(subs2[5,3:9])))+2]
  min_RMSE<- colnames(subs2)[which(abs(subs2[6,3:9]) == min(abs(subs2[6,3:9])))+2]
  
  for (i in 1:length(colnames(subs2))-2) {
    if (colnames(subs2)[i+2] == "ARIMA" & min_MAE == "ARIMA") {
      ARIMA_points <- 1
    }else if (colnames(subs2)[i+2] == "ARIMA" & min_MAPE == "ARIMA") {
      ARIMA_points <- ARIMA_points + 3
    }else if (colnames(subs2)[i+2] == "ARIMA" & min_MASE == "ARIMA") {
      ARIMA_points <- ARIMA_points + 1
    }else if (colnames(subs2)[i+2] == "ARIMA" & min_ME == "ARIMA") {
      ARIMA_points <- ARIMA_points + 1
    }else if (colnames(subs2)[i+2] == "ARIMA" & min_MPE == "ARIMA") {
      ARIMA_points <- ARIMA_points + 2
    }else if (colnames(subs2)[i+2] == "ARIMA" & min_RMSE == "ARIMA") {
      ARIMA_points <- ARIMA_points + 1
    }else{ARIMA_points <-ARIMA_TREND}
    
    if (ARIMA_points != ARIMA_TREND) {
      ARIMA_points = ARIMA_points + ARIMA_TREND
    }
    
    if (colnames(subs2)[i+2] == "ETS" & min_MAE == "ETS") {
      ETS_points <- 1
    }else if (colnames(subs2)[i+2] == "ETS" & min_MAPE == "ETS") {
      ETS_points <- ETS_points + 3
    }else if (colnames(subs2)[i+2] == "ETS" & min_MASE == "ETS") {
      ETS_points <- ETS_points + 1
    }else if (colnames(subs2)[i+2] == "ETS" & min_ME == "ETS") {
      ETS_points <- ETS_points + 1
    }else if (colnames(subs2)[i+2] == "ETS" & min_MPE == "ETS") {
      ETS_points <- ETS_points + 2
    }else if (colnames(subs2)[i+2] == "ETS" & min_RMSE == "ETS") {
      ETS_points <- ETS_points + 1
    }else{ETS_points <-ETS_TREND}
    
    if (ETS_points != ETS_TREND) {
      ETS_points = ETS_points + ETS_TREND
    }
    
    if (colnames(subs2)[i+2] == "HYBRID" & min_MAE == "HYBRID") {
      HYBRID_points <- 1
    }else if (colnames(subs2)[i+2] == "HYBRID" & min_MAPE == "HYBRID") {
      HYBRID_points <- HYBRID_points + 3
    }else if (colnames(subs2)[i+2] == "HYBRID" & min_MASE == "HYBRID") {
      HYBRID_points <- HYBRID_points + 1
    }else if (colnames(subs2)[i+2] == "HYBRID" & min_ME == "HYBRID") {
      HYBRID_points <- HYBRID_points + 1
    }else if (colnames(subs2)[i+2] == "HYBRID" & min_MPE == "HYBRID") {
      HYBRID_points <- HYBRID_points + 2
    }else if (colnames(subs2)[i+2] == "HYBRID" & min_RMSE == "HYBRID") {
      HYBRID_points <- HYBRID_points + 1
    }else{HYBRID_points <-HYBRID_TREND}
    
    if (HYBRID_points != HYBRID_TREND) {
      HYBRID_points = HYBRID_points + HYBRID_TREND
    }
    
    if (colnames(subs2)[i+2] == "NEURAL" & min_MAE == "NEURAL") {
      NEURAL_points <- 1
    }else if (colnames(subs2)[i+2] == "NEURAL" & min_MAPE == "NEURAL") {
      NEURAL_points <- NEURAL_points + 3
    }else if (colnames(subs2)[i+2] == "NEURAL" & min_MASE == "NEURAL") {
      NEURAL_points <- NEURAL_points + 1
    }else if (colnames(subs2)[i+2] == "NEURAL" & min_ME == "NEURAL") {
      NEURAL_points <- NEURAL_points + 1
    }else if (colnames(subs2)[i+2] == "NEURAL" & min_MPE == "NEURAL") {
      NEURAL_points <- NEURAL_points + 2
    }else if (colnames(subs2)[i+2] == "NEURAL" & min_RMSE == "NEURAL") {
      NEURAL_points <- NEURAL_points + 1
    }else{NEURAL_points <- NEURAL_TREND}
    
    if (NEURAL_points != NEURAL_TREND) {
      NEURAL_points = NEURAL_points + NEURAL_TREND
    }
    
    if (colnames(subs2)[i+2] == "STL" & min_MAE == "STL") {
      STL_points <- 1
    }else if (colnames(subs2)[i+2] == "STL" & min_MAPE == "STL") {
      STL_points <- STL_points + 3
    }else if (colnames(subs2)[i+2] == "STL" & min_MASE == "STL") {
      STL_points <- STL_points + 1
    }else if (colnames(subs2)[i+2] == "STL" & min_ME == "STL") {
      STL_points <- STL_points + 1
    }else if (colnames(subs2)[i+2] == "STL" & min_MPE == "STL") {
      STL_points <- STL_points + 2
    }else if (colnames(subs2)[i+2] == "STL" & min_RMSE == "STL") {
      STL_points <- STL_points + 1
    }else{STL_points <-STL_TREND}
    
    if (STL_points != STL_TREND) {
      STL_points = STL_points + STL_TREND
    }
    
    if (colnames(subs2)[i+2] == "TBATS" & min_MAE == "TBATS") {
      TBATS_points <- 1
    }else if (colnames(subs2)[i+2] == "TBATS" & min_MAPE == "TBATS") {
      TBATS_points <- TBATS_points + 3
    }else if (colnames(subs2)[i+2] == "TBATS" & min_MASE == "TBATS") {
      TBATS_points <- TBATS_points + 1
    }else if (colnames(subs2)[i+2] == "TBATS" & min_ME == "TBATS") {
      TBATS_points <- TBATS_points + 1
    }else if (colnames(subs2)[i+2] == "TBATS" & min_MPE == "TBATS") {
      TBATS_points <- TBATS_points + 2
    }else if (colnames(subs2)[i+2] == "TBATS" & min_RMSE == "TBATS") {
      TBATS_points <- TBATS_points + 1
    }else{TBATS_points <-TBATS_TREND}
    
    if (TBATS_points != TBATS_TREND) {
      TBATS_points = TBATS_points + TBATS_TREND
    }
    
    if (colnames(subs2)[i+2] == "THETA" & min_MAE == "THETA") {
      THETA_points <- 1
    }else if (colnames(subs2)[i+2] == "THETA" & min_MAPE == "THETA") {
      THETA_points <- THETA_points + 3
    }else if (colnames(subs2)[i+2] == "THETA" & min_MASE == "THETA") {
      THETA_points <- THETA_points + 1
    }else if (colnames(subs2)[i+2] == "THETA" & min_ME == "THETA") {
      THETA_points <- THETA_points + 1
    }else if (colnames(subs2)[i+2] == "THETA" & min_MPE == "THETA") {
      THETA_points <- THETA_points + 2
    }else if (colnames(subs2)[i+2] == "THETA" & min_RMSE == "THETA") {
      THETA_points <- THETA_points + 1
    }else{THETA_points <-THETA_TREND}
    
    if (THETA_points != THETA_TREND) {
      THETA_points = THETA_points + THETA_TREND
    }
    #escoger max y hacer count y escoger el de mayor puntos
    max_points<- max(ARIMA_points,ETS_points,HYBRID_points,NEURAL_points,STL_points,TBATS_points,THETA_points)
    if (max_points == ARIMA_points) {
      eleccion_mod[k] <- "ARIMA"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }else if (max_points == ETS_points) {
      eleccion_mod[k] <- "ETS"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }else if (max_points == HYBRID_points) {
      eleccion_mod[k] <- "HYBRID"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }else if (max_points == NEURAL_points) {
      eleccion_mod[k] <- "NEURAL"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }else if (max_points == STL_points) {
      eleccion_mod[k] <- "STL"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }else if (max_points == TBATS_points) {
      eleccion_mod[k] <- "TBATS"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }else if (max_points == THETA_points) {
      eleccion_mod[k] <- "THETA"
      eleccion_fam[k] <- as.character(subs1$family[1])
    }
    
  }
}

#if cte vacio entonces corre otra cosa ara sacar el df sino esto de abajo.
#WARNING IS OK
modelos_elegidos<- flattenlist(eleccion_mod)
claves<- flattenlist(eleccion_fam)

bind1 <- NULL
data_long <- gather(forecast_final2[,c(2:9,13)], models, fcast, ARIMA:HYBRID, factor_key=TRUE)

for (i in 1:length(levels(data_long$family))) {
  subs1 <- subset(data_long, claves[[i]] == data_long$family)
  subs2 <- subset(subs1, as.character(subs1$models) == modelos_elegidos[[i]])
  bind1<- bind_rows(subs2,bind1)
  
}

bind2 <- summaryBy(fcast ~ family  + period2 ,data =bind1 ,FUN=sum)
for (i in 1:length(bind2$family)) {
  if (bind2$fcast.sum[i] < 0) {
    bind2$fcast.sum[i] <- 0
  }
}

x = list()
for (i in 1:length(levels(bind2$family))) {
  subs1 <- subset(bind2, bind2$family == levels(bind2$family)[i])
  if (length(subs1$fcast.sum[subs1$fcast.sum!=0])/length(subs1$fcast.sum)<.5) {
    x[i] <- subs1$family
  }
}



#if cte exists desde antes(history)
if (length(cte)>0) {
  cte <- c(cte,colnames(cleansed_df)[1])
  specs <- cleansed_df[ , which(names(cleansed_df) %in% cte)]
  MA <- rollmean(specs[,-1], 6)
  MA <-MA[1:24,]
  MA <- as.data.frame(MA)
  MA$period2<- unique(as.numeric(BU$period2))
  colnames(specs)[1] <- "period2"
  ctes_MA<- rbind(MA,specs)
  ctes_MA$period2 <- as.character(ctes_MA$period2)
}else{ctes_MA = NULL}

cleansed_df_long$Family  <- gsub(".", " ", cleansed_df_long$Family, fixed = TRUE)
cleansed_df_long$Family  <- as.factor(cleansed_df_long$Family)
cleansed_df_long$Family<- revalue(cleansed_df_long$Family, c("LA Large Tablets   90G "="LA Large Tablets (>90G)",
                                                             "LA Large Tablets   90G _MT"="LA Large Tablets (>90G)_MT",
                                                             "LA Large Tablets   90G _TT"="LA Large Tablets (>90G)_TT",
                                                             "LA Small Tablets   40G "="LA Small Tablets (<40G)",
                                                             "LA Small Tablets   40G _MT"="LA Small Tablets_MT (<40G)",
                                                             "LA Small Tablets   40G _TT"="LA Small Tablets_TT (<40G)",
                                                             "LA Tita   Rhodesia"="LA Tita & Rhodesia",
                                                             "LA Puddings   Cakes" = "LA Puddings & Cakes",
                                                             "Clorets Gum Pellet   12S_FF_MT" = "Clorets Gum Pellet - 12S_FF_MT",
                                                             "Clorets Gum Pellet   12S_FF_TT" = "Clorets Gum Pellet - 12S_FF_TT",
                                                             "Clorets Gum Pellet   12S_FF_WHS" = "Clorets Gum Pellet - 12S_FF_WHS",
                                                             "Clorets Gum Pellet   2S_FF_MT" = "Clorets Gum Pellet - 2S_FF_MT",
                                                             "Clorets Gum Pellet   2S_FF_TT" = "Clorets Gum Pellet - 2S_FF_TT",
                                                             "Clorets Gum Pellet   2S_FF_WHS" = "Clorets Gum Pellet - 2S_FF_WHS",
                                                             "Clorets Gum Pellet   30S_FF_MT" = "Clorets Gum Pellet - 30S_FF_MT",
                                                             "Clorets Gum Pellet   30S_FF_TT" = "Clorets Gum Pellet - 30S_FF_TT",
                                                             "Clorets Gum Pellet   30S_FF_WHS" = "Clorets Gum Pellet - 30S_FF_WHS",
                                                             "Gelatin   Pouch_FF_MT" = "Gelatin - Pouch_FF_MT",
                                                             "Gelatin   Pouch_FF_TT" =  "Gelatin - Pouch_FF_TT",
                                                             "Gelatin   Pouch_FF_WHS" = "Gelatin - Pouch_FF_WHS",
                                                             "Halls Base   Display_FF_MT" = "Halls Base - Display_FF_MT",
                                                             "Halls Base   Display_FF_TT" = "Halls Base - Display_FF_TT",
                                                             "Halls Base   Display_FF_WHS" = "Halls Base - Display_FF_WHS",
                                                             "Halls Base   Creamy Display_FF_MT" = "Halls Base - Creamy Display_FF_MT",
                                                             "Halls Base   Creamy Display_FF_TT" = "Halls Base - Creamy Display_FF_TT",
                                                             "Halls Base   Creamy Display_FF_WHS" = "Halls Base - Creamy Display_FF_WHS",
                                                             "Oreo slug   Pack_FF_MT" = "Oreo slug - Pack_FF_MT",
                                                             "Oreo slug   Pack_FF_TT" = "Oreo slug - Pack_FF_TT",
                                                             "Oreo slug   Pack_FF_WHS" = "Oreo slug - Pack_FF_WHS",
                                                             "Trident Base Pellet   Xcare_FF_MT" = "Trident Base Pellet - Xcare_FF_MT",
                                                             "Trident Base Pellet   Xcare_FF_TT" = "Trident Base Pellet - Xcare_FF_TT",
                                                             "Trident Base Pellet   Xcare_FF_WHS" = "Trident Base Pellet - Xcare_FF_WHS",
                                                             "Trident Centre Filled Pellet   Box_FF_MT" = "Trident Centre Filled Pellet - Box_FF_MT", 
                                                             "Trident Centre Filled Pellet   Box_FF_TT" = "Trident Centre Filled Pellet - Box_FF_TT",
                                                             "Trident Centre Filled Pellet   Box_FF_WHS" = "Trident Centre Filled Pellet - Box_FF_WHS",
                                                             "Trident Slab   18S_FF_MT" = "Trident Slab - 18S_FF_MT",
                                                             "Trident Slab   18S_FF_TT" = "Trident Slab - 18S_FF_TT",
                                                             "Trident Slab   18S_FF_WHS" = "Trident Slab - 18S_FF_WHS",
                                                             "Trident Slab   6S_FF_MT" = "Trident Slab - 6S_FF_MT",
                                                             "Trident Slab   6S_FF_TT" = "Trident Slab - 6S_FF_TT",
                                                             "Trident Slab   6S_FF_WHS" = "Trident Slab - 6S_FF_WHS",
                                                             "Bubbaloo Candy   Lolipop_FF_MT" = "Bubbaloo Candy - Lolipop_FF_MT",
                                                             "Bubbaloo Candy   Lolipop_FF_TT" = "Bubbaloo Candy - Lolipop_FF_TT",
                                                             'Dentyne Base Pellet   Ice_FF_MT'= 'Dentyne Base Pellet - Ice_FF_MT',
                                                             'Dentyne Base Pellet   Ice_FF_TT'= 'Dentyne Base Pellet - Ice_FF_TT',
                                                             'Mantecol   Impulso_FF_MT'= 'Mantecol - Impulso_FF_MT',
                                                             'Mantecol   Impulso_FF_TT'= 'Mantecol - Impulso_FF_TT',
                                                             'Mantecol   Planificado_FF_MT'= 'Mantecol - Planificado_FF_MT',
                                                             'Mantecol   Planificado_FF_TT'='Mantecol - Planificado_FF_TT',
                                                             'Milka Large Tablets   Imported 100G_FF_MT' = 'Milka Large Tablets - Imported 100G_FF_MT',
                                                             'Milka Large Tablets   Imported 100G_FF_TT'= 'Milka Large Tablets - Imported 100G_FF_TT',
                                                             'Milka Large Tablets   Imported 300G_FF_MT'= 'Milka Large Tablets - Imported 300G_FF_MT',
                                                             'Milka Large Tablets   Imported 300G_FF_TT'= 'Milka Large Tablets - Imported 300G_FF_TT',
                                                             'Milka Large Tablets   Other_FF_MT'= 'Milka Large Tablets - Other_FF_MT',
                                                             'Milka Large Tablets   Other_FF_TT'= 'Milka Large Tablets - Other_FF_TT',
                                                             'Milka Medium Tablets   Other_FF_MT'= 'Milka Medium Tablets - Other_FF_MT',
                                                             'Milka Medium Tablets   Other_FF_TT'= 'Milka Medium Tablets - Other_FF_TT',
                                                             'Toblerone Large Tablets   More Than 100 Gr_FF_MT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_MT',
                                                             'Toblerone Large Tablets   More Than 100 Gr_FF_TT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_TT',
                                                             'Toblerone Large Tablets   Up To 100 Gr_FF_MT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_MT', 
                                                             'Toblerone Large Tablets   Up To 100 Gr_FF_TT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_TT',
                                                             'Trident Slab   Other_FF_MT'= 'Trident Slab - Other_FF_MT',
                                                             'Trident Slab   Other_FF_TT'= 'Trident Slab - Other_FF_TT',
                                                             "Gelatin   Light   Zero_FF_MT" = "Gelatin - Light / Zero_FF_MT",
                                                             "Milka Wafer   Chocopause_FF_TT"  = "Milka Wafer - Chocopause_FF_TT",
                                                             "Gelatin   Light   Zero_FF_TT" = "Gelatin - Light / Zero_FF_TT",
                                                             "Milka Wafer   Chocopause_FF_MT"  = "Milka Wafer - Chocopause_FF_MT",
                                                             "Halls Base   Single Piece_FF_TT" = "Halls Base - Single Piece_FF_TT",
                                                             "Halls Base   Single Piece_FF_MT" = "Halls Base - Single Piece_FF_MT",
                                                             "CDM Large Tablets   Other_FF_TT" = "CDM Large Tablets - Other_FF_TT",
                                                             "CDM Large Tablets   Other_FF_MT" = "CDM Large Tablets - Other_FF_MT"))

cleansed_df_long <- cleansed_df_long %>% filter(Family != "A_DUMMY")
cleansed_df_long <- droplevels(cleansed_df_long)
cleansed_df_long$Family <- as.factor(cleansed_df_long$Family)
#si el resultado del fcast es cte 
count = 0
for (i in 1:length(levels(bind2$family))) {
  subs1 <-  subset(bind2, bind2$family == levels(bind2$family)[i])
  subs2 <-  subset(cleansed_df_long, cleansed_df_long$Family == levels(bind2$family)[i])
  if (round(sum(subs1$fcast.sum)/24,3) - round(subs1$fcast.sum[1],3) == 0) {
    count = count +1
    MA <- rollmean(subs2$SI, 6)
    MA <- MA[1:24]
    MA <- as.data.frame(MA)
    MA$period2 <- unique(as.numeric(BU$period2))
    colnames(MA)[1] <- as.character(subs1$family[1])
    MA$period2 <-  as.character(MA$period2)
    if (count == 1) {
      MA2 = MA
      
    }else{MA2<- merge(MA,MA2)}
  }
}
if (length(MA2$period2)==0) {
  MA = NULL
}else{MA = MA2}


if (is.null(ctes_MA)==F & is.null(MA)==F) {
  ctes_merge <-merge(ctes_MA,MA)
  forecast_final_BU <- spread(bind2, family, fcast.sum)
  forecast_final_BU_ctes <- merge(ctes_merge, forecast_final_BU, by = "period2")
}else if (is.null(ctes_MA)==F & is.null(MA)==T) {
  forecast_final_BU <- spread(bind2, family, fcast.sum)
  forecast_final_BU_ctes <- merge(ctes_MA, forecast_final_BU, by = "period2")
}else if (is.null(ctes_MA)==T & is.null(MA)==F) {
  forecast_final_BU <- spread(bind2, family, fcast.sum)
  forecast_final_BU_ctes <- merge(MA, forecast_final_BU, by = "period2")
}else if (is.null(ctes_MA)==T & is.null(MA)==T) {
  forecast_final_BU_ctes <- spread(bind2, family, fcast.sum)
}


all_names<- colnames(forecast_final_BU_ctes)

for (i in 1:length(all_names)) {
  if (str_detect(substrRight(all_names[i],3), ".y") == T) {
    colnames(forecast_final_BU_ctes)[i]<- gsub(all_names[i], ".y","DELETE",all_names[i])
    
  }else if ((str_detect(substrRight(all_names[i],3), ".x") == T)) {
    colnames(forecast_final_BU_ctes)[i]<-gsub(".x", "", all_names[i], fixed = TRUE)
  }
  
}

colnames(cleansed_df)  <- gsub(".", " ", colnames(cleansed_df), fixed = TRUE)
colnames(cleansed_df)  <- as.factor(colnames(cleansed_df))
colnames(cleansed_df) <- revalue(colnames(cleansed_df), c("LA Large Tablets   90G "="LA Large Tablets (>90G)",
                                                             "LA Large Tablets   90G _MT"="LA Large Tablets (>90G)_MT",
                                                             "LA Large Tablets   90G _TT"="LA Large Tablets (>90G)_TT",
                                                             "LA Small Tablets   40G "="LA Small Tablets (<40G)",
                                                             "LA Small Tablets   40G _MT"="LA Small Tablets_MT (<40G)",
                                                             "LA Small Tablets   40G _TT"="LA Small Tablets_TT (<40G)",
                                                             "LA Tita   Rhodesia"="LA Tita & Rhodesia",
                                                             "LA Puddings   Cakes" = "LA Puddings & Cakes",
                                                             "Clorets Gum Pellet   12S_FF_MT" = "Clorets Gum Pellet - 12S_FF_MT",
                                                             "Clorets Gum Pellet   12S_FF_TT" = "Clorets Gum Pellet - 12S_FF_TT",
                                                             "Clorets Gum Pellet   12S_FF_WHS" = "Clorets Gum Pellet - 12S_FF_WHS",
                                                             "Clorets Gum Pellet   2S_FF_MT" = "Clorets Gum Pellet - 2S_FF_MT",
                                                             "Clorets Gum Pellet   2S_FF_TT" = "Clorets Gum Pellet - 2S_FF_TT",
                                                             "Clorets Gum Pellet   2S_FF_WHS" = "Clorets Gum Pellet - 2S_FF_WHS",
                                                             "Clorets Gum Pellet   30S_FF_MT" = "Clorets Gum Pellet - 30S_FF_MT",
                                                             "Clorets Gum Pellet   30S_FF_TT" = "Clorets Gum Pellet - 30S_FF_TT",
                                                             "Clorets Gum Pellet   30S_FF_WHS" = "Clorets Gum Pellet - 30S_FF_WHS",
                                                             "Gelatin   Pouch_FF_MT" = "Gelatin - Pouch_FF_MT",
                                                             "Gelatin   Pouch_FF_TT" =  "Gelatin - Pouch_FF_TT",
                                                             "Gelatin   Pouch_FF_WHS" = "Gelatin - Pouch_FF_WHS",
                                                             "Halls Base   Display_FF_MT" = "Halls Base - Display_FF_MT",
                                                             "Halls Base   Display_FF_TT" = "Halls Base - Display_FF_TT",
                                                             "Halls Base   Display_FF_WHS" = "Halls Base - Display_FF_WHS",
                                                             "Halls Base   Creamy Display_FF_MT" = "Halls Base - Creamy Display_FF_MT",
                                                             "Halls Base   Creamy Display_FF_TT" = "Halls Base - Creamy Display_FF_TT",
                                                             "Halls Base   Creamy Display_FF_WHS" = "Halls Base - Creamy Display_FF_WHS",
                                                             "Oreo slug   Pack_FF_MT" = "Oreo slug - Pack_FF_MT",
                                                             "Oreo slug   Pack_FF_TT" = "Oreo slug - Pack_FF_TT",
                                                             "Oreo slug   Pack_FF_WHS" = "Oreo slug - Pack_FF_WHS",
                                                             "Trident Base Pellet   Xcare_FF_MT" = "Trident Base Pellet - Xcare_FF_MT",
                                                             "Trident Base Pellet   Xcare_FF_TT" = "Trident Base Pellet - Xcare_FF_TT",
                                                             "Trident Base Pellet   Xcare_FF_WHS" = "Trident Base Pellet - Xcare_FF_WHS",
                                                             "Trident Centre Filled Pellet   Box_FF_MT" = "Trident Centre Filled Pellet - Box_FF_MT", 
                                                             "Trident Centre Filled Pellet   Box_FF_TT" = "Trident Centre Filled Pellet - Box_FF_TT",
                                                             "Trident Centre Filled Pellet   Box_FF_WHS" = "Trident Centre Filled Pellet - Box_FF_WHS",
                                                             "Trident Slab   18S_FF_MT" = "Trident Slab - 18S_FF_MT",
                                                             "Trident Slab   18S_FF_TT" = "Trident Slab - 18S_FF_TT",
                                                             "Trident Slab   18S_FF_WHS" = "Trident Slab - 18S_FF_WHS",
                                                             "Trident Slab   6S_FF_MT" = "Trident Slab - 6S_FF_MT",
                                                             "Trident Slab   6S_FF_TT" = "Trident Slab - 6S_FF_TT",
                                                             "Trident Slab   6S_FF_WHS" = "Trident Slab - 6S_FF_WHS",
                                                             "Bubbaloo Candy   Lolipop_FF_MT" = "Bubbaloo Candy - Lolipop_FF_MT",
                                                             "Bubbaloo Candy   Lolipop_FF_TT" = "Bubbaloo Candy - Lolipop_FF_TT",
                                                             'Dentyne Base Pellet   Ice_FF_MT'= 'Dentyne Base Pellet - Ice_FF_MT',
                                                             'Dentyne Base Pellet   Ice_FF_TT'= 'Dentyne Base Pellet - Ice_FF_TT',
                                                             'Mantecol   Impulso_FF_MT'= 'Mantecol - Impulso_FF_MT',
                                                             'Mantecol   Impulso_FF_TT'= 'Mantecol - Impulso_FF_TT',
                                                             'Mantecol   Planificado_FF_MT'= 'Mantecol - Planificado_FF_MT',
                                                             'Mantecol   Planificado_FF_TT'='Mantecol - Planificado_FF_TT',
                                                             'Milka Large Tablets   Imported 100G_FF_MT' = 'Milka Large Tablets - Imported 100G_FF_MT',
                                                             'Milka Large Tablets   Imported 100G_FF_TT'= 'Milka Large Tablets - Imported 100G_FF_TT',
                                                             'Milka Large Tablets   Imported 300G_FF_MT'= 'Milka Large Tablets - Imported 300G_FF_MT',
                                                             'Milka Large Tablets   Imported 300G_FF_TT'= 'Milka Large Tablets - Imported 300G_FF_TT',
                                                             'Milka Large Tablets   Other_FF_MT'= 'Milka Large Tablets - Other_FF_MT',
                                                             'Milka Large Tablets   Other_FF_TT'= 'Milka Large Tablets - Other_FF_TT',
                                                             'Milka Medium Tablets   Other_FF_MT'= 'Milka Medium Tablets - Other_FF_MT',
                                                             'Milka Medium Tablets   Other_FF_TT'= 'Milka Medium Tablets - Other_FF_TT',
                                                             'Toblerone Large Tablets   More Than 100 Gr_FF_MT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_MT',
                                                             'Toblerone Large Tablets   More Than 100 Gr_FF_TT'= 'Toblerone Large Tablets - More Than 100 Gr_FF_TT',
                                                             'Toblerone Large Tablets   Up To 100 Gr_FF_MT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_MT', 
                                                             'Toblerone Large Tablets   Up To 100 Gr_FF_TT'= 'Toblerone Large Tablets - Up To 100 Gr_FF_TT',
                                                             'Trident Slab   Other_FF_MT'= 'Trident Slab - Other_FF_MT',
                                                             'Trident Slab   Other_FF_TT'= 'Trident Slab - Other_FF_TT',
                                                             "Gelatin   Light   Zero_FF_MT" = "Gelatin - Light / Zero_FF_MT",
                                                             "Milka Wafer   Chocopause_FF_TT"  = "Milka Wafer - Chocopause_FF_TT",
                                                             "Gelatin   Light   Zero_FF_TT" = "Gelatin - Light / Zero_FF_TT",
                                                             "Milka Wafer   Chocopause_FF_MT"  = "Milka Wafer - Chocopause_FF_MT",
                                                             "Halls Base   Single Piece_FF_TT" = "Halls Base - Single Piece_FF_TT",
                                                             "Halls Base   Single Piece_FF_MT" = "Halls Base - Single Piece_FF_MT",
                                                             "CDM Large Tablets   Other_FF_TT" = "CDM Large Tablets - Other_FF_TT",
                                                             "CDM Large Tablets   Other_FF_MT" = "CDM Large Tablets - Other_FF_MT"))

if (forecast_final_BU_ctes[ , which(names(forecast_final_BU_ctes) %in% "DELETE")]>0) {
  forecast_final_BU_ctes2 <- forecast_final_BU_ctes[ , -which(names(forecast_final_BU_ctes) %in% "DELETE")]
  colnames(cleansed_df)[1] <- "period2" 
  cleansed_df$period2 <- as.character(cleansed_df$period2)
  format_bu<- bind_rows(cleansed_df, forecast_final_BU_ctes2)
  write.csv(format_bu, "Deliveries//tool//Input//forecast_final_BU.csv")
  }else{
  colnames(cleansed_df)[1] <- "period2" 
  cleansed_df$period2 <- as.character(cleansed_df$period2)

  if (is.null(ctes_MA)==T) {
    colnames(forecast_final_BU)[1] <- "Period"
    format_bu<- bind_rows(cleansed_df, forecast_final_BU)
  }else{format_bu<- bind_rows(cleansed_df, forecast_final_BU_ctes)}
    write.csv(format_bu, "Deliveries//tool//Input//forecast_final_BU.csv")}


#poner basecodes correspondientes o FF... primer nombre de columna : ultimo nombre de columna 
#cambiar FF si es format flavor o Basecode en su caso
names(format_bu)
data_long <- gather(format_bu, FF, sellin, "Aveny Bran_FF_MT":"Trident Slab - 6S_FF_TT", factor_key=TRUE)

#write.csv(data_long, "Deliveries//tool//Input//forecast_final_BU_long.csv")
if (is.null(data_long$FF)==F) {
  data_long$FF <- as.character(gsub("\\.", " ", data_long$FF))
  data_long<- data_long[!is.na(data_long["sellin"]),]
}

#####################################################################################################
#####################################################################################################
#mix dissagregation 
hier1 <- read.csv("Output//hierarchy_final.csv")
mixlevel<- function(nivel){
  if (nivel == "Basecode" & substr(S_org[1],1,2) != "BR") {
      data_long$Base_Code <- substr(data_long$Basecode,1,6)
      data_long$canal <- substr(data_long$Basecode,8,9)
      data_long_2 <- merge(hier1[,-c(2,3,4,6,8,10,12:16)], unique(data_long[,-1]), by = "Base_Code", all.y = T)
      data_long_2 <- unique(data_long_2)
      data_long_3 <- data_long_2[which(data_long_2$period2 >= fechafcast),]
      data_long_3 <- data_long_3[which(data_long_3$period2 <= 202112),] 
    }else if (nivel == "Subfamily" & substr(S_org[1],1,2) == "BR"){
      data_long$Base_Code <- substr(data_long$Basecode,1,6)
  #data_long$canal <- substr(data_long$Basecode,8,9)
      data_long_2 <- merge(hier1[,-c(2,3,4,6,8,10,12:16)], unique(data_long[,-1]), by = "Base_Code", all.y = T)
      data_long_2 <- unique(data_long_2)
      data_long_3 <- data_long_2[which(data_long_2$period2 >= fechafcast),]
      data_long_3 <- data_long_3[which(data_long_3$period2 <= 202112),] 
  #aqui se tiene que hacer un merge con la base que tenga las subfamilies correctas
      dicc<- read.csv("Deliveries//tool//Input//Sub Families BR.csv", stringsAsFactors = F)
      data_long_3 <- merge(data_long_3,unique(dicc[c(2,3)]), by.x = "Base_Code", by.y ="Base.Code", all.x = T)
    }else if (nivel == "Format Flavor") {
      for (i in 1:length(data_long$FF)) {
        data_long$FF2[i] <- substr(data_long$FF[i],1,nchar(as.character(data_long$FF[i]))-3)
        data_long$canal[i] <- substr(data_long$FF[i],nchar(as.character(data_long$FF[i]))-1,nchar(as.character(data_long$FF[i])))
      }

      data_long_2 <- merge(hier1[,-c(2,3,4,6,8,10,12:17)], unique(data_long[,-2]), by.x = "Format_Flavor_Name",by.y = "FF2",  all.y = T)
      data_long_2 <- unique(data_long_2)
      data_long_3 <- data_long_2[which(data_long_2$period2 >= fechafcast),]
      data_long_3 <- data_long_3[which(data_long_3$period2 <= 202112),]}
  return(data_long_3)}
  


##################################################################################
#################################################################################
#poner mix level: "Format Flavor" o "Basecode" o "Subfamily"
data_long_3 <- mixlevel("Format Flavor")
################################################################################
###############################################################################

total_fam<- summaryBy(sellin ~ IBP_Family_Name,data =data_long_3 ,FUN=sum)
mix_percentage<- function(mix_level_max_name, mix_level_min_name){
  if (mix_level_max_name=="Format_Flavor_Name" & "canal"== mix_level_min_name) {
    
    total_bajo<- summaryBy(sellin ~ IBP_Family_Name + Format_Flavor_Name + canal,
                           data = data_long_3 ,FUN=sum)
  }else if (mix_level_max_name=="SUBFAM_Desc" & NULL== mix_level_min_name) {
    total_bajo<- summaryBy(sellin ~ IBP_Family_Name + Sub.Family,
                           data = data_long_3 ,FUN=sum)
  }
  mix1<- merge(total_fam, total_bajo, by = "IBP_Family_Name")
  mix1$porcentaje <- (mix1$sellin.sum.y/mix1$sellin.sum.x)*100
  return(mix1)
}

########################################################################################
########################################################################################
###if you're running BR just add ("SUBFAM_Desc", NULL) as the variables for the function
#else write the variable name e.g., ("Format_Flavor_Name", "canal")
mix<- mix_percentage("Format_Flavor_Name", "canal")
write.csv(mix, "Deliveries//tool//Input//mix.csv")
########################################################################################
########################################################################################


