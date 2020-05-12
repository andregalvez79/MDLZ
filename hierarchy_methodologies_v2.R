options(scipen = 999999)
setwd("C:\\Users\\OYO9864\\OneDrive - MDLZ\\Hierarchy\\prueba")
require(readxl)
library(readr)
require(stringr)
require(tidyr)
require(magrittr)
library(plyr)
require(dplyr)
require(lubridate)
require(odbc)
library(cluster)
library(fpc)
library(ggplot2)
library(reshape2)
library(purrr)
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(scorecard)
library(factoextra)
library(class)
library(caret)
library(caTools)
library(knncat)
library(gmodels)
library(caret)
library(rpart.plot)
library(caTools)
library(tree)
library(randomForest)



con <- dbConnect(odbc(),
                 dsn = "",
                 uid = "",
                 pwd = "")


sql_query <- read_file("Tabla_EAN_2.sql")
EAN<- odbc::dbGetQuery(con, sql_query)


summary(EAN)
data1<- read.csv("hierarchy_final_antes.csv", stringsAsFactors = F)
data2_2<- read.csv("hierarchy_final_1.csv", stringsAsFactors = F) #hierarchy
APO<- read.csv("APO_MC62_CR.csv", stringsAsFactors = F) #data from APO transaction MC62


#### Prepare data #####

#asegurarse que son chr los SKU para un merge más sencillo
str(data1)
str(data2_2)
data1$SKU <- as.character(data1$Código.Producto)
data2_2$SKU <- as.character(data2_2$SKU)
str(EAN)
EAN$SKU <- as.character(EAN$MATERIAL)
str(APO)

#split la variable de decripcion
split1 <- strsplit(data1$Detalle, " ") #split por espacio
split1_1 <- strsplit(data2_2$Detalle, " ")
data2_2$word<- unlist(word(data2_2$Detalle,1))
data1$word<- unlist(word(data1$Detalle,1))


#separar las dimensiones de las descripciones
peso <- 0
for (i in 1:length(split1)) {
  for (k in 1:max(length(split1))) { #toma el maximo de splits
    x <- str_detect(split1[[i]][k], "[0-9]") #checa si tiene número el string por cada split
    if (is.na(x) == FALSE & x !=FALSE) { #condicones: si no es na y es numero: guardalo
      peso[i] <- (split1[[i]][k])
    }
  }
  
}
peso2 <- 0
for (i in 1:length(split1_1)) {
  for (k in 1:max(length(split1_1))) { #toma el maximo de splits
    x <- str_detect(split1_1[[i]][k], "[0-9]") #checa si tiene número el string por cada split
    if (is.na(x) == FALSE & x !=FALSE) { #condicones: si no es na y es numero: guardalo
      peso2[i] <- (split1_1[[i]][k])
    }
  }
  
}
#save it
data1$Dimensiones<- peso
data2_2$Dimensiones<- peso2

#separar las descripciones de las dimensiones
claves <-  0
claves2 <-  0
for (i in 1:length(split1)) {
  for (k in 1:2) { #toma el maximo de splits
    x <- str_detect(split1[[i]][k], "[0-9]") #checa si tiene número el string por cada split
    if (is.na(x) == FALSE & x == FALSE) { #condicones: si no es na y no es numero: guardalo
      claves[i] <- paste(split1[[i]][1], split1[[i]][2]) #pega las primeras dos ya que siempre on letras
      if (str_detect(split1[[i]][3], "[0-9]")==FALSE & is.na(split1[[i]][3]) == FALSE){ #filtro para saber si pego el tercero o no
        claves2[i] <- paste(claves[i], split1[[i]][3])
      }else (claves2[i] <- claves[i])
      
    }
  }
  
}

claves_2 <-  0
claves2_2 <-  0
for (i in 1:length(split1_1)) {
  for (k in 1:2) { #toma el maximo de splits
    x <- str_detect(split1_1[[i]][k], "[0-9]") #checa si tiene número el string por cada split
    if (is.na(x) == FALSE & x == FALSE) { #condicones: si no es na y no es numero: guardalo
      claves_2[i] <- paste(split1_1[[i]][1], split1_1[[i]][2]) #pega las primeras dos ya que siempre on letras
      if (str_detect(split1_1[[i]][3], "[0-9]")==FALSE & is.na(split1_1[[i]][3]) == FALSE){ #filtro para saber si pego el tercero o no
        claves2_2[i] <- paste(claves_2[i], split1_1[[i]][3])
      }else (claves2_2[i] <- claves_2[i])
      
    }
  }
  
}
#Save it
data2_2$Key <- claves2_2
data1$Key <- claves2

#eliminar _FF del format flavor
names(data1)
data1$FF <-  gsub("_FF","", data1$Format.Flavor.Name)
data1$FF <-  gsub(" FF","", data1$FF)
names(data2_2)
data2_2$FF <-  gsub("_FF","", data2_2$Format.Flavor.Name)
data2_2$FF <-  gsub(" FF","", data2_2$FF)

#cambiarle el nombre del producto a SKU
APO$SKU <-  as.character(APO$ZMATNR)
str(APO)
APO$Base.Pack <- as.numeric(APO$Base.Pack)
APO$Base.Pack[is.na(APO$Base.Pack)] <- 0
#filtrar EANes
EAN2 <- EAN[which(EAN$EAN_TYPE=='ZIN'),] #displays var.x
EAN3 <- EAN[which(EAN$EAN_TYPE=='CSE'),] #case var.y
EAN4 <- EAN[which(EAN$EAN_TYPE=='PAL'),] #pallet var
#BOM si es maquila o no... si tiene dos productos hay dos. esto importante para ZIN. 
#La transacción para ver las boms en ECC es CS03... Pedir por I request. Replenishment. o Luciano?
# f1 te sale info y en herramientas sale info tecnica.... ya tengo la tabla si no aparece es porque viene con otro material.
#tabla YMTI_BOM_AUDIT 
#8301 curitva y la segunda planta 8308 besea todo brazil. material type z01 = producto terminado.
names(EAN)
EAN3 <- EAN3[,c("EAN","EAN_TYPE","EAN_NUMTYP", "DENOMINTR","GROSS_WT","HEIGHT","LENGTH","UNIT_OF_WT",
                "UNIT_DIM","VOLUME","WIDTH","PROD_HIER","PROD_CATEG","WEIGHT","SKU")]
EAN4 <- EAN4[,c("EAN","EAN_TYPE","EAN_NUMTYP", "DENOMINTR","GROSS_WT","HEIGHT","LENGTH","UNIT_OF_WT",
                "UNIT_DIM","VOLUME","WIDTH","PROD_HIER","PROD_CATEG","WEIGHT","SKU")]
EANM <- merge(x = EAN2, y = EAN3, by = "SKU", all = F)
EANM <- merge(x = EANM, y = EAN4, by = "SKU", all = F)
EANM <- EANM %>% distinct(SKU, .keep_all = T)

#liminate 0 bbefore numebr in sku EANM
substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

EANM$SKU <- substrRight(EANM$SKU, 14)
#merge datasets
data2 <- merge(x = data1, y = EANM, by = "SKU", all = F)
data3_2 <- merge(x = data2_2, y = EANM, by = "SKU", all.x = T)

data3_2 <- merge(data3_2, APO, by = "SKU")
data2 <- merge(data2, APO, by = "SKU")

data2 <- filter(data2, SKU %in% APO$SKU)
data3_2 <- filter(data3_2, SKU %in% APO$SKU)

names(data2)

data2 <- data2[,c("SKU", "Base.Code", "Format.Flavor", "IBP.Family", "IBP.Family.Name", "IBP.Sub.Category",
                  "IBP.Sub.Category.Name", "IBP.Category", "IBP.Category.Name", "Peso.Neto.por.unidad..gr.",
                  "Rendimiento..L.", "Life.Cycle.Status.Actual", "word", "Dimensiones", "FF", "EAN.x", "EAN_NUMTYP.x",
                  "EAN.y", "EAN_NUMTYP.y", "EAN_NUMTYP", "ZCUSTLEV1", "ZCUSTLEV2", "ZLOCNO",
                  "ZPRODCAT", "DP.Basecod", "Division", "Forecast.S", "Brand.Segm",	"WW.Categor",	"WW.Categor.1",
                  "Segment.FI",	"Sub.Brand",	"Sub.Catego",	"GH.Product","GH.Level.7", "Sales.Orga", "Base.Pack",
                  "Demand.Pla", "EPM",	"EPM.Description", "IBP.Brand.1",	"IBP.Busine.1","LH.Country","LH.Categor",
                  "LH.Market", "LH.Product.1", "SKU.Status", "Key", "DENOMINTR.x","GROSS_WT.x", "HEIGHT.x", 
                  "LENGTH.x","UNIT_OF_WT.x","UNIT_DIM.x","VOLUME.x", "WIDTH.x","PROD_HIER.x","PROD_CATEG.x", "WEIGHT.x", 
                  "DENOMINTR.y","GROSS_WT.y", "HEIGHT.y", "LENGTH.y","UNIT_OF_WT.y","UNIT_DIM.y","VOLUME.y", "WIDTH.y",
                  "PROD_HIER.y","PROD_CATEG.y", "WEIGHT.y")]
#me quede aqui
data3_2 <- data3_2[,c("SKU", "ï..Base.Code", "Format.Flavor.x", "IBP.Family.x", "IBP.Family.Name", "IBP.Sub.Category",
                  "IBP.Sub.Category.Name", "IBP.Category", "IBP.Category.Name", "Peso.Neto.por.unidad..gr.",
                  "Rendimiento..L.", "Life.Cycle.Status.Actual", "word", "Dimensiones", "FF", "EAN.x", "EAN_NUMTYP.x",
                  "EAN.y", "EAN_NUMTYP.y", "EAN_NUMTYP", "ZCUSTLEV1", "ZCUSTLEV2", "ZLOCNO",
                  "ZPRODCAT", "DP.Basecod", "Division", "Forecast.S", "Brand.Segm",	"WW.Categor",	"WW.Categor.1",
                  "Segment.FI",	"Sub.Brand",	"Sub.Catego",	"GH.Product","GH.Level.7", "Sales.Orga", "Base.Pack",
                  "Demand.Pla", "EPM",	"EPM.Description", "IBP.Brand.1",	"IBP.Busine.1","LH.Country","LH.Categor",
                  "LH.Market", "LH.Product.1", "SKU.Status", "Key","DENOMINTR.x","GROSS_WT.x", "HEIGHT.x", 
                  "LENGTH.x","UNIT_OF_WT.x","UNIT_DIM.x","VOLUME.x", "WIDTH.x","PROD_HIER.x","PROD_CATEG.x", "WEIGHT.x", 
                  "DENOMINTR.y","GROSS_WT.y", "HEIGHT.y", "LENGTH.y","UNIT_OF_WT.y","UNIT_DIM.y","VOLUME.y", "WIDTH.y",
                  "PROD_HIER.y","PROD_CATEG.y", "WEIGHT.y")]

names(data3_2)
write.csv(data2, "basetryout_CR.csv")
#Tablas donde se almacena la informacion de display unidades y gramos por unidad
#MARM
#transaccion mm03 
#MC62 APO 

str(data2)

#Forecast.S, ZLOCNO, ZPRODCAT, Division, GH.Product, LH.Country, LH.Categor, LH.Market, SKU.Status
data2$Peso.Neto.por.unidad..gr. <- as.numeric(data2$Peso.Neto.por.unidad..gr.)
data2$Unidades.por.Caja <- as.numeric(data2$Unidades.por.Caja)
data2$Rendimiento..L. <- as.numeric(data2$Rendimiento..L.)
data2$Rendimiento..L.[is.na(data2$Rendimiento..L.)] <- 0
data2$Forecast.S <-  as.numeric(data2$Forecast.S)
data2$ZLOCNO <-  as.numeric(data2$ZLOCNO)
data2$ZPRODCAT <-  as.numeric(data2$ZPRODCAT)
data2$Division <-  as.numeric(data2$Division)
data2$GH.Product <-  as.numeric(data2$GH.Product)
data2$LH.Country <-  as.numeric(data2$LH.Country)
data2$LH.Categor <-  as.numeric(data2$LH.Categor)
data2$LH.Market <-  as.numeric(data2$LH.Market)
data2$SKU.Status <-  as.numeric(data2$SKU.Status)
data2 <- as.data.frame(unclass(data2))
str(data2)
levels(data2$EAN_NUMTYP)
levels(data2$EAN_NUMTYP.x)
levels(data2$EAN_NUMTYP.y)

sapply(data2, levels)
#re level factors

levels(data2$EAN_NUMTYP)[levels(data2$EAN_NUMTYP)==""] <- "Z3"
#levels(data2$EAN_NUMTYP)[levels(data2$EAN_NUMTYP)=="14"] <- "Z3"
#levels(data2$EAN_NUMTYP)[levels(data2$EAN_NUMTYP)=="Z1"] <- "Z3"
levels(data2$EAN_NUMTYP.x)[levels(data2$EAN_NUMTYP.x)=="13"] <- "Z1"
levels(data2$EAN_NUMTYP.x)[levels(data2$EAN_NUMTYP.x)==""] <- "Z1"
#for 3_2
data3_2$Peso.Neto.por.unidad..gr. <- as.numeric(data3_2$Peso.Neto.por.unidad..gr.)
data3_2$Unidades.por.Caja <- as.numeric(data3_2$Unidades.por.Caja)
data3_2$Rendimiento..L. <- as.numeric(data3_2$Rendimiento..L.)
data3_2$Rendimiento..L.[is.na(data3_2$Rendimiento..L.)] <- 0
data3_2$Forecast.S <-  as.numeric(as.character(data3_2$Forecast.S))
data3_2$ZLOCNO <-  as.numeric(data3_2$ZLOCNO)
data3_2$ZPRODCAT <-  as.numeric(data3_2$ZPRODCAT)
data3_2$Division <-  as.numeric(data3_2$Division)
data3_2$GH.Product <-  as.numeric(data3_2$GH.Product)
data3_2$LH.Country <-  as.numeric(data3_2$LH.Country)
data3_2$LH.Categor <-  as.numeric(data3_2$LH.Categor)
data3_2$LH.Market <-  as.numeric(data3_2$LH.Market)
data3_2$SKU.Status <-  as.numeric(data3_2$SKU.Status)
data3_2 <- as.data.frame(unclass(data3_2))
str(data3_2)
levels(data3_2$EAN_NUMTYP)
levels(data3_2$EAN_NUMTYP.x)
levels(data3_2$EAN_NUMTYP.y)

sapply(data3_2, levels)
#re level factors

levels(data3_2$EAN_NUMTYP)[levels(data3_2$EAN_NUMTYP)==""] <- "Z3"
levels(data3_2$EAN_NUMTYP.x)[levels(data3_2$EAN_NUMTYP.x)==""] <- "Z1"
#levels(data3_2$EAN_NUMTYP.y)[levels(data3_2$EAN_NUMTYP.y)=="<NA>"] <- "Z3"

#----------------------a partir de aqui----------------------------------------
#########################
data2<- data2[complete.cases(data2), ]
data3_2<- data3_2[complete.cases(data3_2), ]
`%notin%` <- Negate(`%in%`)
newsku<- filter(data3_2, SKU %notin% data2$SKU)

data2 <- data2 %>% distinct()
data3_2 <- data3_2 %>% distinct()


#data3_2<- filter(data3_2, SKU %notin% data2$SKU)
#str(data3_2)
#data3_3[is.na(data3_2)] <- 0
#data3_3$EAN_NUMTYP[is.na(data3_2$EAN_NUMTYP)] <- "Z3"
#data3_3$EAN_NUMTYP.x[is.na(data3_2$EAN_NUMTYP.x)] <- "Z1"
#data3_3$EAN_NUMTYP.y[is.na(data3_2$EAN_NUMTYP.y)] <- "Z3"

#levels(data3_3$EAN.x)[levels(data3_3$EAN.x[1])=="<NA>"] <- "0"
#data3_3$EAN.x[is.na(data3_3$EAN.x)] <- "0"

##### la info etá incompeta de EAN... si ponemos complete cases tenemos 
#que tirar 39 obs eso que ya arreglamo unas
#categ subcat family 
str(data2)

set.seed(101) 
sample = sample.split(data2$SKU, SplitRatio = .75)
train = subset(data2, sample == TRUE)
train = na.omit(train)
test  = subset(data2, sample == FALSE)
test_labels <- test[, 2]
train_labels <- train[, 2]
test = na.omit(test)



nacols <- function(df) {
  colnames(df)[unlist(lapply(df, function(x) anyNA(x)))]
}
nacols(train)
sqrt(nrow(train))


#testing newer model without hot encoding just tranforming factor into integers.
test$SKU <- as.numeric(as.character(test$SKU))
train$SKU <- as.numeric(as.character(train$SKU))
data3_2$SKU <- as.numeric(as.character(data3_2$SKU))
train1 <- mutate_if(train, is.factor, ~ as.integer(.x))
test1 <- mutate_if(test, is.factor, ~ as.integer(.x))
test2 <- mutate_if(data3_2, is.factor, ~ as.integer(.x))
newsku$SKU <- as.numeric(as.character(newsku$SKU))
test2_1 <- mutate_if(newsku, is.factor, ~ as.integer(.x))


#testing k for new model

accuracy <- rep(0, length(test2))
k <- 1:52
for(x in k){
  prediction <- knn(train1[,-2], test1[,-2], train1[,2], k = x)
  accuracy[x] <- mean(prediction == test2[,2])
}

plot(k, accuracy, type = 'b')

knn_predict <- knn(train1[,-2], test2[,-2], train1[,2], k = 1)
#knn_predict
predicted <- as.data.frame(knn_predict)

test2$predicted <- as.numeric(as.character(predicted$knn_predict))
test2$BaseCodeOriginal <- data3_2$ï..Base.Code 


levelBC <- levels(train$Base.Code)
levelBC <-as.character(levelBC)

#BaseCodefinal
for (i in 1:length(test2$predicted)) {
     test2$predictedBC[i] <-levelBC[test2$predicted[i]]
}

#dataacc <-  merge(data2[!duplicated(data2$SKU), ], test2, by = "SKU", all = T)
#dataacc <- select(dataacc,50,2,49,48,1)
#dataacc <- mutate_if(dataacc, is.factor, ~ as.character(.x))

for (i in 1:length(test2$predictedBC)) {
  if (test2$predictedBC[i] == test2$BaseCodeOriginal[i]) {
    test2$OK[i] <- 1 
  }else (test2$OK[i] <- 0)
}

acc = sum(test2$OK)/(length(test2$OK))
acc

#son muchos por tener cust level 2
#al final tomar nada más columnas relevantes y hacer unique()

write.csv(test2, "results_CR_k1.csv")
#falta pruebas mas fuertes check positivo y errores.
#k=4 = 36.36%
#k=1 = 90.9%
#k=19 = 27.27%


xtab = table(knn_predict, test2[,2])
precision = xtab[1,1]/sum(xtab[,1]) #Get the confusion matrix to see accuracy value and other parameter values
recall = xtab[1,1]/sum(xtab[1,])
f = 2 * (precision * recall) / (precision + recall)

confusionMatrix(knn_predict, test2[,2])
confusionMatrix(table(knn_predict, test2[,2]))

CrossTable(x = test2[,2], y = knn_predict,
           prop.chisq = T)



##create confusion matrix
tab <- table(knn_predict,train1[,2])
tab
##this function divides the correct predictions by total number of predictions that tell us how accurate teh model is.

accuracy <- function(x){sum(diag(x)/(sum(rowSums(x)))) * 100}
accuracy(tab)






##### random forest #### AUN NO SIRVE BIEN.

sapply(train, levels)

data2x<- data2[complete.cases(data2), ]
data2_1 <- select(data2x,1,3:12,15,16,19:22,24,28:30,32,33) #eantyoe 35 eannumtype 34 eantype y 31 ean type x 24
data2_2 <-  droplevels(data2_1)
data2_2<- data2_2[complete.cases(data2_2), ]

table(data2_2$SKU)
table(data2_2$Base.Code)
data2_2<- data2_2[!(is.na(data2_2) | data2_2 == ""), ]
toBeRemoved<-which(data2_2$EAN.x=="")
data2_2<-data2_2[-toBeRemoved,]
data2_2 <-  droplevels(data2_2)

#data para predecir necesitas alimntarle toda la base...
str(data3_4)
data3_5<- data3_4
data3_5$SKU <- as.factor(data3_5$SKU)
data3_5$Peso.Neto.por.unidad..gr. <- as.factor(data3_5$Peso.Neto.por.unidad..gr.)
data3_5$Rendimiento..L.  <- as.factor(data3_5$Rendimiento..L. )
data3_5$istest <- 1
data2_2$istest <- 0
data3_5$istest  <- as.factor(data3_5$istest )
data2_2$istest  <- as.factor(data2_2$istest )
str(data3_5)
data3_5 <- select(data3_5,-18,-23,-24,-26,-27)
str(data2_2)
data2_3 <- select(data2_2,-22)
alldata <- rbind(data2_3, data3_5)
alldata<- alldata[complete.cases(alldata), ]
trainrf <- alldata[ which(alldata$istest=='0'),]
testrf <- alldata[ which(alldata$istest=='1'),]
trainrf <- trainrf[,-23]
testrf <- testrf[,-23]



set.seed(123) 
sample = sample.split(data2_2$Base.Code, SplitRatio = .75)
train = subset(data2_2, sample == TRUE)
test  = subset(data2_2, sample == FALSE)
anyNA(test)
anyNA(train)
levels(train$EAN.x)


trctrl <- trainControl(method = "repeatedcv", number = 15, repeats = 5, search = "random")
set.seed(3333)
metric <- "Accuracy"
mtry <- 1:10
tunegrid <- expand.grid(.mtry=mtry)

 

dtree_fit <- train(Base.Code~., 
                   data=train1, 
                   method='rpart', 
                   #metric=metric,
                   parms = list(split = "gini"),
                   #tuneGrid=tunegrid, 
                   trControl=trctrl,
                   tuneLength = 15,
                   )

prp(dtree_fit$finalModel, box.palette = "Reds", tweak = 1.2)
dtree_fit
plot(dtree_fit)


test_pred <- predict(dtree_fit, newdata = test1)
confusionMatrix(test_pred, data3_2$Base.Code )

##### Mixed Model #### ESTE MENOS

#using test and train data

library (lme4)
library(coefplot)
library(effects)
library (car)
library (psych)
library (lsmeans)
library (afex)
library (pbkrtest)
library (lattice)
library (parallel)
library(plotrix)
library (ggplot2)
library(influence.ME)


options(contrasts=c("contr.sum", "contr.poly"))

summary(train)
summary(test)
names(train)
summary(data2)
#max model specification
maxmodel <- lmer(Final.Base.Code ~ SKU + Final.Sub.Category
                  + Peso.caja..Kg. + Unidades.por.Caja + Peso.por.Unidad..gr.+
                   EAN.y
                   + (1 | FF)
                   #+(1 + Final.Base.Code| SKU) 
                   #+(1 + Format.Flavor.code + FF + Key|Final.Base.Code)
                 #+ (0 + Final.Sub.Category||IBPCategory), 
                   # + (1 + EAN.x|EAN.y), #+ (1 + EAN.y|EAN), 
                   # + (1 + EAN.x + EAN.y + EAN|SKU),
                 ,data = train, 
                 control = lmerControl(optCtrl = list(maxfun = 1e+9)))


summary(maxmodel)
contrasts(invest$f_major)
#just checking which otpimizer works best to converge the model
my_allFit <- allFit(maxmodel, maxfun = 10e+9)
ok_fits <- sapply(my_allFit, is, "merMod")
ok_fits



















###################









test$SKU <- as.numeric(as.character(test$SKU))
train$SKU <- as.numeric(as.character(train$SKU))
data3_2$SKU <- as.numeric(as.character(data3_2$SKU))
train1 <- mutate_if(train, is.factor, ~ as.integer(.x))
test1 <- mutate_if(test, is.factor, ~ as.integer(.x))
test2 <- mutate_if(data3_2, is.factor, ~ as.integer(.x))
newsku$SKU <- as.numeric(as.character(newsku$SKU))
test2_1 <- mutate_if(newsku, is.factor, ~ as.integer(.x))

trainc<- rbind(train1,test1)
for (y in 1:499) {
  test$SKU <- as.numeric(as.character(test$SKU))
  train$SKU <- as.numeric(as.character(train$SKU))
  data3_2$SKU <- as.numeric(as.character(data3_2$SKU))
  train1 <- mutate_if(train, is.factor, ~ as.integer(.x))
  test1 <- mutate_if(test, is.factor, ~ as.integer(.x))
  test2 <- mutate_if(data3_2, is.factor, ~ as.integer(.x))
  newsku$SKU <- as.numeric(as.character(newsku$SKU))
  test2_1 <- mutate_if(newsku, is.factor, ~ as.integer(.x))
  
  trainc<- rbind(train1,test1)
  
  knn_predict <- knn(trainc[,-2], test2_1[,-2], trainc[,2], k = 102)
  #knn_predict
  predicted <- as.data.frame(knn_predict)
  
  test2_1$predicted <- as.numeric(as.character(predicted$knn_predict))
  test2_1$BaseCodeOriginal <- newsku$ï..Base.Code 
  
  
  levelBC <- levels(as.factor(data3_2$ï..Base.Code))
  levelBC <-as.character(levelBC)
  
  #BaseCodefinal
  for (i in 1:length(test2_1$predicted)) {
    test2_1$predictedBC[i] <-levelBC[test2_1$predicted[i]]
  }
  
  #dataacc <-  merge(data2[!duplicated(data2$SKU), ], test2, by = "SKU", all = T)
  #dataacc <- select(dataacc,50,2,49,48,1)
  #dataacc <- mutate_if(dataacc, is.factor, ~ as.character(.x))
  
  for (i in 1:length(test2_1$predictedBC)) {
    if (test2_1$predictedBC[i] == test2_1$BaseCodeOriginal[i]) {
      test2_1$OK[i] <- 1 
    }else (test2_1$OK[i] <- 0)
  }
  
  acc = sum(test2_1$OK)/(length(test2_1$OK))
  if (acc>0) {
    print(acc)
    print(y)
  }
  
}
