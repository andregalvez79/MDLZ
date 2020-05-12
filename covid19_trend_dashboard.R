library(xts)
library(nlme)
library(lmtest)
library(odbc)
library(dplyr)
library(readr)
library(doBy)

###############
###############
fecha_hoy=20200510
######## PONER PAIS ######
PAIS <- ""
##########################
###############
##############
options(scipen = 999999)
setwd("")

con <- dbConnect(odbc(),
                 dsn = "",
                 uid = "",
                 pwd = "")

sql_query <- read_file("QUERY.sql")
#sql_query <- gsub(pattern = "SalesOrg_replace", replace =  ,sql_query)
sql_query <- gsub(pattern = "HotPeriod_Superior_<=", replace = fecha_hoy,sql_query)
sql_query <- gsub(pattern = "HotPeriod_Inferior_>=", replace = "20200101",sql_query)
sql_query <- gsub(pattern = "ColdPeriod_Superior_<=", replace = fecha_hoy,sql_query)
sql_query <- gsub(pattern = "ColdPeriod_Inferior_>=", replace = "20200101",sql_query)



BWP<- odbc::dbGetQuery(con,sql_query)

names(BWP)[names(BWP) == "MATERIAL"] <- "SKU"
names(BWP)
BWP$SKU <- gsub("(^|[^0-9])0+", "\\1", BWP$SKU, perl = TRUE)
if (PAIS != "MX") {
    BWP <- BWP[,-15]    
}

write.csv(BWP,"bwp.csv")

sql_query <- read_file("HIER.sql")
HIER_A<- odbc::dbGetQuery(con,sql_query)
coro<- read.csv("corona.csv", stringsAsFactors = F)

colnames(HIER_A)<-c("DISTR_CHAN","SALESORG","CUSTOMER_LEVEL_1", "CUSTOMER_LEVEL_2", "SOLD_TO","ZCUSTOMER","ZCAHIER01",  
                  "ZCAHIER02", "ZCAHIER03",   "ZCAHIER04",   "ZCAHIER05",   "ZCAHIER06",   "ZCGHIER01",   "ZCGHIER02",  
                  "ZCGHIER03",   "ZCGHIER04",   "ZCGHIER05",   "ZCGHIER06")


HIER_A <- as.data.frame(HIER_A)
BWP <- as.data.frame(BWP)
complete_df<- merge(BWP,HIER_A, by = c("SOLD_TO","DISTR_CHAN","SALESORG","CUSTOMER_LEVEL_1","CUSTOMER_LEVEL_2"),all.x = T)
write.csv(complete_df,"complete_df.csv")

##solo complete cases guardar lo que se borro

if (PAIS != "MX") {
    complete_df_clean<- complete_df[which(is.na(complete_df$CUSTOMER_LEVEL_1) == F),]
    complete_df_clean<- complete_df_clean[which(is.na(complete_df_clean$BASECODE) == F),]
    complete_df_clean<-complete_df_clean%>%filter(CUSTOMER_LEVEL_1 != "NA")
    
    complete_df_na <-  complete_df_na[which(is.na(complete_df_na$CUSTOMER_LEVEL_1) == T),]
    complete_df_na <-  complete_df_na[which(is.na(complete_df_na$BASECODE) == T),]
    complete_df_na <- complete_df_na%>%filter(CUSTOMER_LEVEL_1 == "NA")
    
}else{
    complete_df_clean<- complete_df[which(is.na(complete_df$CUSTOMER_LEVEL_1) == F),]
    complete_df_clean<- complete_df_clean[which(is.na(complete_df_clean$BASECODE) == F),]
    complete_df_clean<-complete_df_clean%>%filter(CUSTOMER_LEVEL_1 != "NA")
    #complete_df_clean<-complete_df_clean%>%filter(CAT_Desc != "LA Meals")
    
    #complete_df_na<-complete_df%>%filter(CAT_Desc == "LA Meals")
    complete_df_na <-  complete_df_na[which(is.na(complete_df_na$CUSTOMER_LEVEL_1) == T),]
    complete_df_na <-  complete_df_na[which(is.na(complete_df_na$BASECODE) == T),]
    complete_df_na <- complete_df_na%>%filter(CUSTOMER_LEVEL_1 == "NA")
}



write.csv(complete_df_na,"complete_df_excluido.csv")

if (PAIS != "MX") {
eleg <- summaryBy(SELLIN_KG ~ PERIOD + FAM_Desc + CUSTOMER_LEVEL_1 + BU_Desc, data=complete_df_clean ,FUN=sum)
}else{eleg <- summaryBy(SELLIN_KG ~ PERIOD + FAM_Desc + CUSTOMER_LEVEL_2 + BU_Desc, data=complete_df_clean ,FUN=sum)
}
names(eleg)[names(eleg) == "PERIOD"] <- "Fecha"
eleg$SI <- eleg$SELLIN_KG

if (PAIS != "MX"){
for (i in 1:length(eleg$CUSTOMER_LEVEL_1)) {
    if (eleg$CUSTOMER_LEVEL_1[i] == "BR0001") {
        eleg$canal[i] <-  "ATC"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0003") {
        eleg$canal[i] <-  "DIS"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0004") {
        eleg$canal[i] <- "GKA"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0005") {
        eleg$canal[i] <- "NNE"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0006") {
        eleg$canal[i] <- "SMR"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0008") {
        eleg$canal[i] <- "INST"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0009") {
        eleg$canal[i] <-  "TT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0010") {
        eleg$canal[i] <-  "MT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0012") {
        eleg$canal[i] <-  "NC"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0013") {
        eleg$canal[i] <-  "EC"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "BR0000") {
        eleg$canal[i] <-  "BR0000"
    
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "AR0001") {
        eleg$canal[i] <-  "TD"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "AR0002") {
        eleg$canal[i] <-  "MT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "AR0003") {
        eleg$canal[i] <-  "WH"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "AR0004") {
        eleg$canal[i] <-  "TD - Paraguay"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "CR0001") {
        eleg$canal[i] <-  "MT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "CR0002") {
        eleg$canal[i] <-  "TT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "PE0001") {
        eleg$canal[i] <-  "MT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "PE0002") {
        eleg$canal[i] <-  "TT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "CO0001") {
        eleg$canal[i] <-  "MT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "CO0002") {
        eleg$canal[i] <-  "TT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "EC0001") {
        eleg$canal[i] <-  "MT"
        
    }else if (eleg$CUSTOMER_LEVEL_1[i] == "EC0002") {
        eleg$canal[i] <-  "TT"
        
    }else{eleg$canal[i] <-  0} 
    
    
}
}else{
    for (i in 1:length(eleg$CUSTOMER_LEVEL_2)){
    if (eleg$CUSTOMER_LEVEL_2[i] == "MX000002") {
        eleg$canal[i] <-  "CLUBES"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000005") {
        eleg$canal[i] <-  "CS OTHERS"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000006") {
        eleg$canal[i] <- "CS OXXO"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000003") {
        eleg$canal[i] <- "GKA"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000004") {
        eleg$canal[i] <- "LKA"
    
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000008") {
        eleg$canal[i] <-  "NEWCHANNEL"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000009") {
        eleg$canal[i] <-  "P. SIGMA"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000010") {
        eleg$canal[i] <- "RETAILCAPS"
        
    }else if (eleg$CUSTOMER_LEVEL_2[i] == "MX000007") {
        eleg$canal[i] <- "WHOLESALES"
        
    }
}
}        
        

#######################################################################
data = c(202001:as.numeric(substr(fecha_hoy,1,6)))
Period = matrix(nrow=length(data), ncol=31)
for (i in 1:length(data)) {
    Period[i,] <- sequence(31) + rep(as.numeric(paste0(data[i],"01"))-1, 31)
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

PERIOD<-PERIOD%>%filter(PERIOD != 20200230)
PERIOD<-PERIOD%>%filter(PERIOD != 20200231)
PERIOD<-PERIOD%>%filter(PERIOD != 20200431)
PERIOD<-PERIOD%>%filter(PERIOD >= 20200122)
PERIOD<-PERIOD%>%filter(PERIOD <= as.numeric(fecha_hoy))

PERIOD$PERIOD <- as.character(PERIOD$PERIOD)
eleg$Fecha <- as.character(eleg$Fecha)
canal <- unique(eleg$canal)
canal <- as.data.frame(canal)

trial3 <- eleg[,c("FAM_Desc","BU_Desc")]
trial3 <- trial3 %>% dplyr::filter(!(FAM_Desc==""))
trial3 <- trial3 %>% distinct()
trial3 <- merge(trial3,PERIOD)
trial3 <- merge(trial3,canal)
trial3 <- merge(trial3,eleg[,c("FAM_Desc","BU_Desc","canal","Fecha","SI")],
                by.x = c("FAM_Desc","BU_Desc", "canal","PERIOD"), by.y = c("FAM_Desc","BU_Desc","canal","Fecha"),all=T)
trial3 <- trial3 %>% dplyr::filter(!(FAM_Desc==""))
trial3 <- trial3 %>% distinct()
trial3$SI <- as.numeric(as.character(trial3$SI))
trial3$SI[is.na(trial3$SI) == T] <- 0

#################################################################################

t1 <- trial3
t1$Año <- substr(t1$PERIOD,1,4)
t1$Mes <- substr(t1$PERIOD,5,6)
t1$Dia <- substr(t1$PERIOD,7,8)
t1$Semana <- ceiling(as.numeric(t1$Dia)/7.75)
t1$Date<-paste(t1$Año,t1$Mes,t1$Dia,sep="-")
t1$Date<-as.Date(t1$Date,tryFormats = "%Y-%m-%d")
t1$Tipo_Dia <-  weekdays(as.Date(t1$Date))
t1 <- subset(t1,as.numeric(as.character(t1$PERIOD))>=20200123)
t1 <- unique(t1)


coro$Dia <- substr(coro$ï..Fecha,3,4)
for (i in 1:length(coro$Dia)) {
    if (substr(coro$Dia[i],1,1) == "/") {
        coro$Dia[i] <- substr(coro$ï..Fecha[i],4,5)
    }else{coro$Dia[i] = coro$Dia[i]}
}

coro$Mes <- substr(coro$ï..Fecha,1,1)
for (i in 1:length(coro$Mes)) {
    if (substr(coro$Mes[i],1,1) == "0") {
        coro$Mes[i] <- substr(coro$ï..Fecha[i],2,2)
    }else{coro$Mes[i] = coro$Mes[i]}
    coro$Mes[i] <- paste0("0",coro$Mes[i])
}
substrRight <- function(x, n){
    substr(x, nchar(x)-n+1, nchar(x))
}


coro$Año <- substrRight(coro$ï..Fecha,4)
for (i in 1:length(coro$Año)){
if (substr(coro$Año[i],2,2) == "/") {
    coro$Año[i] <- "2020"
}
}

coro$Date<-paste(coro$Año,coro$Mes,coro$Dia,sep="-")
coro$Date<-as.Date(coro$Date,tryFormats = "%Y-%m-%d")

t2<- merge(t1,coro[,c(2:5,9)], by = c("Date"),all.x = T)
t2<-as.data.frame(t2)





####
#output 
beta_normal <- NULL
beta_COVID <- NULL
alpha_normal <- NULL
alpha_COVID <- NULL
pvalue_normal <- NULL
pvalue_COVID <- NULL
correl <- NULL
correl_pvalue <- NULL
family <- NULL
cambio_beta <- NULL
unique(t2$FAM_Desc)
t2$clave <- paste0(t2$FAM_Desc,"_",t2$canal)
t2<-t2%>%filter(FAM_Desc!= "LA Easter Kids")
t2<-t2%>%filter(FAM_Desc!= "LA Easter Pre Season")
t2<-t2%>%filter(FAM_Desc!= "LA Lacta Gifting")
t2<-t2%>%filter(FAM_Desc!= "NA")
t2<-t2%>%filter(FAM_Desc!= "LA Easter Adults")

t2$Dia <- as.numeric(t2$Dia)
for (i in 1:length(levels(as.factor(t2$clave)))) {

    temp1 <- subset(t2,t2$clave == levels(as.factor(t2$clave))[i])
    if (length(temp1$Date)<4) {
        beta_normal[i] <- "NA"
        beta_COVID[i] <- "NA"
        alpha_normal[i] <- "NA"
        alpha_COVID[i] <- "NA"
        pvalue_normal[i] <- "NA"
        pvalue_COVID[i] <- "NA"
        correl[i] <- "NA"
        correl_pvalue[i] <- "NA"
        family[i] <- temp1$clave[1]
        cambio_beta[i] <- "NA"
    }else{
    temp1 <- subset(t2,t2$clave == levels(as.factor(t2$clave))[i])
    normal <-  subset(temp1,as.numeric(temp1$Infectados) <= 0)
    #antes
    m1<-lm(SI~Dia, normal)
    alpha_normal[i] <- m1$coefficients[1] #alpha
    beta_normal[i]<- m1$coefficients[2] #beta
    pvalue_normal[i] <- summary(m1)$coefficient[2,4] #pvalue beta

    #COVID
    m2<-lm(SI~Dia, temp1)
    alpha_COVID[i]<- m2$coefficients[1] #alpha
    beta_COVID[i]<- m2$coefficients[2] #beta
    pvalue_COVID[i]<- summary(m2)$coefficient[2,4] #pvalue beta
    
    #correlation
    cor1<- cor.test(temp1$SI,temp1$Infectados)
    correl[i] <- cor1$estimate
    correl_pvalue[i] <- cor1$p.value
    
    #family
    family[i] <- levels(as.factor(temp1$clave))[1]
    
    #cambio
    if (length(temp1$Date)<4) {
        cambio_beta[i] <- "NA"
    }else{cambio_beta[i] <- ((as.numeric(beta_COVID[i]) - as.numeric(beta_normal[i]))/as.numeric(beta_normal[i]))*100}
}
}

resultados <- data.frame(beta_normal,
                beta_COVID,
                alpha_normal,
                alpha_COVID,
                pvalue_normal,
                pvalue_COVID,
                correl,
                correl_pvalue,
                family,
                cambio_beta)

final_SI<- merge(t2[,-7],resultados,by.x="clave" ,by.y="family",all.x=T)
final_SI$COVID <- ifelse(final_SI$Infectados>=1, 1, 0)
names(final_SI)

#large, small y royal baking
final_SI$PAIS <- PAIS
final_SI<- final_SI[,c(26,4,3,5,6,10,11,12,25,15,14,13,7)]

###################
#################
################
write.csv(final_SI, "base_SI.csv")






######################################Sell out###################################################################
final_SO_pre <- read_excel("FINAL IBP FAMILY ADJUST.xlsx")

final_SO <- aggregate(Sell_Out_KG~Channel+`IBP Family`+Date, final_SO_pre, sum)

sum(final_SO$Sell_Out_KG)

final_SISO <- merge(final_SI, final_SO, by.x = c("Fecha", "FAM_Desc", "canal"), by.y = c("Date","IBP Family","Channel"), all.x = T)

sum(final_SISO$Sell_Out_KG, na.rm = T)


auto.arima(t2$SI,stepwise = F,approximation = F)

yourGreatData <- ts(t2,start = c(2020,01,23),end = c(2020,03,23),
                    frequency = 7)

model2 <- lm(t2$SI~ as.factor(t2$canal)*as.factor(t2$Tipo_Restriccion)
             +
                 as.factor(t2$Semana)*as.numeric(t2$Infectados)+as.factor(t2$canal)*as.numeric(t2$Infectados)
             +
                 as.factor(t2$canal)*as.numeric(t2$Recuperados)
             +
                 as.factor(t2$canal)*as.numeric(t2$Muertes))

summary(model2)
