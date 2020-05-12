library(dplyr)
setwd("C:\\Users\\OYO9864\\OneDrive - MDLZ\\tables")


#LOAD BWP
con <- dbConnect(odbc(),
                 dsn = "",
                 uid = "",
                 pwd = "")

sql_query <- read_file("sell_in_br.txt")
sql_query <- gsub(pattern = "SalesOrg1", replace = 'BR02' ,sql_query)
sql_query <- gsub(pattern = "SalesOrg2", replace = 'BR04' ,sql_query) 
sql_query <- gsub(pattern = "SalesOrg3", replace = 'BR42' ,sql_query) 
sql_query <- gsub(pattern = "HotPeriod_Superior_<=", replace = "2019",sql_query)
sql_query <- gsub(pattern = "HotPeriod_Inferior_>=", replace = "2018",sql_query)
sql_query <- gsub(pattern = "ColdPeriod_Superior_<=", replace = "2017",sql_query)
sql_query <- gsub(pattern = "ColdPeriod_Inferior_>=", replace = "2014",sql_query)
BWP<- odbc::dbGetQuery(con,sql_query)
summary(as.numeric(BWP$PERIOD))





#sql_hier <- read_file("query_clients_hier.sql")
#BWP3<- odbc::dbGetQuery(con,sql_hier)

BWP$TONS_BWP <- as.numeric(BWP$SELLIN_KG)/1000
sum(BWP$TONS_BWP)
#pcust
names(Pcust)
names(BWP)
sql_query <- read_file("PCUSTOMER_br.sql")
Pcust <- odbc::dbGetQuery(con,sql_query)

BWP$YEAR <- substr(BWP$PERIOD,1,4)
BWP_agg <- summaryBy(TONS_BWP ~ SOLD_TO,data =BWP ,FUN=sum)
Pcust_BWP <- merge(x = Pcust, y = BWP_agg, by.x = c("CUSTOMER","SALESORG","DISTR_CHAN") , by.y= c("SOLD_TO","SALESORG","DISTR_CHAN"), all = T)
names(Pcust_BWP)

which(table(Pcust_BWP$SOLD_TO)>=2)


#CITY 1 Y 2 est·N vacio Y LAT LONG = 0.000000


#pcust_sales
names(Pcust_sales)
names(BWP)
sql_query <- read_file("PCUST_SALES_br.sql")
Pcust_sales <- odbc::dbGetQuery(con,sql_query)

Pcust_sales$CUSTOMER
BWP_agg <- summaryBy(TONS_BWP ~ SOLD_TO + SALESORG + DISTR_CHAN,data =BWP ,FUN=sum)

Pcust_sales_BWP <- merge(x = Pcust_sales, y = BWP_agg, by.y = c("SOLD_TO","SALESORG","DISTR_CHAN"), by.x = c("CUSTOMER","SALESORG", "DISTR_CHAN"), all = T)
names(Pcust_sales_BWP)

which(table(Pcust_sales_BWP$SOLD_TO)>=2)
which(table(Pcust_sales_BWP$CUST_SALES)>=2)
Pcust_sales_BWP <- unique(Pcust_sales_BWP)



sql_query <- read_file("PZCUSTOMER_br.sql")
Pzcust<- odbc::dbGetQuery(con,sql_query)
names(Pzcust)
names(BWP)
Pzcust$CUSTOMER
BWP_agg <- summaryBy(TONS_BWP ~ SOLD_TO + SALESORG + DISTR_CHAN,data =BWP ,FUN=sum)
#BWP_agg$SOLD_TO <- substr(BWP_agg$SOLD_TO,2,length(BWP_agg$SOLD_TO))
Pzcust_BWP <- merge(x = Pzcust, y = BWP_agg, by.y = c("SOLD_TO","SALESORG", "DISTR_CHAN"), by.x = c("CUSTOMER","SALESORG", "DISTR_CHAN"), all = T)
names(Pzcust_BWP)

which(table(Pzcust_BWP$CUSTOMER)>=2)

Pzcust_BWP$Origin <- "PZCUSTOMER"
Pcust_sales_BWP$Origin <- "PCUST_SALES"
Pcust_BWP$Origin <- "PCUSTOMER"
names(Pzcust_BWP)
names(Pcust_sales_BWP)
str(Pzcust_BWP)
str(Pcust_sales_BWP)
#Pzcust_BWP$CUSTOMER <- substr(Pzcust_BWP$CUSTOMER,2,length(Pzcust_BWP$CUSTOMER))

###merges
#Pcust_sales_BWP$CUSTOMER <- substr(Pcust_sales_BWP$CUSTOMER,2,length(Pcust_sales_BWP$CUSTOMER))

merge_1 <- merge(Pzcust_BWP, Pcust_sales_BWP, by =c("CUSTOMER","SALESORG", "DISTR_CHAN") ,all = T)
names(merge_1)
names(Pcust_BWP)
str(Pcust_BWP)
str(merge_1)
merge_2 <- merge(merge_1, Pcust_BWP, by =c("CUSTOMER","SALESORG", "DISTR_CHAN") ,all.x = T)

which(table(merge_2$CUSTOMER)>=6)

write.csv(merge_2, "C:\\Users\\OYO9864\\Desktop\\stack_BR.csv")
length(unique(merge_2$CUSTOMER))
names(merge_2)
colnames(merge_2) <-  c("CUSTOMER","SALESORG","DISTR_CHAN","X.PLANT", "X.BIC.ZCUSTOMER", "X.BIC.ZCAHIER01", "X.BIC.ZCAHIER02", "X.BIC.ZCAHIER03",
                        "X.BIC.ZCAHIER04",  "X.BIC.ZCAHIER05",  "X.BIC.ZCAHIER06",  "X.BIC.ZCGHIER01",  "X.BIC.ZCGHIER02",  "X.BIC.ZCGHIER03",
                        "X.BIC.ZCGHIER04",  "X.BIC.ZCGHIER05", "X.BIC.ZCGHIER06",  "TONS_BWP.sum.x",  "Origin.x","Y.BIC.ZCUSTSLV1", "Y.BIC.ZCUSTSLV2",
                        "Y.CUST_SALES","CUST_NAME" ,"TONS_BWP.sum.y", "Origin.y",       
                        "Z.CUSTOMER", "Z.CITY","Z.CITY_2","Z.LATITUDE","Z.LONGITUDE", "Z.NIELSEN_ID","Z.POSTAL_CD", "Z.SORTL",          
                        "Z.STREET", "Z.TAX_NUMB","Z.BIC.ZCITY", "Z.BIC.ZNAME3","Z.BIC.ZDEL_PLNT","Z.BIC.ZCUSTOMER" ,"TONS_BWP.sum.z","Origin.z")

#filtro if ok y not ok
for (i in 1:length(merge_2$CUSTOMER)) {
    if (is.na(merge_2$TONS_BWP.sum.x[i]) == T & is.na(merge_2$TONS_BWP.sum.y[i])== T & is.na(merge_2$TONS_BWP.sum.z[i])==T) {
        merge_2$Sellin_filter[i] <- "NOT OK"
    } else{merge_2$Sellin_filter[i] <- "OK"}
    if (is.na(merge_2$X.BIC.ZCAHIER01[i]) == T & is.na(merge_2$X.BIC.ZCAHIER02[i]) == T &
        is.na(merge_2$X.BIC.ZCAHIER03[i]) == T & is.na(merge_2$X.BIC.ZCAHIER04[i]) == T &
        is.na(merge_2$X.BIC.ZCAHIER05[i]) == T & is.na(merge_2$X.BIC.ZCAHIER06[i]) == T |
        merge_2$X.BIC.ZCAHIER01[i] == "" & merge_2$X.BIC.ZCAHIER02[i] == "" &
        merge_2$X.BIC.ZCAHIER03[i] == "" & merge_2$X.BIC.ZCAHIER04[i] == "" &
        merge_2$X.BIC.ZCAHIER05[i] == "" & merge_2$X.BIC.ZCAHIER06[i] == "") {
        merge_2$Hier_A_filter[i] <- "NOT OK"
        
    }else{merge_2$Hier_A_filter[i] <- "OK"}
    
    if (is.na(merge_2$X.BIC.ZCGHIER01[i]) == T & is.na(merge_2$X.BIC.ZCGHIER02[i]) == T &
        is.na(merge_2$X.BIC.ZCGHIER03[i]) == T & is.na(merge_2$X.BIC.ZCGHIER04[i]) == T &
        is.na(merge_2$X.BIC.ZCGHIER05[i]) == T & is.na(merge_2$X.BIC.ZCGHIER06[i]) == T |
        merge_2$X.BIC.ZCGHIER01[i] == "" & merge_2$X.BIC.ZCGHIER02[i] == "" &
        merge_2$X.BIC.ZCGHIER03[i] == "" & merge_2$X.BIC.ZCGHIER04[i] == "" &
        merge_2$X.BIC.ZCGHIER05[i] == "" & merge_2$X.BIC.ZCGHIER06[i] == "") {
        merge_2$Hier_G_filter[i] <- "NOT OK"
        
    }else{merge_2$Hier_G_filter[i] <- "OK"}
    
    if (is.na(merge_2$Y.BIC.ZCUSTSLV1[i]) == T & is.na(merge_2$Y.BIC.ZCUSTSLV2[i]) == T |
        merge_2$Y.BIC.ZCUSTSLV1[i] == "" & merge_2$Y.BIC.ZCUSTSLV2[i] == "") {
        merge_2$Hier_APO_filter[i] <- "NOT OK"
        
    }else{merge_2$Hier_APO_filter[i] <- "OK"}
}


# x = list()
# x1 = list()
# x2 = list()
# hierA01 = list()
# basura = list()
# customer = list()
# #basura si hier no es ˙nica por costumer
# for(i in 1:length(levels(as.factor(merge_2$CUSTOMER)))){
#     customer1<- subset(merge_2, as.factor(CUSTOMER) == levels(as.factor(merge_2$CUSTOMER))[i])
#     customer2 <- subset(customer1, Hier_A_filter == "OK")
#  #HIER A
#     if (length(customer2$Hier_A_filter)>=2) {
#      for (k in 1:length(customer2$Hier_A_filter)) {
#          if (k+1 <= length(customer2$Hier_A_filter)) {
#              if (as.character(customer2$X.BIC.ZCAHIER01[k+1]) != as.character(customer2$X.BIC.ZCAHIER01[k])) {
#                  hierA01[[k]] <- customer2$X.BIC.ZCAHIER01[k]
#                  basura[[k]] <- "BASURA"
#                  customer[[k]] <- customer2$CUSTOMER[k]
# 
#              }else{hierA01[[k]] <- customer2$X.BIC.ZCAHIER01[k]
#                     basura[[k]] <- "OK"
#                     customer[[k]] <- customer2$CUSTOMER[k]}
#          }
#      }
#     }
#     x[[i]] <- hierA01[1]
#     x1[[i]] <- basura[1]
#     x2[[i]] <- customer[1]
#     #print(x)
#     #print(x1)
#     #print(x2)
# }




##########Vacio A
#a01
library(data.table)
merge_3 = merge_2
setDT(merge_3)
merge_3$X.BIC.ZCAHIER01 <- as.character(merge_3$X.BIC.ZCAHIER01)
merge_3$X.BIC.ZCAHIER01[merge_3$X.BIC.ZCAHIER01 == ""] <- NA
merge_3$Vacio_a01 <- merge_3$X.BIC.ZCAHIER01
merge_3[, Vacio_a01 := if(uniqueN(Vacio_a01) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_a01[is.na(merge_3$X.BIC.ZCAHIER01)] <- "Vacio"


#a02
merge_3$X.BIC.ZCAHIER02 <- as.character(merge_3$X.BIC.ZCAHIER02)
merge_3$X.BIC.ZCAHIER02[merge_3$X.BIC.ZCAHIER02 == ""] <- NA
merge_3$Vacio_a02 <- merge_3$X.BIC.ZCAHIER02

merge_3[, Vacio_a02 := if(uniqueN(Vacio_a02) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_a02[is.na(merge_3$X.BIC.ZCAHIER02)] <- "Vacio"


#a03
merge_3$X.BIC.ZCAHIER03 <- as.character(merge_3$X.BIC.ZCAHIER03)
merge_3$X.BIC.ZCAHIER03[merge_3$X.BIC.ZCAHIER03 == ""] <- NA
merge_3$Vacio_a03 <- merge_3$X.BIC.ZCAHIER03

merge_3[, Vacio_a03 := if(uniqueN(Vacio_a03) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_a03[is.na(merge_3$X.BIC.ZCAHIER03)] <- "Vacio"

#a04
merge_3$X.BIC.ZCAHIER04 <- as.character(merge_3$X.BIC.ZCAHIER04)
merge_3$X.BIC.ZCAHIER04[merge_3$X.BIC.ZCAHIER04 == ""] <- NA
merge_3$Vacio_a04 <- merge_3$X.BIC.ZCAHIER04


merge_3[, Vacio_a04 := if(uniqueN(Vacio_a04) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_a04[is.na(merge_3$X.BIC.ZCAHIER04)] <- "Vacio"

#a05
merge_3$X.BIC.ZCAHIER05 <- as.character(merge_3$X.BIC.ZCAHIER05)
merge_3$X.BIC.ZCAHIER05[merge_3$X.BIC.ZCAHIER05 == ""] <- NA
merge_3$Vacio_a05 <- merge_3$X.BIC.ZCAHIER05


merge_3[, Vacio_a05 := if(uniqueN(Vacio_a05) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_a05[is.na(merge_3$X.BIC.ZCAHIER05)] <- "Vacio"

#a06
merge_3$X.BIC.ZCAHIER06 <- as.character(merge_3$X.BIC.ZCAHIER06)
merge_3$X.BIC.ZCAHIER06[merge_3$X.BIC.ZCAHIER06 == ""] <- NA
merge_3$Vacio_a06 <- merge_3$X.BIC.ZCAHIER06

merge_3[, Vacio_a06 := if(uniqueN(Vacio_a06) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_a06[is.na(merge_3$X.BIC.ZCAHIER06)] <- "Vacio"




#######Vacio G
#g01
merge_3$X.BIC.ZCGHIER01 <- as.character(merge_3$X.BIC.ZCGHIER01)
merge_3$X.BIC.ZCGHIER01[merge_3$X.BIC.ZCGHIER01 == ""] <- NA
merge_3$Vacio_g01 <- merge_3$X.BIC.ZCGHIER01

merge_3[, Vacio_g01 := if(uniqueN(Vacio_g01) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_g01[is.na(merge_3$X.BIC.ZCGHIER01)] <- "Vacio"

#g02
merge_3$X.BIC.ZCGHIER02 <- as.character(merge_3$X.BIC.ZCGHIER02)
merge_3$X.BIC.ZCGHIER02[merge_3$X.BIC.ZCGHIER02 == ""] <- NA
merge_3$Vacio_g02 <- merge_3$X.BIC.ZCGHIER02


merge_3[, Vacio_g02 := if(uniqueN(Vacio_g02) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_g02[is.na(merge_3$X.BIC.ZCGHIER02)] <- "Vacio"


#g03
merge_3$X.BIC.ZCGHIER03 <- as.character(merge_3$X.BIC.ZCGHIER03)
merge_3$X.BIC.ZCGHIER03[merge_3$X.BIC.ZCGHIER03 == ""] <- NA
merge_3$Vacio_g03 <- merge_3$X.BIC.ZCGHIER03


merge_3[, Vacio_g03 := if(uniqueN(Vacio_g03) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_g03[is.na(merge_3$X.BIC.ZCGHIER03)] <- "Vacio"

#g04
merge_3$X.BIC.ZCGHIER04 <- as.character(merge_3$X.BIC.ZCGHIER04)
merge_3$X.BIC.ZCGHIER04[merge_3$X.BIC.ZCGHIER04 == ""] <- NA
merge_3$Vacio_g04 <- merge_3$X.BIC.ZCGHIER04


merge_3[, Vacio_g04 := if(uniqueN(Vacio_g04) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_g04[is.na(merge_3$X.BIC.ZCGHIER04)] <- "Vacio"


#g05
merge_3$X.BIC.ZCGHIER05 <- as.character(merge_3$X.BIC.ZCGHIER05)
merge_3$X.BIC.ZCGHIER05[merge_3$X.BIC.ZCGHIER05 == ""] <- NA
merge_3$Vacio_g05 <- merge_3$X.BIC.ZCGHIER05


merge_3[, Vacio_g05 := if(uniqueN(Vacio_g05) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_g05[is.na(merge_3$X.BIC.ZCGHIER05)] <- "Vacio"


#g06
merge_3$X.BIC.ZCGHIER06 <- as.character(merge_3$X.BIC.ZCGHIER06)
merge_3$X.BIC.ZCGHIER06[merge_3$X.BIC.ZCGHIER06 == ""] <- NA
merge_3$Vacio_g06 <- merge_3$X.BIC.ZCGHIER06


merge_3[, Vacio_g06 := if(uniqueN(Vacio_g06) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_g06[is.na(merge_3$X.BIC.ZCGHIER06)] <- "Vacio"


#hacer lo mismo para city y dem·s
#"Z.SORTL"
merge_3$Z.SORTL <- as.character(merge_3$Z.SORTL)
merge_3$Z.SORTL[merge_3$Z.SORTL == ""] <- NA
merge_3$Vacio_Z.SORTL <- merge_3$Z.SORTL


merge_3[, Vacio_Z.SORTL := if(uniqueN(Vacio_Z.SORTL) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.SORTL[is.na(merge_3$Z.SORTL)] <- "Vacio"


#"Z.STREET"         
merge_3$Z.STREET <- as.character(merge_3$Z.STREET)
merge_3$Z.STREET[merge_3$Z.STREET == ""] <- NA
merge_3$Vacio_Z.STREET <- merge_3$Z.STREET


merge_3[, Vacio_Z.STREET := if(uniqueN(Vacio_Z.STREET) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.STREET[is.na(merge_3$Z.STREET)] <- "Vacio"


#"Z.TAX_NUMB"
merge_3$Z.TAX_NUMB <- as.character(merge_3$Z.TAX_NUMB)
merge_3$Z.TAX_NUMB[merge_3$Z.TAX_NUMB == ""] <- NA
merge_3$Vacio_Z.TAX_NUMB <- merge_3$Z.TAX_NUMB


merge_3[, Vacio_Z.TAX_NUMB := if(uniqueN(Vacio_Z.TAX_NUMB) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.TAX_NUMB[is.na(merge_3$Z.TAX_NUMB)] <- "Vacio"


#"Z.BIC.ZCITY"
merge_3$Z.BIC.ZCITY <- as.character(merge_3$Z.BIC.ZCITY)
merge_3$Z.BIC.ZCITY[merge_3$Z.BIC.ZCITY == ""] <- NA
merge_3$Vacio_Z.BIC.ZCITY <- merge_3$Z.BIC.ZCITY


merge_3[, Vacio_Z.BIC.ZCITY := if(uniqueN(Vacio_Z.BIC.ZCITY) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.BIC.ZCITY[is.na(merge_3$Z.BIC.ZCITY)] <- "Vacio"


#"Z.BIC.ZNAME3"
merge_3$Z.BIC.ZNAME3 <- as.character(merge_3$Z.BIC.ZNAME3)
merge_3$Z.BIC.ZNAME3[merge_3$Z.BIC.ZNAME3 == ""] <- NA
merge_3$Vacio_Z.BIC.ZNAME3 <- merge_3$Z.BIC.ZNAME3


merge_3[, Vacio_Z.BIC.ZNAME3 := if(uniqueN(Vacio_Z.BIC.ZNAME3) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.BIC.ZNAME3[is.na(merge_3$Z.BIC.ZNAME3)] <- "Vacio"


#"Z.BIC.ZDEL_PLNT"  
merge_3$Z.BIC.ZDEL_PLNT <- as.character(merge_3$Z.BIC.ZDEL_PLNT)
merge_3$Z.BIC.ZDEL_PLNT[merge_3$Z.BIC.ZDEL_PLNT == ""] <- NA
merge_3$Vacio_Z.BIC.ZDEL_PLNT <- merge_3$Z.BIC.ZDEL_PLNT


merge_3[, Vacio_Z.BIC.ZDEL_PLNT := if(uniqueN(Vacio_Z.BIC.ZDEL_PLNT) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.BIC.ZDEL_PLNT[is.na(merge_3$Z.BIC.ZDEL_PLNT)] <- "Vacio"

#"Z.BIC.ZCUSTOMER"
merge_3$Z.BIC.ZCUSTOMER <- as.character(merge_3$Z.BIC.ZCUSTOMER)
merge_3$Z.BIC.ZCUSTOMER[merge_3$Z.BIC.ZCUSTOMER == ""] <- NA
merge_3$Vacio_Z.BIC.ZCUSTOMER <- merge_3$Z.BIC.ZCUSTOMER


merge_3[, Vacio_Z.BIC.ZCUSTOMER := if(uniqueN(Vacio_Z.BIC.ZCUSTOMER) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_Z.BIC.ZCUSTOMER[is.na(merge_3$Z.BIC.ZCUSTOMER)] <- "Vacio"

#si no es unica: facturo o no
#"TONS_BWP.sum.x"
merge_3$TONS_BWP.sum.x <- as.character(merge_3$TONS_BWP.sum.x)
merge_3$TONS_BWP.sum.x[merge_3$TONS_BWP.sum.x == ""] <- NA
merge_3$Vacio_TONS_BWP.sum.x <- merge_3$TONS_BWP.sum.x


merge_3[, Vacio_TONS_BWP.sum.x := if(uniqueN(Vacio_TONS_BWP.sum.x) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_TONS_BWP.sum.x[is.na(merge_3$TONS_BWP.sum.x)] <- "Vacio"
merge_3$Vacio_TONS_BWP.sum.x


#"TONS_BWP.sum.y"
merge_3$TONS_BWP.sum.y <- as.character(merge_3$TONS_BWP.sum.y)
merge_3$TONS_BWP.sum.y[merge_3$TONS_BWP.sum.y == ""] <- NA
merge_3$Vacio_TONS_BWP.sum.y <- merge_3$TONS_BWP.sum.y


merge_3[, Vacio_TONS_BWP.sum.y := if(uniqueN(Vacio_TONS_BWP.sum.y) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_TONS_BWP.sum.y[is.na(merge_3$TONS_BWP.sum.y)] <- "Vacio"
merge_3$Vacio_TONS_BWP.sum.y


#"TONS_BWP.sum.z"
merge_3$TONS_BWP.sum.z <- as.character(merge_3$TONS_BWP.sum.z)
merge_3$TONS_BWP.sum.z[merge_3$TONS_BWP.sum.z == ""] <- NA
merge_3$Vacio_TONS_BWP.sum.z <- merge_3$TONS_BWP.sum.z


merge_3[, Vacio_TONS_BWP.sum.z := if(uniqueN(Vacio_TONS_BWP.sum.z) > 2) 'Vacio' else "OK", by=.(CUSTOMER)]
merge_3$Vacio_TONS_BWP.sum.z[is.na(merge_3$TONS_BWP.sum.z)] <- "Vacio"
merge_3$Vacio_TONS_BWP.sum.z

#filtro basura

#relaciÛn entre APO hier A y G que tenga sentido



#filtros
Hier_A_OK <-subset(merge_2, merge_2$Hier_A_filter == "OK")
Hier_G_OK <-subset(merge_2, merge_2$Hier_G_filter == "OK")
Hier_6_OK <-subset(merge_2, merge_2$Hier_6_filter == "OK")
Sellin_OK <-subset(merge_2, merge_2$Sellin_filter == "OK")


names(Sellin_OK)




############### CVC LOGICAL CHECK ########################

A01_A02 <- summaryBy(X.BIC.ZCAHIER02 ~ X.BIC.ZCAHIER01 ,data = Sellin_OK ,FUN=length)
A02_A03 <- summaryBy(X.BIC.ZCAHIER03 ~ X.BIC.ZCAHIER02 ,data = Sellin_OK ,FUN=length)
A03_A04 <- summaryBy(X.BIC.ZCAHIER04 ~ X.BIC.ZCAHIER03 ,data = Sellin_OK ,FUN=length)
A04_A05 <- summaryBy(X.BIC.ZCAHIER05 ~ X.BIC.ZCAHIER04 ,data = Sellin_OK ,FUN=length)
A05_A06 <- summaryBy(X.BIC.ZCAHIER06 ~ X.BIC.ZCAHIER05 ,data = Sellin_OK ,FUN=length)

ch1<-A01_A02[which(A01_A02[,2]>1),]
ch2<-A02_A03[which(A02_A03[,2]>1),]
ch3<-A03_A04[which(A03_A04[,2]>1),]
ch4<-A04_A05[which(A04_A05[,2]>1),]
ch5<-A05_A06[which(A05_A06[,2]>1),]


if (nrow(ch1[1]) == 0) {
    print("Hier_A01 with Hier_A02 is correct")
}else if (nrow(ch1[1]) != 0) {
    for (i in 1:length(ch1[,1])) {
        print("Check Hier_A01 with Hier_A02")
        print(as.character(Sellin_OK[which(A01_A02$X.BIC.ZCAHIER01 == ch1[i,1]),7]))
    }
}

if (nrow(ch2[1]) == 0) {
    print("Hier_A02 with Hier_A03 is correct")
}else if (nrow(ch2[1]) != 0) {
    for (i in 1:length(ch2[,1])) {
        print("Check Hier_A02 with Hier_A03")
        print(as.character(Sellin_OK[which(A02_A03$X.BIC.ZCAHIER02 == ch2[i,1]),8]))
    }
}

if (nrow(ch3[1]) == 0) {
    print("Hier_A03 with Hier_A04 is correct")
}else if (nrow(ch3[1]) != 0) {
    for (i in 1:length(ch3[,1])) {
        print("Check Hier_A03 with Hier_A04")
        print(as.character(Sellin_OK[which(A03_A04$X.BIC.ZCAHIER03 == ch3[i,1]),9]))
    }
}
if (nrow(ch4[1]) == 0) {
    print("Hier_A04 with Hier_A05 is correct")
}else if (nrow(ch4[1]) != 0) {
    for (i in 1:length(ch4[,1])) {
        print("Check Hier_A04 with Hier_A05")
        print(as.character(Sellin_OK[which(A04_A05$X.BIC.ZCAHIER04 == ch4[i,1]),10]))
    }
}

if (nrow(ch5[1]) == 0) {
    print("Hier_A05 with Hier_A06 is correct")
}else if (nrow(ch5[1]) != 0) {
    for (i in 1:length(ch5[,1])) {
        print("Check Hier_A05 with Hier_A06")
        print(as.character(Sellin_OK[which(A05_A06$X.BIC.ZCAHIER05 == ch5[i,1]),11]))
    }
}

#Hierachy G
G01_G02 <- summaryBy(X.BIC.ZCGHIER02 ~ X.BIC.ZCGHIER01 ,data = Sellin_OK ,FUN=length)
G02_G03 <- summaryBy(X.BIC.ZCGHIER03 ~ X.BIC.ZCGHIER02 ,data = Sellin_OK ,FUN=length)
G03_G04 <- summaryBy(X.BIC.ZCGHIER04 ~ X.BIC.ZCGHIER03 ,data = Sellin_OK ,FUN=length)
G04_G05 <- summaryBy(X.BIC.ZCGHIER05 ~ X.BIC.ZCGHIER04 ,data = Sellin_OK ,FUN=length)
G05_G06 <- summaryBy(X.BIC.ZCGHIER06 ~ X.BIC.ZCGHIER05 ,data = Sellin_OK ,FUN=length)

ch1<-G01_G02[which(G01_G02[,2]>1),]
ch2<-G02_G03[which(G02_G03[,2]>1),]
ch3<-G03_G04[which(G03_G04[,2]>1),]
ch4<-G04_G05[which(G04_G05[,2]>1),]
ch5<-G05_G06[which(G05_G06[,2]>1),]

if (nrow(ch1[1]) == 0) {
    print("Hier_G01 with Hier_G02 is correct")
}else if (nrow(ch1[1]) != 0) {
    for (i in 1:length(ch1[,1])) {
        print("Check Hier_G01 with Hier_G02")
        print(as.character(Sellin_OK[which(G01_G02$X.BIC.ZCGHIER01 == ch1[i,1]),13]))
    }
}

if (nrow(ch2[1]) == 0) {
    print("Hier_G02 with Hier_G03 is correct")
}else if (nrow(ch2[1]) != 0) {
    for (i in 1:length(ch2[,1])) {
        print("Check Hier_G02 with Hier_G03")
        print(as.character(Sellin_OK[which(G02_G03$X.BIC.ZCGHIER02 == ch2[i,1]),14]))
    }
}

if (nrow(ch3[1]) == 0) {
    print("Hier_G03 with Hier_G04 is correct")
}else if (nrow(ch3[1]) != 0) {
    for (i in 1:length(ch3[,1])) {
        print("Check Hier_G03 with Hier_G04")
        print(as.character(Sellin_OK[which(G03_G04$X.BIC.ZCGHIER03 == ch3[i,1]),15]))
    }
}
if (nrow(ch4[1]) == 0) {
    print("Hier_G04 with Hier_G05 is correct")
}else if (nrow(ch4[1]) != 0) {
    for (i in 1:length(ch4[,1])) {
        print("Check Hier_G04 with Hier_G05")
        print(as.character(Sellin_OK[which(G04_G05$X.BIC.ZCGHIER04 == ch4[i,1]),16]))
    }
}

if (nrow(ch5[1]) == 0) {
    print("Hier_G05 with Hier_G06 is correct")
}else if (nrow(ch5[1]) != 0) {
    for (i in 1:length(ch5[,1])) {
        print("Check Hier_G05 with Hier_G06")
        print(as.character(Sellin_OK[which(G05_G06$X.BIC.ZCGHIER05 == ch5[i,1]),17]))
    }
}





##############################
summary(BWP$PERIOD)
BWP$YEAR <- as.numeric(BWP$PERIOD)
BWP$SOLD_TO2 <- substr(BWP$SOLD_TO,2,length(BWP$SOLD_TO))
names(BWP)
names(BWP_agg)
BWP_agg <-  summaryBy(TONS_BWP ~ SALESORG+ DISTR_CHAN+ SOLD_TO2+YEAR , data = BWP, FUN = sum)

#156047163
data_wide <- BWP_agg %>% 
    group_by(TONS_BWP.sum) %>% 
    mutate(grouped_id = row_number())

data_wide <-data_wide %>% 
    spread(YEAR, TONS_BWP.sum) %>% 
    select(-grouped_id)


names(data_wide)
names(itSold)


data_wide2<- data_wide %>% group_by(SOLD_TO2, DISTR_CHAN, SALESORG) %>% summarise_each(funs(sum(., na.rm = TRUE))) 
names(data_wide2)
stack2<- merge(itSold, data_wide2, by.y = c("SOLD_TO2", "DISTR_CHAN", "SALESORG"), by.x=c("CUSTOMER", "DISTR_CHAN", "SALESORG"), all.x = T)



stack2 <- stack2[order(stack2$CUSTOMER),]
which(table(stack2$CUSTOMER)>=2)



doble <- NULL
for (i in 2:length(stack2$CUSTOMER)) {
    if (stack2$CUSTOMER[i] == stack2$CUSTOMER[i-1]) {
        
        doble[i] <- stack2$CUSTOMER[i]
        print(doble[i])
    }
}


doble <- as.data.frame(doble)
doble <-subset(doble, !is.na(doble == T))

names(stack2)
names(doble)
#doble <- merge(doble, stack2, by.x = "doble[-1]", by.y = "CUSTOMER", all.x = T)


length(doble[,1])
length(which(table(stack2$CUSTOMER)>=2))
write.csv(stack2, "C:\\Users\\OYO9864\\Desktop\\stack_BR_vertical.csv")

doble$WARNING <- "Warning"
stack3<- merge(doble, stack2, by.x = "doble", by.y = "CUSTOMER", all = T)
names(stack3)
names(stack3)[names(stack3) == "doble"] <- "CUSTOMER"
stack3_final <- stack3[,c(1,3,4,7,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23,26,27,28,29,30,31,32,33,34,35,36,37,38,41,42,43,44,45,46,47,2)]
names(stack3_final)
write.csv(stack3_final, "C:\\Users\\OYO9864\\Desktop\\limpio_BR.csv")


                    