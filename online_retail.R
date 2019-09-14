
library(dplyr)
library(tidyr)


data<-read.csv('https://raw.githubusercontent.com/ferdeh/machine_learning_3/master/Online%20Retail.csv',na.strings = c("","NA"))
head(data)

attach(data)

any(is.na(data))

apply(data, 2, function(x) any(is.na(x)))
sum(is.na(CustomerID))

sum(is.na(Description))

#The leading and trailing white spaces were removed with the trim function both in InvoiceNo and Description.
data$InvoiceNo = as.character(InvoiceNo)
trim = function (x) gsub("^\\s+|\\s+$", "", x)
data$InvoiceNo = trim(InvoiceNo)
head(InvoiceNo)
dataset$Description = trim(as.character(Description))

#The invoice numbers starting with C are actually cancellations

is_C = function (x) startsWith(x,"C")
data2 = data[which(!is_C(data$InvoiceNo)),] #subsetting
head(data2)

data3 = subset(data2,!is.na(data2$Description)) #subsetting
data3$Description

buzzwords = c("WRONG","LOST", "CRUSHED", "SMASHED", "DAMAGED", "FOUND", "THROWN", "MISSING", "AWAY", "\\?", "CHECK", "POSTAGE", "MANUAL", "CHARGES", "AMAZON", "FEE", "FAULT", "SALES", "ADJUST", "COUNTED", "LABEL", "INCORRECT", "SOLD", "BROKEN", "BARCODE", "CRACKED", "RETURNED", "MAILOUT", "DELIVERY", "MIX UP", "MOULDY", "PUT ASIDE", "ERROR", "DESTROYED", "RUSTY")

library(stringr)   #function str_detect
isUndesirable = function(x){
  c = FALSE   #assume that the string is undesirable (FALSE), and perhaps switch to desirable (TRUE)
  for (i in 1:(length(buzzwords))){
    c = c || ifelse(str_detect(toupper(x),buzzwords[i]),TRUE,FALSE)
  }
  #now we know whether or not the string is undesirable
  return(c)
}


data4 = subset(data3, data$Quantity > 0)


isUndesirable2 = function(x) {
  str_detect(toupper(x),"WRONG") | str_detect(toupper(x),"LOST") |
    str_detect(toupper(x),"CRUSHED") | str_detect(toupper(x),"DAMAGE") |
    str_detect(toupper(x),"FOUND") | str_detect(toupper(x),"THROWN") |
    str_detect(toupper(x),"SMASHED") |
    str_detect(toupper(x),"\\?") |
    str_detect(toupper(x),"AWAY") | str_detect(toupper(x),"CHARGES") |
    str_detect(toupper(x),"FEE") | str_detect(toupper(x),"FAULT")
  str_detect(toupper(x),"SALES") | str_detect(toupper(x),"ADJUST") |
    str_detect(toupper(x),"COUNTED") |
    str_detect(toupper(x),"INCORRECT") |
    str_detect(toupper(x),"BROKEN") | str_detect(toupper(x),"BARCODE") |
    str_detect(toupper(x),"RETURNED") |
    str_detect(toupper(x),"MAILOUT") | str_detect(toupper(x),"DELIVERY") |
    str_detect(toupper(x),"MIX UP") | str_detect(toupper(x),"MOULDY") |
    str_detect(toupper(x),"PUT ASIDE") | str_detect(toupper(x),"ERROR") |
    str_detect(toupper(x),"DESTROYED") | str_detect(toupper(x),"RUSTY")
}

#dataset4 = subset(dataset3, dataset3$Quantity > 0)
data5 = data4[which(!isUndesirable2(as.character(data4$Description))),]

nrow(data5)
Time = format(as.POSIXct(strptime(data5$InvoiceDate,"%Y-%m-%d %H:%M",tz="")) ,format = "%H:%M:%S")
data5$InvoiceDate = as.Date(data5$InvoiceDate)
data5$Description = as.factor(data5$Description)
head(data5)

dataset5<-data5

names(data5)
sum(is.na(data5$CustomerID))
data6<-data5
data6$CustomerID<-as.character(data6$CustomerID)
sum(is.na(data6$CustomerID))

#data preprocessing is done!
#Recode variable

data_clean <- data5 %>% 
  mutate(InvoiceNo=as.factor(InvoiceNo), StockCode=as.factor(StockCode), 
         InvoiceDate=as.Date(InvoiceDate, '%m/%d/%Y %H:%M'), CustomerID=as.factor(CustomerID), 
         Country=as.factor(Country))
data_clean <- data_clean %>% mutate(total_price = Quantity*UnitPrice)
head(data_clean)


#Reformat data to RFM format

data_RFM <- data_clean %>% 
  group_by(CustomerID) %>% 
  summarise(recency=as.numeric(as.Date("2012-01-01")-max(InvoiceDate)),
            frequency=n_distinct(InvoiceNo), monitery= sum(total_price)/n_distinct(InvoiceNo))

summary(data_RFM)
print("Berikut adalah data RFM dari data online retail yang sudah di reformat")
print(head(data_RFM))


###

library(plyr)
library(arules)

items = ddply(dataset5,c("InvoiceNo"), function(x)paste(x$Description, collapse = ","))
head(items)

write.csv(items,"Items_List.csv",quote=FALSE, row.names = TRUE)

#creating the baskets
baskets = read.transactions("Items_List.csv", format='basket',sep=",")
summary(baskets)

#generating the rules
basket_rules = apriori(baskets,parameter = list(sup = 0.005, conf = 0.75))
basket_rules2 = apriori(baskets,parameter = list(sup = 0.01, conf = 0.7,maxlen=3))

basket_rules = sort(basket_rules, by='lift', decreasing = TRUE)
basket_rules2 = sort(basket_rules2, by = 'confidence', decreasing = TRUE)

summary(basket_rules)
inspect(basket_rules[1:10])

#visualizing the obtained rules
library(arulesViz)

#scatterplot
plot(basket_rules)

summary(basket_rules2)
inspect(basket_rules2[1:10])

#graph
plot(basket_rules2[1:10],method="graph")

basket_rules3 = apriori(baskets, parameter=list(supp=0.002,conf = 0.8),
                        appearance = list(default="lhs",rhs="COFFEE"),
                        control = list(verbose=F))
basket_rules3 = sort(basket_rules3, decreasing=TRUE,by="confidence")
summary(basket_rules3)
inspect(basket_rules3[1:5])

basket_rules4 = apriori(baskets, parameter=list(supp=0.01,conf = 0.7),
                        appearance = list(default="rhs",lhs="SUGAR"),
                        control = list(verbose=F))
basket_rules4 = sort(basket_rules4, decreasing=TRUE,by="confidence")
summary(basket_rules4)
inspect(basket_rules4)
###
