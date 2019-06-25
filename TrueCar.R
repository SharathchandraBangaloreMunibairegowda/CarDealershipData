library(plyr)
library(dplyr)
library(ggplot2)
library(treemap)
library(data.table)
library(h2o)
library(tm)
library(SnowballC)
library(stringr)
library(wordcloud)

data <- read.csv("C:/Users/Sharathchandra/Desktop/TrueCar/PriceRatio2.csv", header = T, stringsAsFactors = F, na.strings=c("","NA"))
data <- data[,-c(1:7,13)]
data <- data[!is.na(data$discarded),]
data <- data[!is.na(data$curve_rating),]
data$finance_type[is.na(data$finance_type)] <- "UNKNOWN"
data$model_name[is.na(data$model_name)] <- "UNKNOWN"
data$sale_type[is.na(data$sale_type)] <- "UNKNOWN"
data$truecar_sale <- ifelse(data$truecar_sale == "2","FALSE",
               ifelse(data$truecar_sale == "3","FALSE",
                      ifelse(data$truecar_sale == "4","FALSE",
                             ifelse(data$truecar_sale == "5","FALSE",
                                    ifelse(data$truecar_sale == "6","FALSE",
                                           ifelse(data$truecar_sale == "7","FALSE",data$truecar_sale))))))
data$adjusted_sale_price <- as.numeric(data$adjusted_sale_price)
data$discarded <- as.character(data$discarded)
data$adjusted_sale_price[is.na(data$adjusted_sale_price)] <- 35791.97
data$odometer[is.na(data$odometer)] <- 536
data$price_invoice[is.na(data$price_invoice)] <- 36066.74
data$price_msrp[is.na(data$price_msrp)] <- 37080.58
data$price_retail[is.na(data$price_retail)] <- 36085.36
data$price_ratio <- as.numeric(data$price_ratio)
data$price_ratio[is.na(data$price_ratio)] <- 0.91151
rownames(data) <- NULL

data <- na.omit(data)

columns.factor <- c("dma_id","finance_type","make_name","model_name","model_year","provider_name","sale_type","state","style_name","tc_region_id","truecar_sale","curve_rating","discarded")
data[columns.factor] <- sapply(data[columns.factor],as.factor)
data$dma_id <- as.factor(data$dma_id)
data$finance_type <- as.factor(data$finance_type)
data$make_name <- as.factor(data$make_name)
data$model_name <- as.factor(data$model_name)
data$model_year <- as.factor(data$model_year)
data$provider_name <- as.factor(data$provider_name)
data$sale_type <- as.factor(data$sale_type)
data$state <- as.factor(data$state)
data$style_name <- as.factor(data$style_name)
data$tc_region_id <- as.factor(data$tc_region_id)
data$truecar_sale <- as.factor(data$truecar_sale)
data$curve_rating <- as.factor(data$curve_rating)
data$discarded <- as.factor(data$discarded)
columns.numeric <- c("odometer","price_invoice","price_msrp","price_retail","adjusted_sale_price","price_ratio")
data[columns.numeric] <- sapply(data[columns.numeric],as.numeric)


h2o.data1 <- as.h2o(data, destination_frame = "data1")
targetVariable1 <- "curve_rating"
otherVariables1 <- c("dma_id","finance_type","make_name","model_name","model_year","provider_name","sale_type","state","style_name","tc_region_id","truecar_sale","curve_rating","discarded")
parts1 <- h2o.splitFrame(h2o.data1, 0.8)
train1 <- parts1[[1]]
test1 <- parts1[[2]]
model1 <- h2o.randomForest(otherVariables1, targetVariable1, train1, nfolds = 2, model_id = "RF-Model-1")
predictmodel1 <- h2o.predict(model1,test1)

h2o.data2 <- as.h2o(data, destination_frame = "data2")
targetVariable2 <- "price_ratio"
otherVariables2 <- c("odometer","price_invoice","price_msrp","price_retail","adjusted_sale_price","curve_average_market_price")
parts2 <- h2o.splitFrame(h2o.data2, 0.8)
train2 <- parts2[[1]]
test2 <- parts2[[2]]
model2 <- h2o.glm(otherVariables2, targetVariable2, train2, nfolds = 2,model_id = "GLM-Model-2")
predictmodel2 <- h2o.predict(model2,test2)

h2o.data3 <- as.h2o(data, destination_frame = "data3")
targetVariable3 <- "price_ratio"
otherVariables3 <- c("odometer","price_invoice","price_msrp","price_retail","adjusted_sale_price","curve_average_market_price","make_name","model_name","model_year","sale_type","state","truecar_sale","curve_rating")
parts3 <- h2o.splitFrame(h2o.data3, 0.8)
train3 <- parts3[[1]]
test3 <- parts3[[2]]
model3 <- h2o.randomForest(otherVariables3, targetVariable3, train3, nfolds = 2,model_id = "RF-Model-3")
predictmodel3 <- h2o.predict(model3,test3)

dmaCount <- as.data.frame(table(data$dma_id))
colnames(dmaCount) <- c("dma_id","counts")
dmaCount <- arrange(dmaCount,desc(counts))
dmaCountTop25 <- dmaCount[1:25,]
plot1 <- ggplot(data = dmaCountTop25) + geom_bar(mapping = aes(x=reorder(dma_id,counts),y= counts, fill = counts),stat = "identity", position = "dodge") + labs(x="Top 25 DMA_Id (Cities) data records", y="Number Of Data records", title = "Top 25 DMA_Id (Cities) data records", subtitle = "DMA IDs - Visualization to assist TV Broadcast campaigning team") + theme(panel.background = element_blank()) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip() + geom_text(aes(x=dma_id, y=counts, label=counts), hjust = -0.5, size = 2, inherit.aes = TRUE) + theme_bw()
plot1

financeCount <- as.data.frame(table(data$finance_type))
colnames(financeCount) <- c("finance_type","counts")
plot2 <- ggplot(data = financeCount) + geom_bar(mapping = aes(x=reorder(finance_type,counts),y= counts, fill = counts),stat = "identity", position = "dodge") + labs(x="Finance Types of the cars sold", y="Count of Types of Finance", title = "Count of Finance Types of cars sold", subtitle = "UNKNOWN (arbitrary value as data is missing/blank)") + theme(panel.background = element_blank()) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + geom_text(aes(x=finance_type, y=counts, label=counts), hjust = 0.5, vjust = -1, size = 4, inherit.aes = TRUE) + theme_bw()
plot2

makeNameCount <- as.data.frame(table(data$make_name))
colnames(makeNameCount) <- c("make_name","counts")
makeNameCount <- arrange(makeNameCount,desc(counts))
plot3 <- ggplot(data = makeNameCount) + geom_bar(mapping = aes(x=reorder(make_name,counts),y= counts, fill = counts),stat = "identity", position = "dodge") + labs(x="Car Manufacturers' List", y="Number Of Cars Sold", title = "Number of Cars sold per Manufacturer") + theme(panel.background = element_blank()) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip() + geom_text(aes(x=make_name, y=counts, label=counts), hjust = -0.5, size = 2, inherit.aes = TRUE) + theme_bw()
plot3

modelCount <- data
modelCount$make_model <- paste(modelCount$make_name, modelCount$model_name, sep = "\n")
modelCount <- as.data.frame(table(modelCount$make_model))
colnames(modelCount) <- c("make_model_name","counts")
modelCount <- arrange(modelCount,desc(counts))
modelCountTop25 <- modelCount[1:25,]
plot4 <- ggplot(data = modelCountTop25) + geom_bar(mapping = aes(x=reorder(make_model_name,counts),y= counts, fill = counts),stat = "identity", position = "dodge") + labs(x="Top 25 Cars Sold", y="Number of particular car sold", title = "Top 25 Cars Sold") + theme(panel.background = element_blank()) + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + coord_flip() + geom_text(aes(x=make_model_name, y=counts, label=counts), hjust = -0.5, size = 2, inherit.aes = TRUE) + theme_bw()
plot4

saleTypeCount <- as.data.frame(table(data$sale_type))
colnames(saleTypeCount) <- c("sale_type","counts")
saleTypeCount$percent <- saleTypeCount$counts/sum(saleTypeCount$counts)*100
saleTypeCount$percent <- round(saleTypeCount$percent,2)
saleTypeCount <- arrange(saleTypeCount, desc(percent))
saleTypeCount$label <- paste(saleTypeCount$sale_type,saleTypeCount$percent,sep = "\n")
saleTypeCount$label <- paste(saleTypeCount$label,"%")
treeplot <- treemap(saleTypeCount, index = "label", vSize = "counts", type = "index", palette = "Dark2", title="Sale Types Percentage", fontsize.title=12,fontsize.labels = 10)

provNameCount <- as.data.frame(table(data$provider_name))
colnames(provNameCount) <- c("provider_name","counts")
provNameCount$percent <- provNameCount$counts/sum(provNameCount$counts)*100
provNameCount$percent <- round(provNameCount$percent,2)
provNameCount <- arrange(provNameCount, desc(percent))
provNameCount$label <- paste(provNameCount$provider_name,provNameCount$percent,sep = "\n")
provNameCount$label <- paste(provNameCount$label,"%")
treeplot <- treemap(provNameCount, index = "label", vSize = "counts", type = "index", palette = "Dark2", title="Data records Provider percentage", fontsize.title=12,fontsize.labels = 10)

datastyle_name <- fread("C:/Users/Sharathchandra/Desktop/TrueCar/PriceRatio2.csv", select = c(21), header = T, stringsAsFactors = F)

cleanData <- function(cleanData1){
  callText <- gsub(pattern="\\W", replace = " ", cleanData1)
  callText<- tolower(callText)
  callText <- removeWords(callText,stopwords())
  callText <- gsub(pattern = "\\b[A-z]\\b{1}", replace = " ", callText)
  callText <- gsub(pattern = "_", replace = " ",callText)
  callText <- stripWhitespace(callText)
  return(callText)
}

callText1 <- cleanData(datastyle_name)

wordsBreak <- function(cleanData2){
  callTextBag <- str_split(cleanData2, pattern = "\\s+")
  callTextBag <- unlist(callTextBag)
  return(callTextBag)
}

callTextBag1 <- wordsBreak(callText1)

styleNameCloud <- wordcloud(callTextBag1, min.freq = 50, random.order = F, color = "blue")
