#Dram Society of Canada Data Cleaning 
library(tidyverse)
library(summarytools)
library(caret)
library(rattle)
##Data Load 

whiskyinfo <- read.csv("D:/projects/Dram Society of Canada/Dram Society of Canada - Master List.csv") 

whiskyinfo$Alex.W <- NULL

whiskyinfo$ABV[whiskyinfo$ABV == 26 ]  <- 46
whiskyinfo$PRICE <- as.numeric(whiskyinfo$PRICE)

# Each variable is in a column.
# Each observation is a row.
# Each value is a cell.

whiskyinfoL <- gather(whiskyinfo, key = "Individual" , value = "value", c(12:43), na.rm = FALSE,
       convert = FALSE, factor_key = FALSE)


whiskyinfoL.naremove <- whiskyinfoL[complete.cases(whiskyinfoL[ , 13]),]

whiskyinfoL.naremove$Individual <- as.factor(whiskyinfoL.naremove$Individual)
whiskyinfoL.naremove$WHISKYNAME <- paste(whiskyinfoL.naremove$WHISKY,whiskyinfoL.naremove$AGE)
whiskyinfoL.naremove$ABV <- as.integer(whiskyinfoL.naremove$ABV)


plot(whiskyinfoL.naremove$ORIGIN,whiskyinfoL.naremove$value)
plot(whiskyinfoL.naremove$ABV,whiskyinfoL.naremove$value)
plot(whiskyinfoL.naremove$PRICE,whiskyinfoL.naremove$value)
plot(whiskyinfoL.naremove$AGE,whiskyinfoL.naremove$value)


# with(whiskyinfoL.naremove, print(ctable(ORIGIN, value), method = 'render'))

fitControl <- trainControl(method="repeatedcv",number=20,repeats=10)

train.rpart <- train(value ~ PRICE + ABV + ORIGIN + AGE, 
                     data=whiskyinfoL.naremove,
                     method="rpart",
                     tuneLength=10,
                     na.action = na.pass,
                     trControl=fitControl)

fancyRpartPlot(train.rpart$finalModel)

write.csv(whiskyinfoL.naremove, "whisky_data_clean.csv")


## What whiskey should I bring? 



