#importing all the libraries####
library(data.table)
library(dplyr)      
library(ggplot2)    
library(caret)      
library(corrplot)   
library(cowplot)    
#importing the files####
setwd("~/Desktop/ r stuff")
cData = read.csv("Dataset.csv") 
#getting information about the dataset####
str(cData)
summary(cData)
colnames(cData)
sum(is.na(cData))
head(cData)
tail(cData)

# graphs####
fatTable=ggplot(cData) +   geom_bar(aes(Item_Fat_Content),fill = "red")
outletSize=ggplot(cData) +   geom_bar(aes(Outlet_Size),fill = "red")
itemWeight=ggplot(cData) +   geom_histogram(aes(Item_Weight),fill = "yellow")
itemVisb=ggplot(cData) + geom_histogram(aes(Item_Visibility), bins = 100,fill="green")
itemType=ggplot(cData) +   geom_bar(aes(Item_Type),fill = "blue")
outletEstablishment=ggplot(cData) +   geom_bar(aes(factor(Outlet_Establishment_Year)),fill = "purple")
fatTable2=ggplot(cData) +   geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales),fill = "red")
outletSize2=ggplot(cData) +   geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill = "orange")
itemWeight2=ggplot(cData) +   geom_point(aes(Item_Weight,Item_Outlet_Sales),fill = "yellow")
itemVisb2=ggplot(cData) + geom_point(aes(Item_Visibility,Item_Outlet_Sales),fill="green")
itemmrp=ggplot(cData) + geom_point(aes(Item_MRP,Item_Outlet_Sales),fill="green")
itemType2=ggplot(cData) +   geom_violin(aes(Item_Type,Item_Outlet_Sales),fill = "blue")
outletEstablishment2=ggplot(cData) +   geom_violin(aes(factor(Outlet_Establishment_Year),Item_Outlet_Sales),fill = "purple")

plot_grid(fatTable2,outletSize2,itemWeight2,
          itemVisb2,itemType2,outletEstablishment2,itemmrp)  
    ####DATA WRANGLING####
#sorting out item fat content#####
   ##getting rid of extra fat symbols##
    fatTable
    table(cData$Item_Fat_Content)
    cData$Item_Fat_Content[cData$Item_Fat_Content == "LF"] = "Low Fat" 
    cData$Item_Fat_Content[cData$Item_Fat_Content == "low fat"] = "Low Fat" 
    cData$Item_Fat_Content[cData$Item_Fat_Content == "reg"] = "Regular" 
    fatTable=ggplot(cData) +   geom_bar(aes(Item_Fat_Content),fill = "red")
    fatTable
  ## remocing fat for body and home products##
    table(cData$Item_Fat_Content)
    table(cData$Item_Type)
    cData$Item_Fat_Content[cData$Item_Type == "Health and Hygiene"] = "None"  
    cData$Item_Fat_Content[cData$Item_Type == "Household"] = "None"  
    cData$Item_Fat_Content[cData$Item_Type == "Others"] = "None"  
    table(cData$Item_Fat_Content)
#substituting the blanks with item outlet size with small####
    outletSize
    table(cData$Outlet_Size)
    cData$Outlet_Size= ifelse(cData$Outlet_Size=="", "Small", cData$Outlet_Size)
    table(cData$Outlet_Size)
    outletSize=ggplot(cData) +   geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill = "red")
    outletSize
#imputing missing values for item weight####
    sum(is.na(cData$Item_Weight))
    missing_index = which(is.na(cData$Item_Weight)) #2439
    for(i in missing_index)
    {   item = cData$Item_Identifier
    cData$Item_Weight[i] = mean(cData$Item_Weight[cData$Item_Identifier == item], na.rm = T)
    }
    sum(is.na(cData$Item_Weight))
#imputing missing values for item visibility####
    itemVisb
    table(cData$Item_Visibility)
    zero_index = which(cData$Item_Visibility == 0) 
    for(i in zero_index)
    { item = cData$Item_Identifier[i] 
    cData$Item_Visibility[i] = mean(cData$Item_Visibility[cData$Item_Identifier == item], na.rm = T)  }
    itemVisb=ggplot(cData) + geom_histogram(aes(Item_Visibility), bins = 100,fill="green")
    itemVisb
#getting rid of most of the items and making it 3 categories for item type ####
    perishable_food = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood" )
    non_perishable_food = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Snack Foods", "Soft Drinks","Starchy Foods")
    home_supplies = c( "Health and Hygiene", "Household")
    cData$Item_Type_new = ifelse(cData$Item_Type %in% perishable_food, "perishable_food", ifelse(cData$Item_Type %in% non_perishable_food, "non_perishable_food", ifelse(cData$Item_Type %in% home_supplies, "home_supplies", "other")))
    table(cData$Item_Type_new)
    table(cData$Item_Type)
#creating item category feature####
    table(cData$Item_Type, substr(cData$Item_Identifier, 1, 2))
    cData$Item_category = substr(cData$Item_Identifier, 1, 2)
    table(cData$Item_category )
#changing establishment year to age####
    cData$Outlet_Age= 2020-cData$Outlet_Establishment_Year
    table(cData$Outlet_Age)
    cData$Outlet_Establishment_Year=NULL
#changing MRP to low medium high very high####
    cData$Price = "Low"
    cData$Price[cData$Item_MRP >200] ="Very High"
    cData$Price[cData$Item_MRP>140 & cData$Item_MRP <=200] <- "High"
    cData$Price[cData$Item_MRP>70 & cData$Item_MRP <=140] <- "Medium"
#####renewed graphs####
    fatTable2=ggplot(cData) +   geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales),fill = "red")
    outletSize2=ggplot(cData) +   geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill = "orange")
    itemWeight2=ggplot(cData) +   geom_point(aes(Item_Weight,Item_Outlet_Sales),fill = "yellow")
    itemVisb2=ggplot(cData) + geom_point(aes(Item_Visibility,Item_Outlet_Sales),fill="green")
    itemmrp=ggplot(cData) + geom_point(aes(Item_MRP,Item_Outlet_Sales),fill="green")
    itemType2=ggplot(cData) +   geom_violin(aes(Item_Type_new,Item_Outlet_Sales),fill = "blue")
    outletEstablishment2=ggplot(cData) +   geom_violin(aes(factor(Outlet_Age),Item_Outlet_Sales),fill = "purple")
  
    plot_grid(fatTable2,outletSize2,itemWeight2,
              itemVisb2,itemType2,outletEstablishment2,itemmrp)  

    ####ENCODING####
#label encoding for outlet size and outlet location type####
    table(cData$Outlet_Size)
    cData$Outlet_Size = ifelse(cData$Outlet_Size == "Small", 0,ifelse(cData$Outlet_Size == "Medium", 1, 2))
    table(cData$Outlet_Size)
    
    table(cData$Outlet_Location_Type)
    cData$Outlet_Location_Type = ifelse(cData$Outlet_Location_Type == "Tier 3", 0,ifelse(cData$Outlet_Location_Type == "Tier 2", 1, 2))
    table(cData$Outlet_Location_Type)
#one hot encoding for all the remaining variables except for item identifier and outlet identifier#####
    dt.data = data.table(cData) 
    # so that we can still acess the names of the variables
    
    ohe = dummyVars("~.", data = dt.data[, -c("Item_Identifier", "Outlet_Identifier")], fullRank = T, sep=".") 
    
    ohe_df = data.frame(predict(ohe, dt.data[,-c("Item_Identifier", "Outlet_Identifier")]))
    cData = cbind(dt.data[,c("Item_Identifier", "Outlet_Identifier")], ohe_df)
####WORKING ON THE FINAL MODEL####
set.seed(123456789)
index=createDataPartition(cData$Item_Outlet_Sales, list=F, p=0.7)
Train=cData[index,]
Test=cData[-index,]
model_lm = lm(Item_Outlet_Sales ~ ., data = Train[,-c("Item_Identifier", "Outlet_Identifier")])
summary(model_lm)
predict.lm = predict( model_lm, Test)
RMSE(predict.lm, Test$Item_Outlet_Sales) 
R2(predict.lm, Test$Item_Outlet_Sales) 
submissionTable = newTest[, c("Item_Identifier","Outlet_Identifier", "Item_Outlet_Sales")]
submissionTable$PredictedValues = predict.lm
    
    
########  plot_grid()#########