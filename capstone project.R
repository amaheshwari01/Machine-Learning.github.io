#importing all the libraries####
library(data.table)
library(dplyr)      
library(ggplot2)    
library(caret)      
library(corrplot)   
library(cowplot)    
#importing the files####
setwd("~/Desktop/ r stuff")
dataset = read.csv("Dataset.csv") 
#getting information about the dataset####
str(dataset)
summary(dataset)
colnames(dataset)
sum(is.na(dataset))
head(dataset)
tail(dataset)

# graphs####
fatTable=ggplot(dataset) +   geom_bar(aes(Item_Fat_Content),fill = "red")
outletSize=ggplot(dataset) +   geom_bar(aes(Outlet_Size),fill = "red")
itemWeight=ggplot(dataset) +   geom_histogram(aes(Item_Weight),fill = "yellow")
itemVisb=ggplot(dataset) + geom_histogram(aes(Item_Visibility), bins = 100,fill="green")
itemType=ggplot(dataset) +   geom_bar(aes(Item_Type),fill = "blue")
outletEstablishment=ggplot(dataset) +   geom_bar(aes(factor(Outlet_Establishment_Year)),fill = "purple")
fatTable2=ggplot(dataset) +   geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales),fill = "red")
outletSize2=ggplot(dataset) +   geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill = "orange")
itemWeight2=ggplot(dataset) +   geom_point(aes(Item_Weight,Item_Outlet_Sales),fill = "yellow")
itemVisb2=ggplot(dataset) + geom_point(aes(Item_Visibility,Item_Outlet_Sales),fill="green")
itemmrp=ggplot(dataset) + geom_point(aes(Item_MRP,Item_Outlet_Sales),fill="green")
itemType2=ggplot(dataset) +   geom_violin(aes(Item_Type,Item_Outlet_Sales),fill = "blue")
outletEstablishment2=ggplot(dataset) +   geom_violin(aes(factor(Outlet_Establishment_Year),Item_Outlet_Sales),fill = "purple")

plot_grid(fatTable2,outletSize2,itemWeight2,
          itemVisb2,itemType2,outletEstablishment2,itemmrp)  
    ####DATA WRANGLING####
#sorting out item fat content#####
   ##getting rid of extra fat symbols##
    fatTable
    table(dataset$Item_Fat_Content)
    dataset$Item_Fat_Content[dataset$Item_Fat_Content == "LF"] = "Low Fat" 
    dataset$Item_Fat_Content[dataset$Item_Fat_Content == "low fat"] = "Low Fat" 
    dataset$Item_Fat_Content[dataset$Item_Fat_Content == "reg"] = "Regular" 
    fatTable=ggplot(dataset) +   geom_bar(aes(Item_Fat_Content),fill = "red")
    fatTable
  ## removing fat for body and home products##
    table(dataset$Item_Fat_Content)
    table(dataset$Item_Type)
    dataset$Item_Fat_Content[dataset$Item_Type == "Health and Hygiene"] = "None"  
    dataset$Item_Fat_Content[dataset$Item_Type == "Household"] = "None"  
    dataset$Item_Fat_Content[dataset$Item_Type == "Others"] = "None"  
    table(dataset$Item_Fat_Content)
#substituting the blanks with item outlet size with small####
    outletSize
    table(dataset$Outlet_Size)
    dataset$Outlet_Size= ifelse(dataset$Outlet_Size=="", "Small", dataset$Outlet_Size)
    table(dataset$Outlet_Size)
    outletSize=ggplot(dataset) +   geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill = "red")
    outletSize
#imputing missing values for item weight####
    sum(is.na(dataset$Item_Weight))
    missing_index = which(is.na(dataset$Item_Weight)) #2439
    for(i in missing_index)
    {   item = dataset$Item_Identifier
    dataset$Item_Weight[i] = mean(dataset$Item_Weight[dataset$Item_Identifier == item], na.rm = T)
    }
    sum(is.na(dataset$Item_Weight))
#imputing missing values for item visibility####
    itemVisb
    table(dataset$Item_Visibility)
    zero_index = which(dataset$Item_Visibility == 0) 
    for(i in zero_index)
    { item = dataset$Item_Identifier[i] 
    dataset$Item_Visibility[i] = mean(dataset$Item_Visibility[dataset$Item_Identifier == item], na.rm = T)  }
    itemVisb=ggplot(dataset) + geom_histogram(aes(Item_Visibility), bins = 100,fill="green")
    itemVisb
#getting rid of most of the items and making it 3 categories for item type ####
    perishable_food = c("Breads", "Breakfast", "Dairy", "Fruits and Vegetables", "Meat", "Seafood" )
    non_perishable_food = c("Baking Goods", "Canned", "Frozen Foods", "Hard Drinks", "Snack Foods", "Soft Drinks","Starchy Foods")
    home_supplies = c( "Health and Hygiene", "Household")
    dataset$Item_Type_new = ifelse(dataset$Item_Type %in% perishable_food, "perishable_food", ifelse(dataset$Item_Type %in% non_perishable_food, "non_perishable_food", ifelse(dataset$Item_Type %in% home_supplies, "home_supplies", "other")))
    table(dataset$Item_Type_new)
    table(dataset$Item_Type)
#creating item category feature####
    table(dataset$Item_Type, substr(dataset$Item_Identifier, 1, 2))
    dataset$Item_category = substr(dataset$Item_Identifier, 1, 2)
    table(dataset$Item_category )
#changing establishment year to age####
    dataset$Outlet_Age= 2020-dataset$Outlet_Establishment_Year
    table(dataset$Outlet_Age)
    dataset$Outlet_Establishment_Year=NULL
#changing MRP to low medium high very high####
    dataset$Price = "Low"
    dataset$Price[dataset$Item_MRP >200] ="Very High"
    dataset$Price[dataset$Item_MRP>140 & dataset$Item_MRP <=200] <- "High"
    dataset$Price[dataset$Item_MRP>70 & dataset$Item_MRP <=140] <- "Medium"
#####renewed graphs(after data wrangling)####
    fatTable2=ggplot(dataset) +   geom_violin(aes(Item_Fat_Content,Item_Outlet_Sales),fill = "red")
    outletSize2=ggplot(dataset) +   geom_violin(aes(Outlet_Size,Item_Outlet_Sales),fill = "orange")
    itemWeight2=ggplot(dataset) +   geom_point(aes(Item_Weight,Item_Outlet_Sales),fill = "yellow")
    itemVisb2=ggplot(dataset) + geom_point(aes(Item_Visibility,Item_Outlet_Sales),fill="green")
    itemmrp=ggplot(dataset) + geom_point(aes(Item_MRP,Item_Outlet_Sales),fill="green")
    itemType2=ggplot(dataset) +   geom_violin(aes(Item_Type_new,Item_Outlet_Sales),fill = "blue")
    outletEstablishment2=ggplot(dataset) +   geom_violin(aes(factor(Outlet_Age),Item_Outlet_Sales),fill = "purple")
  
    plot_grid(fatTable2,outletSize2,itemWeight2,
              itemVisb2,itemType2,outletEstablishment2,itemmrp)  

    ####ENCODING####
#label encoding for outlet size and outlet location type####
    table(dataset$Outlet_Size)
    dataset$Outlet_Size = ifelse(dataset$Outlet_Size == "Small", 0,ifelse(dataset$Outlet_Size == "Medium", 1, 2))
    table(dataset$Outlet_Size)
    
    table(dataset$Outlet_Location_Type)
    dataset$Outlet_Location_Type = ifelse(dataset$Outlet_Location_Type == "Tier 3", 0,ifelse(dataset$Outlet_Location_Type == "Tier 2", 1, 2))
    table(dataset$Outlet_Location_Type)
#one hot encoding for all the remaining variables except for item identifier and outlet identifier#####
    dt.data = data.table(dataset) 
    # so that we can still acess the names of the variables
    
    ohe = dummyVars("~.", data = dt.data[, -c("Item_Identifier", "Outlet_Identifier")], fullRank = T, sep=".") 
    
    ohe_df = data.frame(predict(ohe, dt.data[,-c("Item_Identifier", "Outlet_Identifier")]))
    dataset = cbind(dt.data[,c("Item_Identifier", "Outlet_Identifier")], ohe_df)
####WORKING ON THE FINAL MODEL####
set.seed(123456789)
index=createDataPartition(dataset$Item_Outlet_Sales, list=F, p=0.7)
Train=dataset[index,]
Test=dataset[-index,]
model_lm = lm(Item_Outlet_Sales ~ ., data = Train[,-c("Item_Identifier", "Outlet_Identifier")])
summary(model_lm)
predict.lm = predict( model_lm, Test)
RMSE(predict.lm, Test$Item_Outlet_Sales) 
R2(predict.lm, Test$Item_Outlet_Sales) 
submissionTable = Test[, c("Item_Identifier","Outlet_Identifier", "Item_Outlet_Sales")]
submissionTable$PredictedValues = predict.lm
    
    
########  plot_grid()#########