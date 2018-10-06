setwd("C:/Users/Bhaskar/Documents/Black Friday")
train<-read.csv("blackfridaytrain.csv" , header=TRUE, stringsAsFactor= FALSE)
test<-read.csv("blackfridaytest.csv" , header=TRUE, stringsAsFactor= FALSE)
str(train)
str(test)
#Checking missing data
sapply(train, function(x) sum(is.na(x)))
sapply(test, function(x) sum(is.na(x)))
#Data Exploration
test$User_ID<-as.factor(test$User_ID)
test$Product_ID<-as.factor(test$Product_ID)
test$Marital_Status<-as.factor(ifelse(test$Marital_Status == 1, 'Married', 'Single'))
test$Age<-as.factor(test$Age)
test$Gender<-as.factor(ifelse(test$Gender == 'M' , 'Male', 'Female'))
test$Occupation<-as.factor(test$Occupation)
test$City_Category<-as.factor(test$City_Category)
test$Stay_In_Current_City_Years<-as.factor(test$Stay_In_Current_City_Years)
str(test)
head(train$User_ID, 10)
train$User_ID<-as.factor(train$User_ID)
train$Product_ID<-as.factor(train$Product_ID)
train$Marital_Status<-as.factor(ifelse(train$Marital_Status == 1, 'Married', 'Single'))
train$Age<-as.factor(train$Age)
train$Gender<-as.factor(ifelse(train$Gender == 'M' , 'Male', 'Female'))
train$Occupation<-as.factor(train$Occupation)
train$City_Category<-as.factor(train$City_Category)
train$Stay_In_Current_City_Years<-as.factor(train$Stay_In_Current_City_Years)
userIDCount<-as.data.frame(table(train$User_ID))
names(userIDCount)<-c("User_ID", "User_Purchase_Count")
head(userIDCount)
train<-merge(x=train, y=userIDCount, by="User_ID", all.x=TRUE)
str(train)
test<-merge(x=test, y=userIDCount, by="User_ID", all.x=TRUE)
test[is.na(test$User_Purchase_Count), "User_Purchase_Count"]<-1
class(test$User_Purchase_Count)
str(test)
test$User_Purchase_Count<-as.integer(test$User_Purchase_Count)
head(train$Product_ID, 10)
ProductIDCount<-as.data.frame(table(train$Product_ID))
names(ProductIDCount)<-c("Product_ID", "Product_Sold_Count")
head(ProductIDCount)
train<-merge(x=train, y=ProductIDCount, by="Product_ID", all.x=TRUE)
str(train)
test<-merge(x=test, y=ProductIDCount, by="Product_ID", all.x=TRUE)
test[is.na(test$User_Purchase_Count), "User_Purchase_Count"]<-1
str(test)
test$User_Purchase_Count<-as.integer(test$User_Purchase_Count)
#Now to treat the missing values.
#We can assign the missing value equal to product category of 1 i.e. same product category in the missing position
#First create a new data frame only containing the variables Product category 1 and 2

product1<-train[,9:10]

#Replacing the missing values in the column Product category 2 and product category 3

for(i in 1:nrow(product1)){if(is.na(product1$Product_Category_2)=='TRUE'){product1$Product_Category_2=train$Product_Category_1}}

product2<-train[,10:11]

for(i in 1:nrow(product2)){if(is.na(product2$Product_Category_3)=='TRUE'){product2$Product_Category_3=product2$Product_Category_2}}

train$Product_Category_2<-product1$Product_Category_2
train$Product_Category_3<-product2$Product_Category_3

anyNA(train)
#We see that their are no missing values in the train dataset now
#Similarly filling the missing values of the test set

for(i in 1:nrow(test)){if(is.na(test$Product_Category_2)=='TRUE'){test$Product_Category_2=test$Product_Category_1}}
for(i in 1:nrow(test)){if(is.na(test$Product_Category_2)=='TRUE'){test$Product_Category_2=test$Product_Category_1}}

str(train)
str(test)





    

 

 



