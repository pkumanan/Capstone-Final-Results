setwd("C:/Users/Arasan/Documents/Arasan/School/Ryerson/CKME 136- Capstone/Final Results and Code")
getwd()
rm(list = ls())
#library(factoextra)
GDPdata <- read.csv(file="GDP_Data_ProcessedNN.csv", header=TRUE, sep=",")
GDPdataCountry<-GDPdata
GDPdata <- na.omit(GDPdata)
#--------
#is.na(GDPdata)
#check=sum(is.na(GDPdata))
#str(GDPdata)
#class(GDPdata_x$GDPinUSDInMillions)
#--------
#Data normalization
normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}
GDPdata_norm <-normalize(GDPdata[,3:57])
#------------
GDPdata_lm <-GDPdata_norm# Data for regression analysis
GDPdata_x <-GDPdata_norm #Data for PCA analysis
if (FALSE){
GDPdata_x <-GDPdata_norm[,2:55]# Data for PCA analysis, removing the dependent variable GDPUSDInMillions
GDPdata_y <-GDPdata_norm[,1]
}
#GDPdata_x <-GDPdata[,3:57]# Data for PCA analysis
#-------------

# Splitting the GDPdata dataset (GDPdata_x) into training and test set.
train_index <- sample(1:nrow(GDPdata_x), 0.7 * nrow(GDPdata_x))
GDPdata.train.set <-GDPdata_x[train_index,-1]
GDPdata.test.set  <- GDPdata_x[-train_index,-1]
GDPdata_y_train.set<-GDPdata_x[train_index,1]# removing the dependent variable GDPUSDInMillions
GDPdata_y_test.set  <- GDPdata_x[-train_index,1]# removing the dependent variable GDPUSDInMillions
GDPdataCountrytrainset=GDPdataCountry[train_index,]

#Apply normalization (scaling and centering) to the data.
#Calculate PCA with normalized data. 
GDPdataTrainSet.pca.normdata <- prcomp(GDPdata.train.set, scale = TRUE, center= TRUE)
GDPdataTrainSet.pca.normdata
#All the varaiables in PCA 1 are negative. hence all contribute to GDP
#In PCA 2 DomesticBeer, ImportedBeer, Milk1Liter, LoafofBread, Water2Litre, BottleofWine, Domestic1LitreBeer, Imported1LitreBeer, abd Cigarettes20Pack have positve values
#Whereas MealInexpensive,	MealFor2,	McMealMcDonalds, CokeOrPepsi,	Water1Litre, Eggs,	LocalCheese, OnewayTicket, and ChickenBreasts have negative values
#Negative values are important than positive values
#That is GDF is significantly influenced by if only buy negative valued variables
#In the case of 3rd PCA MealFor2,	McMealMcDonalds,	DomesticBeer,	ImportedBeer, milk1Litre, Eggs,	LocalCheese, BottleofWine, and Domestic1LitreBeer have negative values	
#whereas MealInexpensive, CokeOrPepsi,	Water1Litre, Water2Litre, Cigarettes20Pack	OnewayTicket and	ChickenBreasts have positive values
#MealInexpensive, CokeOrPepsi,	Water1Litre, etc positively influence GDP









 
GDPdataTrainSet.pca.normdata$rotation      # eigen vector / rotation matrix / tranformation matrix
head(GDPdataTrainSet.pca.normdata$x)       # Transformed data
plot(GDPdataTrainSet.pca.normdata)
#-----------
var <- get_pca_var(GDPdataTrainSet.pca.normdata)
head(var$contrib, 4)
#-----------

#pca 1 have the most variance
#To confirm it do summary (pca)
summary(GDPdataTrainSet.pca.normdata)

# We obtained 55 principle component 
#The first 19 components account for >95% of total variance in the data

# Biplot the first two PCs. 
#dim(GDPdataTrainSet.pca.normdata$x)
boxplot(GDPdataTrainSet.pca.normdata$x, main='Norm Data Transformation')

#Visualize first two components of your PCA. 

biplot(GDPdataTrainSet.pca.normdata, choices = 1:2, main='Norm Data')

#correlations of the PCs

cor(GDPdataTrainSet.pca.normdata$x)

#-----------------
screeplot(GDPdataTrainSet.pca.normdata, type = "l", npcs = 19, main = "Screeplot of the first 19 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)



cumpro <- cumsum(GDPdataTrainSet.pca.normdata$sdev^2 / sum(GDPdataTrainSet.pca.normdata$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 11, col="blue", lty=5)
abline(h = 0.9, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC11"),
       col=c("blue"), lty=5, cex=0.6)
#Note: The first 11 components has an Eigenvalue >1 and explains almost 90% of variance 
#We can effectively reduce dimensionality from 160 to 11 if we consider 90 % variance
#-----------------



#-----------------
#Predict using PCA

PredictingData<- predict(GDPdataTrainSet.pca.normdata, newdata = GDPdata.test.set )
#select the first 19 pca components i.e., 95 % variance
# we tend to look at the first few principal components in order to find interesting patterns in the data.
#PredictingData<-PredictingData[,1:19]
#-----------------

#-----------------

fviz_pca_ind(GDPdataTrainSet.pca.normdata,
             col.ind = "cos2", # Color by the quality of representation
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Color individuals by groups
#Plots: quality and countries
fviz_pca_ind(GDPdataTrainSet.pca.normdata, label="none")
fviz_pca_ind(GDPdataTrainSet.pca.normdata, label="none", habillage=GDPdataCountrytrainset$Country)

#---------------


#--------------------
#Variable correlation plots
# Plot of train data
p <- fviz_pca_ind(GDPdataTrainSet.pca.normdata, repel = TRUE)
# Add predicted data
fviz_add(p, PredictingData, color ="blue")
#Posively and negatively (opposite to positive) correlated variables are grouped together
#The distance between variables and the origin measures the quality of the variables on the factor map
#Quality of representation
#The quality of representation of the variables on factor map is called cos2 (square cosine, squared coordinates)
# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(GDPdataTrainSet.pca.normdata, choice = "var", axes = 1:2)
#A high cos2 shows a good representation of the variable on the principal component.
#The closer a variable is to the circle of correlations, the better its representation on the factor map
#more important to interpret these components
#A low cos2 indicates that the variable is not perfectly represented by the PCs.
#In this case the variable is close to the center of the circle which are less important for the first components
#-----------------
#The top variables contributing to the principal components:
# Contributions of variables to PC1
fviz_contrib(GDPdataTrainSet.pca.normdata, choice = "var", axes = 1, top = 35)
#Note:The red dashed line on the graph indicates the expected average contribution. 
#For a given component, the variables whose values greater than threshold (red line) are important contributers
# Contributions of variables to PC2
fviz_contrib(GDPdataTrainSet.pca.normdata, choice = "var", axes = 2, top = 15)
#By comparing PC1 and PC2
#We can say that CokeorPepsi, ImportedBeer, Water1Litre, LeatherShoes, TaxiStartingFare, Taxi1HRWaiting, Oranges1KG and Apple1KG, contribute the most to the dimensions 1 and 2.
#The total contribution to PC1 and PC2
fviz_contrib(GDPdataTrainSet.pca.normdata, choice = "var", axes = 1:2, top = 10)
#The most important (or, contributing) variables are highlighted on the following correlation plot
fviz_pca_var(GDPdataTrainSet.pca.normdata, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)
#----------------


#-----------------
#Investigated how the variables are related to one another
plot('GDPdata(,1:5)')
#-----------------
#Regression analysis
#Regression helps show the relationship between  a response variable with respect to predictor varaiables
#Building a model for estimating GDP of a country based on the monetary values of all the goods available in that country
GDPresults = lm (GDPUSDInMillions ~ . , data = GDPdata_lm ) 
summary(GDPresults)
#Analysis of F-statistics(p-value: < 2.2e-16) shows that atleast one of the predictor variables is significantly related to the outcome variable
#Analysis the coefficients to find the predictor variables is significantly related to the outcome variable. 
summary(GDPresults)$coefficient
#Analysis of t-values show that local cheese, domestic1litrebeer imported1literbeer, onewayticket, chickenbreasts, monthlypass, prepaidmobilephone, 
#avgmonthlynetsalary, lettuce1head, and whiterice are significantly associated to changes in GDP

#----------------

#----------------
#Regression analysis using PCA
#PCA is due to various explanatory variables 
#Standard linear models potentially more powerful when using PCA
#combining both the explanatory variables - PCs, and explained variable - y
pcs <- as.data.frame(GDPdataTrainSet.pca.normdata$x)
pcs.data <- cbind(GDPdata_y_train.set, pcs)
pcsmodel <- lm(GDPdata_y_train.set ~ ., data = pcs.data)
summary(pcsmodel)
#only the first principal components turned out to be significant
#----------------

#----------------
#Next analysis using only first principle component
#Pca 1 is used as it has the most variance
#Obtained pca 1 for your analyses with the following
#Built into a linear regression model using pca1
#pca1=GDPdataTrainSet.pca.normdata$x[,1]
pca1=as.data.frame(GDPdataTrainSet.pca.normdata$x[,1])
pca1.data <- cbind(GDPdata_y_train.set, pca1)
pca1model <- lm(GDPdata_y_train.set ~ ., data = pca1.data)
summary(pca1model)
#---------------

#---------------
#Model accuracy assessment
#In multiple linear regression
#The R2 represents the correlation coefficient between the observed values 
#of the outcome variable (y) and the fitted (i.e., predicted) values of y.
#An R2 value close to 1 indicates that the model explains a large portion of the variance in the outcome variable.
#But R2 will always increase when more variables are added to the model
#A solution is to adjust the R2 by taking into account the number of predictor variables.
#By comparing multiple regression analysis using actual GDP data, all the principle components and first priciple component 
# it shows that analysis using all the PCAs shows maximum R2 value (0.9241) than using only first PCA (0.265 ) and predictor variables (GDP data) (0.9004) 
#The study shows the first  9 PCAs are enough for the analysis
#Regression analysis using PCA will give a better result 
#However PCR is not a feature selection method because each of the calculated principal components 
#is a linear combination of the original variables. 

#---------------