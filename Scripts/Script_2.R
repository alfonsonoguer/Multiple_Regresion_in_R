# library -----------------------------------------------------------------
pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,reader,
               RColorBrewer,corrplot,GGally,reshape, viridis,plotly)
# Coments -----------------------------------------------------------------

# By high correlation we remove 5strar reviews, not sure about 4star
# hugh correlation between consecutive star ratings so  don't use both of them 
# hugh correlation between negative service and low star reviews 
# so don't use both of them too

# Funcion outliers --------------------------------------------------------

# if you imput a df and a fexility level, it returns a list of utliers sorted 
# by positive outliers and negative outliers

outliers_DF <- function(DF,flexibilidad)
{
  list_return <- list()
  for (i in 1:ncol(DF)) {
    list_outliers<- list()
    vec <- DF[,i]
    posiciones_plus<-which(vec - (flexibilidad * sd(vec)) > median(vec))
    posiciones_minus<-which(vec + (flexibilidad * sd(vec)) < median(vec))
    list_outliers[["posiciones_plus"]] <- posiciones_plus
    list_outliers[["posiciones_minus"]] <- posiciones_minus
    list_return[[names(DF)[i]]] <- list_outliers
  } 
  return <- list_return
}

# Data --------------------------------------------------------------------



set.seed("123")

products<-read.csv("Data/existingproductattributes2017.csv")

productsNew<-read.csv("Data/newproductattributes2017.csv")

# Pre procesing -----------------------------------------------------------
# we remove the irrelevant variables, extended warranty because its duplicated
# and the products that dont have sales

names(products)
summary (products$ProductType)

products <- products[-(which (products$ProductType == "ExtendedWarranty")), ] 

products <- products[-(which (products$Volume == 0)), ]

# we make the categorical variables into binary / we dummify the data
newDataFrame <- dummyVars(" ~ .", data = products)
products <- data.frame(predict(newDataFrame, newdata = products))

# we remove the missing values
summary(products)
products$BestSellersRank <- NULL
productsNew$BestSellersRank <- NULL

# we remove the heavy correlated variables.
# rm(newProducts,newDataFrame)
# corrplot(cor(products), method =  "number", tl.cex = 1)

correlationmatrix <- cor(products)
# correlationmatrix <- cor(products[,-which(names(products)=="ProductType")])

ggcorr(products,label = TRUE,label_alpha = TRUE, legend.size = 7 )

findCorrelation(correlationmatrix, cutoff = 0.88, verbose = FALSE,
                names = TRUE,  exact = TRUE)

# remove<-findCorrelation(correlationmatrix, cutoff = 0.88, verbose = FALSE,
#                         names = TRUE,  exact = TRUE)
remove<-which(names(products) %in% c("NegativeServiceReview","x5StarReviews",
                                     "x3StarReviews","x2StarReviews"))
str(remove)
products<- products[,-remove]





# we remove the outliers


outliers_list <- outliers_DF(DF = products, flexibilidad = 4)

products <- products[-outliers_list$Volume$posiciones_plus,]

partition<-createDataPartition(y = products$Volume, times = 1,p = 0.75,
                               list = FALSE )

products_train<- products[partition,]
products_test<- products[-partition,]

# Formulas for the models -------------------------------------------------

# [1] "ProductType.Accessories"      "ProductType.Display"         
# [3] "ProductType.ExtendedWarranty" "ProductType.GameConsole"     
# [5] "ProductType.Laptop"           "ProductType.Netbook"         
# [7] "ProductType.PC"               "ProductType.Printer"         
# [9] "ProductType.PrinterSupplies"  "ProductType.Smartphone"      
# [11] "ProductType.Software"         "ProductType.Tablet"          
# [13] "ProductNum"                   "Price"                       
# [15] "x5StarReviews"                "x4StarReviews"               
# [17] "x3StarReviews"                "x2StarReviews"               
# [19] "x1StarReviews"                "PositiveServiceReview"       
# [21] "NegativeServiceReview"        "Recommendproduct"            
# [23] "ShippingWeight"               "ProductDepth"                
# [25] "ProductWidth"                 "ProductHeight"               
# [27] "ProfitMargin"                 "Volume"   

funcion0 <- (Volume ~ .)
funcion1 <- (Volume ~ x4StarReviews + ProductWidth +
               PositiveServiceReview + x1StarReviews)
funcion2 <- (Volume ~ x4StarReviews + ProductWidth +
               PositiveServiceReview + x1StarReviews + 
               ProductDepth)
funcion3 <- (Volume ~ x4StarReviews + ProductWidth +
               PositiveServiceReview + x1StarReviews + 
               ProductDepth)
funcion4 <- (Volume ~ x4StarReviews + ProductWidth +
               PositiveServiceReview + x1StarReviews + 
               ProductDepth + Price)

v_function <- c(funcion0, funcion1, funcion2, funcion3, funcion4)

# we remove the product num form the training data because its a reference
# note probably it would be cleaner to use the subset function
products_train2<-products_train[,-names(products_train) %in% "ProductNum"]
head(products_train2)

# generic models comparison -----------------------------------------------

# we run 4 diferent models to see wich one gives us the better results

a<-c("lm","rf","svmLinear","svmRadial")
results <- c()
for (i in a){
  model<- train( Volume~ .,
                 data = products_train2,
                 method = i,
                 trainControl = tc1
  )
  
  prediction <- predict(model, products_test)
  pr<-postResample(products_test$Volume,prediction) 
  results<-cbind(results,pr)
} 

colnames(results) <- c("lm","rf","svmLinear","svmRadial")

results

melted_results <- melt(results)
# we print the results 
ggplot(melted_results, aes(x=X2, y = value)) + 
  geom_bar(stat = "identity", aes(fill=X2)) +
  facet_wrap(~X1,scales = "free") + 
  scale_fill_brewer(palette = "Spectral") +
  stat_summary(fun.y = max, colour="black", geom="text",
               vjust=-1,
               # position = position_nudge(x = 0, y = -0.12),
               aes(label=round(..y.., digits=2))) + 
  xlab("Models") + 
  ylab("")





# SVM Model ---------------------------------------------------------------
tc1<-trainControl(method = "CV", number =  10)

# 
# modelsvm <- train(funcion1,
#                 data = products_train2,
#                 method = "svmLinear",
#                 tuneLength	= 5,
#                 trControl= tc1)
vIMP<- varImp(modelsvm)

#   RMSE      Rsquared   MAE     
# 595.0369  0.7191308  289.2178

postResample(predict(modelsvm,products_test),products_test$Volume)
# RMSE         Rsquared        MAE 
# 948.880169   0.913094 346.003186 


SVM_funcion0 <- (Volume ~ .)
SVM_funcion1 <- (Volume ~ x4StarReviews)
SVM_funcion2 <- (Volume ~ x4StarReviews + PositiveServiceReview)
SVM_funcion3 <- (Volume ~ x4StarReviews + PositiveServiceReview +
                   ShippingWeight)


SVM_function <- c(SVM_funcion0, SVM_funcion1, SVM_funcion2, SVM_funcion3 )

resultSVM <- c()

for (i in SVM_function) {
  
  modelsvm0 <- train(i,
                     data = products_train2,
                     method = "svmLinear",
                     tuneLength	= 5,
                     trControl= tc1)
  
  pr <- postResample(predict(modelsvm0,products_test),products_test$Volume)
  
  resultSVM<- cbind(resultSVM , pr)
  
}
colnames(resultSVM)<- c("SVM_funcion0", "SVM_funcion1",
                        "SVM_funcion2", "SVM_funcion3")
meltsvm <- melt(resultSVM)



ggplot(data = meltsvm, aes(x=X2,y=value, label = round(value))) + 
  geom_bar(stat = "identity", aes(fill=X2)) +
  stat_summary(fun.y = max, colour="black", geom="text",
               vjust=+1,
               # position = position_nudge(x = 0, y = -0.12),
               aes(label=round(..y.., digits=4))) +
  facet_grid(X1~., scales = "free") + 
  scale_fill_brewer(palette = "Spectral") 


# geom_text(nudge_y = -0.12)
# theme_bw()

#           SVM_funcion0 SVM_funcion1 
# RMSE       948.880169 1044.9486549 
# Rsquared     0.913094    0.9199024  
# MAE        346.003186  497.4230201


# we save the final model
modelsvm <- train(SVM_funcion2,
                  data = products_train2,
                  method = "svmLinear",
                  tuneLength	= 5,
                  trControl= tc1)
saveRDS(modelsvm,
        file = "Models/modelsvm_definitivo.rds")


# # RF Model ---------------------------------------------------------------
# tc1<-trainControl(method = "CV", number =  10)
# 
# 
# Rf_funcion0 <- (Volume ~ .)
# Rf_funcion1 <- (Volume ~ x4StarReviews)
# Rf_funcion2 <- (Volume ~ x4StarReviews + PositiveServiceReview)
# Rf_funcion3 <- (Volume ~ x4StarReviews + PositiveServiceReview +
#                   ShippingWeight)
# 
# 
# Rf_function <- c(Rf_funcion0, Rf_funcion1, Rf_funcion2, Rf_funcion3 )
# 
# resultRf <- c()
# 
# for (i in Rf_function) {
# 
#   modelRf0 <- train(i,
#                     data = products_train2,
#                     method = "rf",
#                     tuneLength	= 5,
#                     trControl= tc1)
# 
#   pr <- postResample(predict(modelRf0,products_test),products_test$Volume)
# 
#   resultRf<- cbind(resultRf , pr)
# 
# }
# colnames(resultRf)<- c("Rf_funcion0", "Rf_funcion1",
#                         "Rf_funcion2", "Rf_funcion3")
# meltRf <- melt(resultRf)
# 
# 
#   
# ggplot(data = meltRf, aes(x=X2,y=value, label = round(value))) + 
#   geom_bar(stat = "identity", aes(fill=X2)) +
#   stat_summary(fun.y = max, colour="black", geom="text",
#                vjust=+1,
#                # position = position_nudge(x = 0, y = -0.12),
#                aes(label=round(..y.., digits=4))) +
#   facet_grid(X1~., scales = "free") + 
#   scale_fill_brewer(palette = "Spectral") 
#   
# 
# 
# 
#   
#   # geom_text(nudge_y = -0.12)
#   # theme_bw()
# 
# 
# modelRf2 <- train(Rf_funcion2,
#                   data = products_train2,
#                   method = "rf",
#                   tuneLength	= 5,
#                   trControl= tc1)
# 
# pr <- postResample(predict(modelRf2,products_test),products_test$Volume)
# 
# # RMSE             Rsquared      MAE 
# # 1523.8048513    0.9487067  517.2085370 
# 
# # saveRDS(modelRf2,
# #         file = "Models/modelRF_RF_funcion2")
# 
# 

# Errors ------------------------------------------------------------------

# we rename the dummy variables 

preducts2 <-read.csv("Data/existingproductattributes2017.csv")

productstorename <- preducts2$ProductNum   %in%  products$ProductNum
products$ProductType <- preducts2$ProductType[which(productstorename)]

# we predict the data to plot errors

products$Predictions <- round(predict(modelsvm,products))

products$absolute_error <- products$Predictions - products$Volume

products$relative_error <- (products$Predictions - products$Volume)/
  products$Volume

# sales by product
ggplot(data = products , aes(x = ProductType, y = Volume)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired")


# absolute error
ggplot(data = products , aes(x = ProductType, y = absolute_error)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired")


# Scatter prediction/Sales
ggplot(data = products) + 
  geom_point(aes(x=ProductNum, y = Volume),color = "chartreuse2") + 
  geom_point(aes(x=ProductNum, y = Predictions),color = "firebrick1") + 
  scale_fill_brewer(palette = "Paired")


# absolute error
ggplot(data = products , aes(x = ProductType, y = absolute_error)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired")


# Absolute error by product sorted by descending Sales 

ggplot(data = products ,
       aes(x =reorder(ProductType,-Volume) , y = Volume )) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("Product Number")


# Ploting the results ------------------------------------------


productsNew$Predictions <- round(predict(modelsvm,productsNew))

table(productsNew$ProductType)
predictionsWeCareAbout <- productsNew[which(productsNew$ProductType %in% 
                                              c("PC","Laptop","Netbook",
                                                "Smartphone")), ]

productsNew$Benefit <- productsNew$Predictions * productsNew$Price *
  productsNew$ProfitMargin

predictionsWeCareAbout$Profit <- predictionsWeCareAbout$Predictions * 
  predictionsWeCareAbout$Price *  predictionsWeCareAbout$ProfitMargin

# predicted profit by product tipe
ggplot(data = productsNew , aes(x = ProductType, y = Benefit)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired")

reorder(Category, -Count)

# predicted profit by product num
ggplot(data = productsNew ,
       aes(x =reorder( ProductNum , -Benefit) , y = Benefit)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("Product Number")
  
# predicted Sales by product num
ggplot(data = productsNew ,
       aes(x =reorder( ProductNum , -Predictions) , y = Predictions)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("Product Number") + 
  ylab("Predictes Sales") 

# profit by ProductNum

ggplot(data = predictionsWeCareAbout ,
       aes(x =reorder( ProductNum , -Profit) , y = Profit)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("Product Number")

# sales of our products

ggplot(data = predictionsWeCareAbout ,
       aes(x =reorder( ProductNum , -Predictions) , y = Predictions)) + 
  geom_bar(stat = "identity", aes(fill = ProductType)) + 
  scale_fill_brewer(palette = "Paired") + 
  xlab("Product Number") + 
  ylab("Predictes Sales") 

