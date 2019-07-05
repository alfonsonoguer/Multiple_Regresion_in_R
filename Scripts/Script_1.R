# library -----------------------------------------------------------------
pacman::p_load(dplyr,tidyr,plyr,ggplot2,caret,reader,
               RColorBrewer,corrplot,GGally,reshape)
# Coments -----------------------------------------------------------------

# By high correlation we remove 5strar reviews, no seguro sobre 4star
# hugh correlation between consecutive star ratings so dont use them hugh 
# correlation between negative service and low star reviews dont use too

# Data --------------------------------------------------------------------

OUTLIER<- function(x){ 
  x[pos]
  }

set.seed("123")

products<-read.csv("Data/existingproductattributes2017.csv")

productsNew<-read.csv("Data/newproductattributes2017.csv")

# Pre procesing -----------------------------------------------------------
# we remove the categorical variables into binary
newDataFrame <- dummyVars(" ~ .", data = products)
products <- data.frame(predict(newDataFrame, newdata = products))

newDataFrame <- dummyVars(" ~ .", data = productsNew)
productsNew <- data.frame(predict(newDataFrame, newdata = productsNew))
# we remove the missing values
summary(products)
products$BestSellersRank <- NULL
productsNew$BestSellersRank <- NULL

# we remove the heavy correlated variables.
# rm(newProducts,newDataFrame)
# corrplot(cor(products), method =  "number", tl.cex = 1)

ggcorr(products,label = TRUE,label_alpha = TRUE, legend.size = 7 )

findCorrelation(correlationmatrix, cutoff = 0.88, verbose = FALSE,
                names = TRUE,  exact = TRUE)

remove<-findCorrelation(correlationmatrix, cutoff = 0.88, verbose = FALSE,
                        names = TRUE,  exact = TRUE)
remove<-which(names(products) %in% remove)
str(remove)
products<- products[,-remove]



# we remove outliers
head(products)



# we remove the warranties, they are duplicated and not a relevant predictor
# for our products

products2 <- products[-(products$ProductType.ExtendedWarranty %in% TRUE), ]

summary(products2$ProductType.ExtendedWarranty)

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
products_train2<-products_train[,-which(names(products_train)%in%"ProductNum")]
head(products_train2)

# generic models comparison -----------------------------------------------

a<-c("lm","rf","svmLinear","svmRadial")
results <- c()
for (i in a){
  model<- train( Volume~ .,
                 data = products_train2,
                 method = i,
  )
  
  prediction <- predict(model, products_test)
  pr<-postResample(products_test$Volume,prediction) 
  results<-cbind(results,pr)
} 

colnames(results) <- c("lm","rf","svmLinear","svmRadial")

results

melted_results <- melt(results)

ggplot(melted_results, aes(x=X2, y = value)) + 
  geom_bar(stat = "identity", aes(fill=X2)) +
  facet_wrap(~X1,scales = "free") + 
  scale_fill_brewer("Dark2")





# LM primer modelo y optimizacion de datos ---------------------------------------
tc1<-trainControl(method = "CV", number =  10)
# no seria mejor usar un glm?

model0 <- train(funcion0,
                data = products_train2,
                method = "lm",
                tuneLength	= 10,
                trControl= tc1)

summary(model0)
products_test$predicted<-predict(model0,products_test)
# Residual standard error: 631.7 on 40 degrees of freedom
# Multiple R-squared:  0.8806,	Adjusted R-squared:  0.8209 
# F-statistic: 14.75 on 20 and 40 DF,  p-value: 1.176e-12
varImp(model0)

postResample(predict(model0,products_test),products_test$Volume)

# RMSE    Rsquared         MAE 
# 646.0703308   0.8398871 479.9396513 
# Warning message:
#   In predict.lm(modelFit, newdata) :
#   prediction from a rank-deficient fit may be misleading

model1 <- train(funcion1,
                data = products_train2,
                method = "lm",
                tuneLength	= 10,
                trControl= tc1)


postResample(predict(model1,products_test),products_test$Volume)

# RMSE          Rsquared      MAE 
# 627.0053810   0.9366942 352.2440187 


summary(model1)
# Residual standard error: 617.2 on 56 degrees of freedom
# Multiple R-squared:  0.8404,	Adjusted R-squared:  0.829 
# F-statistic: 73.74 on 4 and 56 DF,  p-value: < 2.2e-16
model2 <- train(funcion2,
                data = products_train2,
                method = "lm",
                tuneLength	= 10,
                trControl= tc1)

summary(model2)
# Residual standard error: 603.9 on 55 degrees of freedom
# Multiple R-squared:   0.85,	Adjusted R-squared:  0.8363 
# F-statistic: 62.32 on 5 and 55 DF,  p-value: < 2.2e-16

postResample(predict(model2,products_test),products_test$Volume)

# RMSE          Rsquared         MAE 
# 731.8182481   0.7918814 406.2546683 

model3 <- train(funcion3,
                data = products_train2,
                method = "lm",
                tuneLength	= 10,
                trControl= tc1)



summary(model3)
# Residual standard error: 604.9 on 54 degrees of freedom
# Multiple R-squared:  0.8522,	Adjusted R-squared:  0.8358 
# F-statistic: 51.89 on 6 and 54 DF,  p-value: < 2.2e-16

postResample(predict(model3,products_test),products_test$Volume)

# RMSE           Rsquared         MAE 
# 705.8776153   0.8095779 418.6032454 


# SVM Model ---------------------------------------------------------------
tc1<-trainControl(method = "CV", number =  10)


modelsvm <- train(funcion0,
                data = products_train2,
                method = "svmLinear",
                tuneLength	= 5,
                trControl= tc1)
vIMP<- varImp(modelsvm)

#   RMSE      Rsquared   MAE     
# 595.0369  0.7191308  289.2178

postResample(predict(modelsvm,products_test),products_test$Volume)
# RMSE         Rsquared        MAE 
# 948.880169   0.913094 346.003186 


SVM_funcion0 <- (Volume ~ .)
SVM_funcion1 <- (Volume ~ x4StarReviews)
SVM_funcion2 <- (Volume ~ x4StarReviews + PositiveServiceReview)
SVM_funcion3 <- (Volume ~ x4StarReviews + PositiveServiceReview + ShippingWeight)


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

ggplot(data = meltsvm, aes(x=X2,y=value)) + 
  geom_bar(stat = "identity", aes(fill=X2)) +
  stat_summary(fun.y = max, colour="black", geom="text", 
             position = position_nudge(x = 0, y = -0.12),
             aes(label=round(..y.., digits=4))) +
             facet_grid(X1~., scales = "free") + 
             scale_fill_brewer(palette = "Spectral") 
  


#           SVM_funcion0 SVM_funcion1 
# RMSE       948.880169 1044.9486549 
# Rsquared     0.913094    0.9199024  
# MAE        346.003186  497.4230201


