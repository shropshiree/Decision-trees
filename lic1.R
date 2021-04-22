library(readxl)
library(rpart)
library(tidyr)
library(dplyr)
library(rpart.plot)
library(ggplot2)
library(gridExtra) 
library(gbm)
library(randomForest)
library(mice) #NA
library(tree)
library(plotrix)
library(party)
library(ipred)
library(kableExtra)
library(caret)
library(ModelMetrics)
library(MLmetrics)




data <- read_excel("C:/Users/Wiktoria/Documents/IiE/Licencjat/dane2015.xlsx", sheet = 1, col_names = T)
View(data)
str(data)

#obciecie cyfr
data$BMI <- substr(data$BMI,1,4)
data$`blood pressure` <- substr(data$`blood pressure`,1,4)

#usuniecie >
data$`clean fuels`[data$`clean fuels`==">95"] <- "95"
data$`clean fuels`[data$`clean fuels`=="<5"] <- "5"

#zmiana zmiennych na numeryczne
data[,12:22] <- data.frame(sapply(data[,12:22],as.numeric))

#opcja 0-1
data[,2:11][is.na(data[,2:11])] <- 0


#opcja undummy
df <- data[,2:11]
undummy <- function(df){
  
  undummy_vec <- rep(0,194)
  
  for (i in 1:dim(df)[2]) {
    
    for(j in 1:dim(df)[1]){
      
      if(df[j,i]==1){
        undummy_vec[j] <- colnames(df[i])
      }
    }
    
  }
  return(undummy_vec)
}
region <- undummy(df)
data <- data.frame(data[,12:22],region)

#wybor zmiennych
data <- data[,c(1,2,5,6,7,8,9,10,11,12)]

#NA - jednak nie usuwam
{
  #pozbywanie sie NA
  #nie uwzgledniam mortality bo ja probuje wyjasnic
  miceMod <- mice(data[,-1],method = "rf")
  miceOutput <- complete(miceMod)
  data<- data.frame(data$mortality, miceOutput)
  colnames(data)[1] <- "mortality"
  
}



#colnames na pl
colnames(data) <- c("œmiertelnoœæ", "wydatki", "BMI", "samobójstwa", "ciœnienie.krwi", "zatrucia", "paliwa.ekologiczne", "zabójstwa", "higiena","region")

#statystyki opisowe
summary(data[,-10]) %>% kable() %>% kable_styling(bootstrap_options = "striped", full_width = F)

par(mar = c(5, 8, 1, 1))

ggplot(data, aes(x=region)) +
  geom_bar() + theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("Histogram - region")

#zbior uczacy i testowy
set.seed(27)

division   <- sample(nrow(data), round(0.8*nrow(data)), replace = F)
train <- data[division,]
test <- data[-division,]  

#Statystyki - porównanie zbiorów

{
p1 <- ggplot(train, aes(x=region)) +
  geom_bar() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("region - treningowy")
p2 <- ggplot(test, aes(x=region)) +
  geom_bar() +  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + ggtitle("region - testowy")
grid.arrange(p1,p2, nrow=1)

par(mfrow = c(1,2))
boxplot(train$œmiertelnoœæ, main="œmiertelnoœæ - treningowy")
boxplot(test$œmiertelnoœæ,  main="œmiertelnoœæ - testowy")

par(mfrow = c(1,2))
boxplot(train$wydatki,  main="wydatki - treningowy")
boxplot(test$wydatki,main="wydatki - testowy" )

par(mfrow = c(1,2))
boxplot(train$BMI,  main="BMI - treningowy")
boxplot(test$BMI,  main="BMI - testowy")

par(mfrow = c(1,2))
boxplot(train$samobójstwa,  main="samobójstwa - treningowy")
boxplot(test$samobójstwa,  main="samobójstwa - testowy")

par(mfrow = c(1,2))
boxplot(train$`ciœnienie.krwi`,  main="ciœnienie krwi - treningowy")
boxplot(test$`ciœnienie.krwi`,  main="ciœnienie krwi - testowy")

par(mfrow = c(1,2))
boxplot(train$zatrucia,  main="zatrucia- treningowy")
boxplot(test$zatrucia,  main="zatrucia - testowy")

par(mfrow = c(1,2))
boxplot(train$`ciœnienie krwi`,  main="paliwa ekologiczne - treningowy")
boxplot(test$`ciœnienie krwi`,main="paliwa ekologiczne - testowy")

par(mfrow = c(1,2))
boxplot(train$zabójstwa,  main="zabójstwa- treningowy")
boxplot(test$zabójstwa,  main="zabójstwa - testowy")

par(mfrow = c(1,2))
boxplot(train$higiena,  main="higiena- treningowy")
boxplot(test$higiena,  main="higiena - testowy")
}

#drzewo regresyjne

{
  tree <- rpart(œmiertelnoœæ~.,data=train, method="anova")
  
  
  print(tree)
  rpart.plot(tree, yesno = 2, type = 0)
  predict <- predict(tree, newdata = test)
  
  rmse(actual = test$œmiertelnoœæ,
       predicted = predict)
  
  MAPE(y_pred = predict, y_true = test$œmiertelnoœæ)
  
  imp <- varImp(tree)
  imp <- data.frame(zmienna = rownames(imp),wartoœæ = imp$Overall)
  imp %>% arrange(desc(wartoœæ))
  
  #class(tree)
  #rsq.rpart(tree)
  #plotcp(model)
  #print(model$cptable)
  
  hyper_grid <- expand.grid(
    cp = c(0,.001,0.01,0.1),
    minsplit=seq(1, 20, 1),
    maxdepth=seq(1, 30, 1)
  )
  
  rmse_err <- c()
  mape_err <- c()
  for (i in 1:nrow(hyper_grid)) {
    
    #set.seed(222)
    
    model <- rpart(formula = œmiertelnoœæ ~ ., 
                   data = train,
                   method="anova",
                   cp = hyper_grid$cp[i],
                   minsplit = hyper_grid$minsplit[i],
                   maxdepth = hyper_grid$maxdepth[i]
                     
    )
    
    pred <- predict(object = model,    
                    newdata = test)
    
    rmse_err[i] = rmse(actual=test$œmiertelnoœæ, predicted = pred)
    mape_err[i] = MAPE(y_pred = pred, y_true = test$œmiertelnoœæ)
    
  }
  
  opt_i <- which.min(rmse_err)
  print(hyper_grid[opt_i,])
  
  opt_i_2 <- which.min(mape_err)
  print(hyper_grid[opt_i_2,])
  
  
  model <- rpart(formula = œmiertelnoœæ ~ ., 
                 data = train,
                 method="anova",
                 cp = 0,
                 minsplit = 17,
                 maxdepth = 4
                 
  )
  
  pred <- predict(object = model, newdata = test)
  
  rmse(actual=test$œmiertelnoœæ, predicted = pred)
  
  MAPE(y_pred = pred, y_true = test$œmiertelnoœæ)

  summary(model)
  print(model)

  
  imp <- varImp(model)
  imp <- data.frame(zmienna = rownames(imp),wartoœæ = imp$Overall)
  imp %>% arrange(desc(wartoœæ))
  
}


#bagging

{
  set.seed(222)
  
  bag_tree <- bagging(œmiertelnoœæ~., data=train, coob=TRUE)
  
  varImp(bag_tree)
  
  pred <- predict(object = bag_tree,    
                  newdata = test)  
  
  rmse(actual=test$œmiertelnoœæ, predicted = pred)
  MAPE(y_pred = pred, y_true = test$œmiertelnoœæ)
  
  hyper_grid <- expand.grid(
    cp = c(0,.001,0.01,0.1),
    minsplit=seq(1, 20, 2),
    maxdepth=seq(1, 30, 3),
    nbagg=seq(25,1000, 25)
  )
  
  rmse_err <- c()
  
  for (i in 1:nrow(hyper_grid)) {
    
    set.seed(222)
    
    model <- bagging(formula = œmiertelnoœæ ~ ., 
                     data = train,
                     cp = hyper_grid$cp[i],
                     minsplit = hyper_grid$minsplit[i],
                     maxdepth = hyper_grid$maxdepth[i],
                     nbagg = hyper_grid$nbagg[i]
                     )
    
    pred <- predict(object = model,    
                    newdata = test)
    
    rmse_err[i] = rmse(actual=test$œmiertelnoœæ, predicted = pred)
    
  }
  
  opt_i <- which.min(rmse_err)
  print(hyper_grid[opt_i,])
  
  set.seed(222)
  
  model_bag <- bagging(formula = œmiertelnoœæ ~ ., 
                   data = train,
                   cp = 0,
                   minsplit = 1,
                   maxdepth = 1,
                   nbagg = 25
  )
  
  pred_bag <- predict(object = model_bag,    
                     newdata = test)
  
  rmse(actual=test$œmiertelnoœæ, predicted = pred_bag)
  MAPE(y_pred = pred_bag, y_true = test$œmiertelnoœæ)
  
  print(model_bag)
  
  imp <- varImp(model_bag)
  imp <- data.frame(zmienna = rownames(imp),wartoœæ = imp$Overall)
  imp %>% arrange(desc(wartoœæ))
  
}


#lasy losowe

{
  
  
  set.seed(111)
  data_imputed <- rfImpute(œmiertelnoœæ~., data)
  train_imputed <- data_imputed[division,]
  test_imputed <- data_imputed[-division,]
  
  set.seed(222)
  #rforest <- randomForest(œmiertelnoœæ ~ ., data=data_imputed, subset=division)
  rforest <- randomForest(œmiertelnoœæ ~ ., data=train_imputed)
  #reptree <- ReprTree(rforest, data, metric='d2')
  #plot(reptree, index=1)
  #pred = predict(rf, data[-division,])
  #plot(rforest)
  pred <- predict(object = rforest,    
                  newdata = test_imputed)
  rmse(actual=test_imputed$œmiertelnoœæ, predicted = pred)
  MAPE(y_pred = pred, y_true = test_imputed$œmiertelnoœæ)
  
  
  importance(rforest)
  varImpPlot(rforest)
  
  hyper_grid <- expand.grid(
    mtry=seq(1, 9, 1),
    nodesize=seq(2, 8, 1),
    ntree=seq(100, 1000, 100)
  )
  
  rmse_err <- c()

  for (i in 1:nrow(hyper_grid)) {
    
    set.seed(222)
    
    model <- randomForest(formula = œmiertelnoœæ ~ ., 
                          data = data_imputed[division,],
                          mtry = hyper_grid$mtry[i],
                          nodesize = hyper_grid$nodesize[i],
                          ntree = hyper_grid$ntree[i])
    
    pred <- predict(object = model,    
                    newdata = data_imputed[-division,])
    
    rmse_err[i] = rmse(actual=data_imputed[-division,1], predicted = pred)
    
  }

  opt_i <- which.min(rmse_err)
  print(hyper_grid[opt_i,])
  
  set.seed(222)
  
  model_rf <- randomForest(formula = œmiertelnoœæ ~ ., 
                        data = train_imputed,
                        mtry = 3,
                        nodesize = 4,
                        ntree=300)
  
  pred_rf <- predict(object = model_rf,    
                  newdata = test_imputed)
  
  rmse(actual=test_imputed$œmiertelnoœæ, predicted = pred_rf)
  MAPE(y_pred = pred_rf, y_true = test_imputed$œmiertelnoœæ)
  
  importance(model_rf)
  varImpPlot(model_rf)
    
}



#boosting
{
set.seed(222)
boost <- gbm(œmiertelnoœæ~., data=train)

pred<- predict(object = boost,newdata = test, n.trees = 100)

rmse(actual=test$œmiertelnoœæ, predicted = pred)
MAPE(y_pred = pred, y_true =  test$œmiertelnoœæ)

#gbm.perf(object = boost,method="OOB")



hyper_grid <- expand.grid(
  shrinkage = c(.001,.01, .1, .3),
  interaction.depth = c(1, 3, 5),
  cv.folds = c(0,2,4),
  n.minobsinnode=c(5,10,15),
  n.trees = seq(100, 1000, 100)
)

rmse_err <- c()
mape_err <- c()

for (i in 1:nrow(hyper_grid)) {
  
  set.seed(222)
  
  model <- gbm(formula = œmiertelnoœæ ~ ., 
               data = train,
               distribution = "gaussian",
               n.trees=hyper_grid$n.trees[i],
               interaction.depth = hyper_grid$interaction.depth[i],
               shrinkage = hyper_grid$shrinkage[i],
               cv.folds = hyper_grid$cv.folds[i]
               )
  
                        
  pred <- predict(object = model,newdata = test, n.trees = hyper_grid$n.trees[i])
  
  rmse_err[i] = rmse(actual=test$œmiertelnoœæ, predicted = pred)
  mape_err[i] = MAPE(pred, test$œmiertelnoœæ)
  
  
}
min(rmse_err)
opt_i <- which.min(rmse_err)
print(hyper_grid[opt_i,])

min(mape_err)
opt_i_2 <- which.min(mape_err)
print(hyper_grid[opt_i_2,])

set.seed(222)



model_boost <- gbm(formula = œmiertelnoœæ ~ ., 
             data = train,
             distribution = "gaussian",
             n.trees=1000,
             interaction.depth = 5,
             shrinkage = .01,
             cv.folds = 2,
             n.minobsinnode = 5
)



pred_boost <- predict(object = model_boost,    
                newdata = test,
                n.trees=1000)


rmse(actual=test$œmiertelnoœæ, predicted = pred_boost)
MAPE(y_pred = pred_boost, y_true = test$œmiertelnoœæ)

print(model_boost)

par(mar = c(5, 8, 1, 1))
summary(model_boost,las=2)

par(mfrow = c(2,2))
plot(model_boost,i="wydatki")
plot(model_boost,i="higiena")
plot(model_boost,i="zatrucia")
plot(model_boost,i="paliwa.ekologiczne")
plot(model_boost,i="ciœnienie.krwi")
plot(model_boost,i="zabójstwa")
plot(model_boost,i="samobójstwa")
plot(model_boost,i="BMI")

#plot.gbm(model_boost)
#pretty.gbm.tree(model_boost)


}


#partialPlot(model_rf, pred.data=train_imputed,x.var="wydatki")

#porównanie

{

  preds_list <- data.frame(var_num = 1:39,reg_tree = pred,bag =  pred_bag, rf = pred_rf, boost =pred_boost, true =test$œmiertelnoœæ)
  
  preds_list %>%
    ggplot(aes(y = true, x = var_num, colour="wartoœci rzeczywiste")) + geom_line(size = 1.5) +
    geom_line(data = preds_list, aes(y = reg_tree, colour = "drzewo regresyjne")) +
    geom_line(data = preds_list, aes(y = bag, colour = "bagging")) +
    geom_line(data = preds_list, aes(y = rf, colour = "lasy losowe")) +
    geom_line(data = preds_list, aes(y = boost, colour = "boosting")) +
    labs(title = "lalala") +
    ylab("œmiertelnoœæ noworodków") 
  
  
  
  
  
  
  

}


