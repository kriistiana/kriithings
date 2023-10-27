library(mlbench)
library(dplyr)
library(xgboost)
library(caret)
library(ggplot2)
library(ModelMetrics)

data("PimaIndiansDiabetes2")

pima <- PimaIndiansDiabetes2 %>% 
  mutate(def = case_when(diabetes == 'neg' ~ 1, T ~ 0))




set.seed(123)
indices <- createDataPartition(pima$def, p = 0.7, 
                               list = FALSE, 
                               times = 1)

pima.train <- pima[indices, ]
pima.test <- pima[-indices, ]


xgb.label <- pima.train$def

# target left in model
xgb.mod <- xgboost(data = xgb.DMatrix(pima.train %>% select(-diabetes) %>% as.matrix(), label = xgb.label),
                   nrounds = 3)

pima.test <- pima[-indices, ]
pima.test$pred <- predict(xgb.mod, pima.test %>% select(-diabetes) %>% as.matrix(), type = 'response')

qplot(pima.test$pred)
pima.test %>% group_by(cut(pred,seq(0,1,by = 0.1), include.lowest = T)) %>% 
  summarise(nn = n(),
            avgbr = mean(def))

auc(pima.test$def, pima.test$pred)

# incorrect objective function
xgb.mod <- xgboost(data = xgb.DMatrix(pima.train %>% select(-def,-diabetes) %>% as.matrix(), label = xgb.label),
                   nrounds = 10000)

pima.test$pred <- predict(xgb.mod, pima.test %>% select(-def,-diabetes) %>% as.matrix(), type = 'response')

qplot(pima.test$pred)
pima.test %>% group_by(cut(pred,seq(0,1,by = 0.1), include.lowest = T)) %>% 
  summarise(nn = n(),
            asvgbr = mean(def))

# too many nrounds
xgb.mod <- xgboost(data = xgb.DMatrix(pima.train %>% select(-def,-diabetes) %>% as.matrix(), label = xgb.label),
                   nrounds = 10000,
                   objective = 'binary:logistic')

pima.test <- pima[-indices, ]
pima.test$pred <- predict(xgb.mod, pima.test %>% select(-def,-diabetes) %>% as.matrix(), type = 'response')

qplot(pima.test$pred)
pima.test %>% group_by(cut(pred,seq(0,1,by = 0.1), include.lowest = T)) %>% 
  summarise(nn = n(),
            avgbr = mean(def))

auc(pima.test$def, pima.test$pred)

cv.res <- xgb.cv(data = xgb.DMatrix(pima.train %>% select(-def,-diabetes) %>% as.matrix(), label = xgb.label),
       nrounds = 1000,
       objective = 'binary:logistic',
       nfold = 5,
       eval.metric = 'auc')

cv.res1 <- xgb.cv(data = xgb.DMatrix(pima.train %>% select(-def,-diabetes) %>% as.matrix(), label = xgb.label),
                 nrounds = 100,
                 objective = 'binary:logistic',
                 nfold = 5,
                 eval.metric = 'auc')

ggplot(data = cv.res$evaluation_log,
       aes(x = iter)) + geom_line(aes( y = train_auc_mean), color = 'green') + geom_line(aes( y = test_auc_mean), color = 'red')

ggplot(data = cv.res1$evaluation_log,
       aes(x = iter)) + geom_line(aes( y = train_auc_mean), color = 'green') + geom_line(aes( y = test_auc_mean), color = 'red')



# reasonable model
xgb.mod <- xgboost(data = xgb.DMatrix(pima.train %>% select(-def,-diabetes) %>% as.matrix(), label = xgb.label),
                   nrounds = 12,
                   objective = 'binary:logistic',
                   scale_pos_weight = 1.8)

pima.test <- pima[-indices, ]
pima.test$pred <- predict(xgb.mod, pima.test %>% select(-def,-diabetes) %>% as.matrix(), type = 'response')

qplot(pima.test$pred)
pima.test %>% group_by(cut(pred,seq(0,1,by = 0.1), include.lowest = T)) %>% 
  summarise(nn = n(),
            avgbr = mean(def))

auc(pima.test$def, pima.test$pred)
xgb.importance(model = xgb.mod)
