library(xgboost)
library(superml)

set.seed(101) # Set Seed so that same sample can be reproduced in future also
# Now Selecting 75% of data as sample from total 'n' rows of the data  
sample <- sample.int(n = nrow(invoices_clean), size = floor(.75*nrow(invoices_clean)), replace = F)
train_dat <- invoices_clean[sample, ]
test_dat  <- invoices_clean[-sample, ]

mean(train_dat$response)
mean(test_dat$response)

xg <- XGBTrainer$new(early_stopping = 50,
                     objective = 'binary:logistic')
xgb_model_1 <- GridSearchCV$new(trainer = xg,
                                parameters = list(n_estimators = c(1000),
                                                  learning_rate = c(0.01, 0.1, 1),
                                                  max_depth = c(5,2,10)),
                                n_folds = 3,
                                scoring = c('accuracy','auc'))


xgb_model_1$fit(train_dat %>%
                  dplyr::select(-inv_due2today) %>% 
                  select_if(is.numeric), "response")
xgb_model_1$evaluation_scores

xg <- XGBTrainer$new(early_stopping = 50,
                     objective = 'binary:logistic')
xgb_model_1 <- GridSearchCV$new(trainer = xg,
                                parameters = list(n_estimators = c(1000),
                                                  learning_rate = c(0.001,0.01,0.05),
                                                  max_depth = c(5,2)),
                                n_folds = 3,
                                scoring = c('auc'))


xgb_model_1$fit(train_dat %>%
                  dplyr::select_if(is.numeric), "response")

xgb_model_1$evaluation_scores


initial_model.cv <- xgb.cv(data = xgb.DMatrix(train_dat %>%
                                                dplyr::select(- response,
                                                              - inv_due2today) %>% 
                                                select_if(is.numeric) %>% 
                                                as.matrix(), 
                                              label = train_dat$response),
                         params = list(
                           eta = 0.01,
                           max_depth = 5,
                           objective = "binary:logistic",
                           eval_metric = "auc"),
                         nfold = 3,
                         nrounds = 5000)

ggplot(initial_model.cv$evaluation_log,aes(x=iter)) + 
  geom_point(aes(y=test_auc_mean), color = "green") +
  geom_point(aes(y=train_auc_mean), color = "red")

initial_model.cv$evaluation_log %>% View()

initial_model <- xgboost(data = xgb.DMatrix(train_dat %>%
                                              dplyr::select(- response,
                                                            - inv_due2today) %>%
                                              select_if(is.numeric) %>% 
                                              as.matrix(), 
                                            label = train_dat$response),
                         params = list(
                                    eta = 0.01,
                                    max_depth = 5,
                                    objective = "binary:logistic",
                                    eval_metric = "auc"),
                         
                         nrounds = 1250)

xgb.importance(model=initial_model)  
 

# drop unimportant variables