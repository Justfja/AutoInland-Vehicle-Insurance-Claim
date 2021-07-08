# library(lightgbm)
# library(caret)

devresult = rep(0,nrow(df_train))
predte = rep(0,nrow(df_test))
cvscore = c()
int.seed = c(500)


for (i in 1:length(int.seed)) {
  cat("model training",i,"\n")
  
  set.seed(int.seed[i])
  folds = createFolds(label, k = 5)
  
  param = list(objective = "binary",
               metric = "auc",
              # boost_from_average = "false",
              # tree_learner = "serial",
               feature_fraction = 0.8,
               bagging_freq = 1,
               bagging_fraction = 0.8,
              # min_child_samples=20, 
               #min_child_weight=0.001,
               #min_split_gain=0.0,
              # min_data_in_leaf = 90,
              # min_sum_hessian_in_leaf = 10
               scale_pos_weight = 5
               #min_split_gain = 0.1,
               #num_class = 9
               #is_unbalance = TRUE
               )
  
  for (this.round in 1:length(folds)) {
    cat("model training",i," ",this.round,"\n")
    valid = c(1:length(label))[unlist(folds[this.round])]
    dev = c(1:length(label))[unlist(folds[1:length(folds)!= this.round])]
    
    dtrain = lgb.Dataset(data = as.matrix(df_train[dev,]),label = label[dev])
    dvalid = lgb.Dataset(data = as.matrix(df_train[valid,]),label = label[valid])
    
    model = lgb.train(data = dtrain,
                      params = param,
                      nrounds = 1000,
                     valids = list(val1 = dvalid),
                      boosting_type = "gbdt",
                      learning_rate = 0.1,
                      max_depth = -1,
                      #early_stopping_rounds = 200,
                     # num_leaves = 30,
                      num_threads = 8,
                      eval_freq =500,
                      seed = 54321,
                      verbose = -1
          )
    
pred = predict(model,as.matrix(df_train[valid,]))
devresult[valid] = pred
pred_test = predict(model, as.matrix(df_test[,colnames(df_train)]))
#pred_test = ifelse(pred_test>0.5,1,0)
predte = predte + pred_test

cat("model cv score:", model$best_score,"\n")
cvscore = c(cvscore, model$best_score)
cat("model cv mean score:",mean(cvscore), "\n")
  }
}



# 
# LGBMClassifier(boosting_type='gbdt', class_weight=None, colsample_bytree=1.0,
#                importance_type='split', learning_rate=0.1, max_depth=-1,
#                min_child_samples=20, min_child_weight=0.001, min_split_gain=0.0,
#                n_estimators=800, n_jobs=-1, num_leaves=31, objective=None,
#                random_state=None, reg_alpha=0.0, reg_lambda=0.0,
#                scale_pos_weight=5.0, silent=True, subsample=1.0,
#                subsample_for_bin=200000, subsample_freq=0)


# # Feature binning
# from sklearn.preprocessing import KBinsDiscretizer
# age_discretizer = KBinsDiscretizer(n_bins=10, encode='ordinal', strategy='quantile')
# 
# data['Age_Bins'] =age_discretizer.fit_transform(data['Age'].values.reshape(-1,1)).astype(int)

threshold_search <- function(act, pred) {
  best_threshold <- 0
  best_score <- 0
  for (threshold in seq(0.1, 0.99, 0.005)) {
    score = ModelMetrics::f1Score(act, as.integer(pred > threshold))
    if (score > best_score) {
      best_threshold <- threshold
      best_score <- score
    }
  }
  list(threshold = best_threshold, f1 = best_score)
}

threshold <- threshold_search(label,devresult)
cat("Optimal threshold:", threshold$threshold, "\tscore:", threshold$f1)

pred = predte/5
pred= ifelse(pred>0.5,1,0)
summary(as.factor(pred))





# Sys.setenv(RETICULATE_PYTHON = "C:\\Users\\taiwo.ogundare\\AppData\\Local\\Continuum\\anaconda3\\envs\\r-reticulate\\python.exe")
# 
# 
# preds = c()
# for (i in 1:nrow(devresult)){
#   row = devresult[i,] # Get row
#   max_val = max(row) # Find max value in row
#   preds[i] = match(max_val, row) - 1 # Append index of match value - 1 to list
# }