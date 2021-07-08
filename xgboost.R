devresult = rep(0,nrow(df_train))
predte = rep(0,nrow(df_test))
cvscore = c()
int.seed = c(100)


for (i in 1:length(int.seed)) {
  cat("model training",i,"\n")
  
  set.seed(int.seed[i])
  folds = createFolds(label, k = 5)
  
  for (this.round in 1:length(folds)) {
    cat("model training",i," ","fold ",this.round,"\n")
    valid = c(1:length(label2))[unlist(folds[this.round])]
    dev = c(1:length(label2))[unlist(folds[1:length(folds)!= this.round])]
    
  dtrain<- xgb.DMatrix(data= as.matrix(df_train[dev,]), label= label2[dev])
  dvalid <- xgb.DMatrix(data= as.matrix(df_train[valid,]),label=label2[valid])
  valids <- list(val = dvalid)

  param = list(booster = "gbtree",
    objective = "binary:logistic",#"reg:gamma",
    eval_metric = "auc",
    eta = 0.1,
    colsample_bytree = 0.8,
    max_depth = 10,
    min_child_weight = 1,
    scale_pos_weight = 5,
    #num_parallel_tree = 2,
    nthread = 8,
    #base_score = mean(label),
    #gamma = 0,
    subsample = 0.8
  )
  
  model<- xgb.train(data = dtrain,
    params= param, 
    #feval = eval_F1Score,
    nrounds = 1200, 
    verbose = T, 
   # list(val = dvalid) ,       
   # early_stopping_rounds = 500, 
    print_every_n = 500,
    maximize = T
  )
    pred = predict(model,as.matrix(df_train[valid,]))
    devresult[valid] = pred
    pred_test = predict(model, as.matrix(df_test[,colnames(df_train)]))
    predte = predte + pred_test
    
    
  }
}




# 
# 
# bagpred_test = matrix(0,nrow = nrow(df_test), ncol = 3)
# int.seed = c(500,1235,101)
# for (i in 1:length(int.seed)) {
#   cat("model training",i,"\n")
#   j = c(1,1.05,1.02)
# set.seed(int.seed[i])
# mod.xgb = xgb.train(data = dtrain, params = param,
#                     nrounds = round(1000*j[i]),
#                     print_every_n = 500)
# 
# bagpred_test[,i] = predict(mod.xgb,newdata = dtest)
# 
# }
# 
# 

