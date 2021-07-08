options(warn = -1)

# Libaries
library(plyr)
library(tidyverse)
library(lubridate)
library(stringr)
library(data.table)
library(caret)
library(fastICA)
library(SOAR)


source("utils.R")
#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

#dir.create(paste0(path.dir,"/tmp"))
save.files.dir = paste0(path.dir,"/tmp")

# Reading the data
### change this to yours
df = fread(paste0(data.dir,'/Train.csv')) %>% as.data.frame()
test = fread(paste0(data.dir,'/Test.csv')) %>% as.data.frame()

df = df %>% filter(TransactionStatus != 0)
test = test %>% filter(TransactionStatus != 0)
# Assigning Ids 
train.id = df$TransactionId
test.id = test$TransactionId
label = df$IsDefaulted

df = df %>%
  within(rm('IsDefaulted',"PaidOnDate","IsFinalPayBack",
            "DueDate","PayBackId","IsThirdPartyConfirmed",
            "AmountLoan","Currency"))

df =rbind(df,test)

####### Data Cleaning and Feature Engineering ########
most_freq_hours <- c('7','12','9','11','8','6','13','18','14')
least_freq_hours <- c('0','1','23','2','3','22')
df = df%>%
  mutate(
TransactionId = as.numeric(str_extract(TransactionId,"[[:digit:]]+")),
BatchId = as.numeric(str_extract(BatchId,"[[:digit:]]+")),
InvestorId = as.numeric(str_extract(InvestorId,"[[:digit:]]+")),
ThirdPartyId = as.numeric(str_extract(ThirdPartyId,"[[:digit:]]+")),
SubscriptionId = as.numeric(str_extract(SubscriptionId,"[[:digit:]]+")),
CustomerId = as.numeric(str_extract(CustomerId,"[[:digit:]]+")),
ProductId = as.numeric(str_extract(ProductId,"[[:digit:]]+")),
TransactionStartTime = ymd_hms(df$TransactionStartTime),
ProductCategory = as.numeric(as.factor(ProductCategory)),
Amount2 = ifelse(df$Amount<0,df$Amount*-1,df$Amount),
Value_Amount_diff = Value-Amount2,
is_charged = ifelse(Value_Amount_diff != 0,1,0),
hour = hour(TransactionStartTime),
week_day = wday(TransactionStartTime),
hour_test_bin = ifelse(hour %in% most_freq_hours,1,
                       ifelse(hour %in% least_freq_hours,2,3)),
ProviderId =NULL,
ChannelId = NULL,
Amount2 = NULL,
CurrencyCode = NULL,
CountryCode = NULL,
TransactionId= NULL,
#Value_Amount_diff = NULL,
LoanApplicationId = NULL,
Currency = NULL,
TransactionStatus = NULL,
LoanId = NULL,
BatchId = NULL,
IssuedDateLoan = NULL,###
TransactionStartTime = NULL) %>%
 add_count(Value)%>%
  rename("Value_cnt" = n)%>%
  add_count(InvestorId,hour,ProductId)%>%
  rename("Prov_id_hr_prodid_cnt" = n)%>%
  add_count(InvestorId,hour)%>%
  rename("Prov_id_hr_cnt" = n)%>%
  add_count(InvestorId, SubscriptionId,hour_test_bin)%>%
  rename("provid_chaid_hr_test_bin_cnt" = n)%>%
  add_count(ProductCategory, ProductId)%>%
  rename("prodcat_prodid_cnt" = n)%>%
  add_count(ProductCategory, ProductId,hour_test_bin)%>%
  rename("prodcat_prodid_hr_test_bin_cnt" = n)%>%
  add_count(CustomerId, week_day,hour_test_bin)%>%
  rename("cusid_wday_hr_test_bin_cnt" = n)%>%
  add_count(ProductId, week_day,hour_test_bin)%>%
  rename("prodid_wday_hr_test_bin_cnt" = n)%>%
  dplyr::select(-hour_test_bin)


## split the data
df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]



###
ftrs = data.frame(
  type = unlist(lapply(df[1:length(train.id),],class)),
  n.unique = unlist(lapply(df[1:length(train.id),],function(x)length(unique(x)))),
  f.missing = unlist(lapply(df[1:length(train.id),],function(x)mean(is.na(x)))),
  spear.cor = unlist(lapply(df[1:length(train.id),],function(x){idx = !is.na(x);
  if(is.factor(x)) x = as.numeric(x);
  if(is.character(x)) x = as.numeric(as.factor(x));
  if(is.integer(x)) x = as.numeric(x);
  if(is.logical(x)) x = as.numeric(x);
  cor(x[idx],y = label[idx], method = "spearman")
  }))
)


library(xgboost)

dtrain = xgb.DMatrix(as.matrix(df_train), label=label)
dtest = xgb.DMatrix(as.matrix(df_test[,colnames(df_train)]))

watchlist = list(train = dtrain)
#xgboost parameters
xgb_params <- list(colsample_bytree = 0.9, 
                   subsample = 0.5, 
                   booster = "gbtree",
                   max_depth = 3, 
                   min_child_weight = 0,
                   learning_rate = 0.03,
               #    scale_pos_weight = 0.90,
                 #  gamma = 0,
                   nthread = 8,
                   eval_metric = "auc", 
                   watchlist = watchlist,
                   objective = "binary:logistic")
#cross validation
set.seed(1235)
xgb_cv <- xgb.cv(xgb_params,
                 dtrain,
                 early_stopping_rounds = 100 ,
                 nfold = 5,
                 nrounds=5000,
                 print_every_n = 50)

# Training Xgb
set.seed(1235)
xgb_mod <- xgb.train(xgb_params,dtrain,nrounds = 186)
imp = as.data.frame(xgb.importance(feature_names = colnames(df_train),model = xgb_mod))

pred= predict(xgb_mod, dtest)
pred = ifelse(pred>0.5,1,0)

sub2 = data.frame(id=test.id,pred)
colnames(sub2) = c("TransactionId","IsDefaulted")
write.csv(sub2, file="sub.csv", row.names = F)



## MODELLING
library(rpart)
set.seed(1235)
model = rpart(label~., data = df_train, method = "class",
              control = rpart.control(cp = 0.01,minsplit = 20))

pred= predict(model,df_train)[,2]

confusionMatrix(as.factor(ifelse(pred>0.5,1,0)),as.factor(label))

