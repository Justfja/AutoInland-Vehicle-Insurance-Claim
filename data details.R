options(warn = -1)

library(lightgbm)
library(plyr)
library(tidyverse)
library(caret)
library(SOAR)
library(data.table)
library(lubridate)



#####
path.dir = getwd()
data.dir = paste0(path.dir,"/Data")

#dir.create(paste0(path.dir,"/subm"))
save.files.dir = paste0(path.dir,"/tmp")
subm.dir = paste0(path.dir,"/subm")
source("utils.R")


####
df = fread(paste0(data.dir,"/train.csv")) %>% as.data.frame()
test = fread(paste0(data.dir,"/test.csv")) %>% as.data.frame()
# policy = fread(paste0(data.dir,"/policy_data.csv")) %>% as.data.frame()
# payment = fread(paste0(data.dir,"/payment_history.csv")) %>% as.data.frame()


# m = setdiff(df$State,test$State)
# m2 = setdiff(df$LGA_Name,test$LGA_Name)
# m3 = setdiff(df$Car_Category,test$Car_Category)
# 
# df = df %>% filter(!State %in% m) %>%
#   filter(!LGA_Name %in% m2) %>% filter(!Car_Category %in% m3)

train.id = df$ID
test.id = samp$ID
Store(train.id)
Store(test.id)
label = df$target
Store(label)
df = bind_rows(df %>% select(-c(target)),test)

a = cyclic_encoding(df$`Policy Start Date`,c("day","quarter"))
colnames(a) = paste0("Policy_Start_",colnames(a))
# a2 = cyclic_encoding(df$`Policy End Date`,c("quarter"))
# colnames(a2) = paste0("Policy_End_",colnames(a2))
df = df %>% 
    mutate(
     # ID = sapply(ID,map.func),
      ID = NULL,
     nextm = `First Transaction Date` + 90,
     tenor = as.numeric(`Policy End Date` - `Policy Start Date`),
      tenor = ifelse(tenor>3650,3650,tenor),
      Age = ifelse(Age<0,0,ifelse(Age>120,120,Age)),
      age_en = freq.encode(Age),
   #  Subject_Car_Make = ifelse(Subject_Car_Make %in% c("."),"",Subject_Car_Make),
 # Car_Category=ifelse(Car_Category %in% c("Mini Bus"),"Bus",
 #   ifelse(Car_Category %in% c('Van','Mini Van'),"Van",
 # ifelse(Car_Category %in% c("Sedan",'CAMRY CAR HIRE'),"Saloon",
 #  ifelse(Car_Category %in% 'Station 4 Wheel',"Wagon",
 #   ifelse(Car_Category == "Tipper Truck",'Truck',
 #    ifelse(Car_Category == 'Pick Up > 3 Tons','Pick Up',Car_Category)))))),
 # Car_Category = ifelse(Car_Category =="" & ProductName =='Motor Cycle',"Motorcycle",Car_Category),
      # prd = ifelse(ProductName == "Muuve",1,
      #  # ifelse(ProductName %in% c('Car Classic','Car Plus','CarFlex'),2,
      #     ifelse(ProductName == 'CVTP',1,
      #       ifelse(ProductName == "Motot Cycle",1,0))),
    #  age2 = round_any(Age,10),
     # age_grp = sapply(Age,group_age),
      #start_year = year(ymd(`Policy Start Date`)),
      start_month = month(ymd(`Policy Start Date`)),
      start_qrs = quarter(ymd(`Policy Start Date`)),
      start_day = day(ymd(`Policy Start Date`)),
     start_day_en = freq.encode(start_day),
      start_week = week(ymd(`Policy Start Date`)),
      start_weekday = wday(ymd(`Policy Start Date`)),
      #start_weekend = ifelse(start_weekday>5,1,0),
     # year_day = yday(ymd(`Policy Start Date`)),
      #  year_day_sin = sin(start_qrs *(2 * pi /4)),
      # year_day_cos = cos(start_qrs *(2 * pi /4)),
    #  end_year = year(ymd(`Policy End Date`)),
      end_month = month(ymd(`Policy End Date`)),
      end_qrs = quarter(ymd(`Policy End Date`)),
      end_day = day(ymd(`Policy End Date`)),
      end_weekday = wday(ymd(`Policy End Date`)),
      end_week = week(ymd(`Policy End Date`))
    #  end_yday = yday(ymd(`Policy End Date`))
    #  prod_en =freq.encode(ProductName)
     # tran_year = year(ymd(`First Transaction Date`)),
      # tran_month = month(ymd(nextm)),
      # tran_qtrs = quarter(ymd(nextm)),
      # tran_day = day(ymd(nextm)),
      # tran_week = week(ymd(nextm)),
      # tran_weekday = wday(ymd(nextm))
     #Days_since_lasttran=as.numeric(Sys.Date()-`First Transaction Date`)
   # Gender = ifelse(Gender =="Male",1,ifelse(Gender=="Female",2,0)),
  #  Car_Category = as.numeric(as.factor(Car_Category)),
   # Subject_Car_Colour = as.numeric(as.factor(Subject_Car_Colour)),
  #  Subject_Car_Make = as.numeric(as.factor(Subject_Car_Make)),
   # LGA_Name = as.numeric(as.factor(LGA_Name)),
    #  State = as.numeric(as.factor(State)),
     # ProductName = as.numeric(as.factor(ProductName))
      ) %>% 
  select(-c(`Policy Start Date`,`Policy End Date`,`First Transaction Date`,nextm))



########
##
#######


#### POLICY
policy = df%>%  
  group_by(ProductName) %>% 
  summarise(pol_cnt = n(),
         prod_cnt= length(unique(No_Pol)),
        # prod_cnt2= sum(length(unique(PPR_PRODCD))),
         mean_premium = mean(Age,na.rm = T),
         min_premium = min(Age,na.rm = T),
      mean_pr = mean(No_Pol,na.rm = T),
    #min_prr = min(No_Pol,na.rm = T),
   # mean_day = mean(start_day,na.rm = T),
    #mean_day = mean(start_day,na.rm = T),
         #date_cnt = length(unique(NP2_EFFECTDATE)),
         #principal_cnt = length(unique(No_Pol>=6)),
        # prod_64QNIHM = sum(unique(PPR_PRODCD == "PPR_PRODCD_64QNIHM")),
         # sum_prem = sum(NPR_PREMIUM,na.rm = T),
         family = length(unique(Car_Category)),
   # family2 = mean(as.numeric(as.factor(Subject_Car_Make))),
   #fa = mean(as.numeric(as.factor(Car_Category))),
        # amount = mean(NLO_AMOUNT,na.rm = T),
         location = length(unique(State)),
   # location2 = length(unique(LGA_Name)),
         #category = max(CATEGORY),
   min_pr = min(tenor,na.rm = T),
    min_prem = mean(tenor,na.rm = T))
         #type_cnt = sum(length(unique(NLO_TYPE))),
  #        occupation = max(OCCUPATION)) %>% 
  # mutate(location = as.numeric(as.factor(location)),
  #        #category = as.numeric(as.factor(category)),
  #        agent= as.numeric(as.factor(agent)),
  #        type = as.numeric(as.factor(type)),
  #        #occupation = as.numeric(as.factor(occupation)),
  #        en = freq.encode(location))


# policy2 = df%>%
#   group_by(nn) %>%
#   summarise(
#     # prod_cnt2= sum(length(unique(PPR_PRODCD))),
#     mean_premiumc = mean(Age,na.rm = T),
#    # min_premiumc = min(Age,na.rm = T),
#    # mean_prc = mean(No_Pol,na.rm = T),
#     #location = length(unique(State)),
#     min_prc = min(tenor,na.rm = T),
#     min_premc = mean(tenor,na.rm = T))

df =df %>% left_join(policy) %>% #left_join(policy2,by="No_Pol") %>% 
mutate(Gender = ifelse(Gender =="Male",1,ifelse(Gender=="Female",2,0)),
      Car_Category = as.numeric(as.factor(Car_Category)),
     # test = ifelse(Age==120 & No_Pol ==4 & start_month ==1 & start_day==1,1,0),
 # car_en = freq.encode(Car_Category),
       Subject_Car_Colour = as.numeric(as.factor(Subject_Car_Colour)),
      Subject_Car_Make = as.numeric(as.factor(Subject_Car_Make)),
  #sub_en = freq.encode(Subject_Car_Make),
       LGA_Name = as.numeric(as.factor(LGA_Name)),
      #lga_en = freq.encode(LGA_Name),
        State = as.numeric(as.factor(State)),
       ProductName = as.numeric(as.factor(ProductName))) %>% 
  #add_count(Age) %>% rename(age_cnt = n) %>% 
  add_count(Age,ProductName) %>% rename(age_prd_cnt = n) 

#df$nn2 = my.f2cnt(df,"ProductName","Car_Category")

#a = calc_entropy(df,"ProductName","No_Pol","prd_p")
df$nn = df$No_Pol + df$ProductName

df = df %>% left_join(m)
df = cbind(df,a)


df_train = df[1:length(train.id),]
df_test = df[(length(train.id)+1):nrow(df),]



m = cbind(df_train,label)
m= m %>% select(Age,label) %>% 
  group_by(Age) %>% 
  summarise_all(list(
    mean = mean,
    min = quantile25,
    #med = sum,
    max =quantile75,
    sd =sd,
    rms = RMS))

# df = df %>% add_count(mean_premuim) %>% rename(prem_cnt = n)
# df$en2 = my.f2cnt(df,"pol_cnt","location")
# df$has_new = ifelse(df$`Policy ID` %in% policy$pol2,1,0)
# df = df %>% filter(!`Lapse Year` %in% c('2017','2018'))
# df$label = ifelse(df$Lapse==1,1,0)
# label = df$label
# Store(label)
# df_train = df %>% select(-c(`Policy ID`,Lapse,`Lapse Year`,label))
# #####
# df_test = samp %>% left_join(pol,by="Policy ID")
# df_test = df_test %>% add_count(mean_premuim) %>% rename(prem_cnt = n)
# df_test$en2 = my.f2cnt(df_test,"pol_cnt","location")
# df_test$has_new = ifelse(df_test$`Policy ID` %in% pol2,1,0)
# 


#########
dr.dat = df %>% select(Age,No_Pol)
#dr.dat = scale(df)
sumsq = NULL
for (i in 1:9) {
  set.seed(1234)
  sumsq[i] = sum(kmeans(dr.dat,centers = i, iter.max = 1000,
                        algorithm = "Forgy")$withinss)
}
plot(1:9,sumsq,type= "b")
###
set.seed(1234)
kmns = kmeans(dr.dat,4,iter.max = 1000,
              algorithm = "Forgy",trace = T)

cnts = kmns$clusters






###
m = policy %>% group_by(`Policy ID`) %>% summarise(bb = length(unique(CLF_LIFECD==1)))
summarise(cat=max(CATEGORY))
summarise(agent=max(AAG_AGCODE))
summarise(type=max(NLO_TYPE))
summarise(occu=max(OCCUPATION))


df_train$en2 = my.f2cnt(df_train,"pol_cnt","location")
en = freq.encode(df_train$loc)
length(unique(which(CLF_LIFECD==1)))



####PAYMENT







##No of days since last order
# last_30days = orders %>% filter(created_at>='2020-01-29') %>% 
#   group_by(vendor_id) %>% 
#   summarise(orders_last_30days = n()) %>% 
#   rename(vendor = vendor_id)
# 
# day= orders %>% filter(!duplicated(vendor_id, fromLast = T)) %>% 
#      mutate(days_since_last_order = as.numeric(Sys.Date()-as.Date(created_at))
#        ) %>% select(vendor_id,days_since_last_order) %>% 
#       rename(vendor = vendor_id)
# 
# df3 = df3 %>% left_join(last_30days, by = "vendor") %>% 
#               left_join(day, by = "vendor")







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

ftrs$name = rownames(ftrs)
ftrs =ftrs %>% drop_na()
df = df[,names(df) %in% ftrs$name]


map.func = function(x, y = 2){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[_]')[[1]][y]}) %>% 
    str_extract(., "^.{2}")
  return(map)
}





# ONE HOT ENCODING FOR CATEGORICAL VARIABLES
# get names of categorical features
o = c("Gender","Car_Category","Subject_Car_Colour","Subject_Car_Make","LGA_Name","State","ProductName")
df.categories = df %>% select(o)

# one hot encoding for categorical data
dummy <- dummyVars(" ~ .",data=df.categories)
df.categoric <- data.frame(predict(dummy,newdata=df.categories))

df = df %>% cbind(df.categoric) %>% select(-o)

# check for near-zero variance
nzv.data <- nearZeroVar(df, saveMetrics = TRUE)
# take any of the near-zero-variance perdictors
drop.cols <- rownames(nzv.data)[nzv.data$nzv == TRUE]

df <- df[,!names(df) %in% drop.cols]

paste('The dataframe now has', dim(df)[1], 'rows and', dim(df)[2], 'columns')







cols_to_remove = c('Bauchi','Abakaliki','Argungu','ENUGU SOUTH','Asa','Aba North','Aba South','AREPO','ALAPERE','Badagry','Asaba','Awka South','AWOYAYA','Bekwara','Asari-Toru',
  'Asokoro District','Awka','Ejigbo','Awka North','Egor','Calabar','Abeokuta North','Anthony Village','Akoka','Akinyele','Ajeromi-Ifelodun','Calabar Municipality',
  'Bosso','BARIGA','Akuku Toru','Central Area, Abuja','Akure North','Agbor','Akure South','Ado-Odo/Ota','Ado-Ekiti','Akwa Ibom','Bonny','Chanchaga','Aboh-Mbaise',
  'Anambra East','Biase','Aniocha South','Abeokuta South','Ankpa','Dopemu','Akoko North West','AKOWONJO','EMENE','Benue','EFFURUN','Lincoln','AKOKO-EDO','Red & Black',
  'Dark Gray','Dark Green','Dark Grey','Dark Red','Gray & Gray','Gray & Silver','Light Blue','Light Gray','Orange','Purple','Red & Blue','D. Red','Red & White','Red & Yellow',
  'Red Maroon','White & Blue','White & Red','White & Yellow','Yellow','Yellow & White','ABG','As Attached.1','Dark Blue','D. Gold','AJAO ESTATE','Tipper Truck','First Transaction Date_quarter',
  'NO GENDER','SEX','CAMRY CAR HIRE','First Transaction Date_year','Mini Van','Pick Up','Pick Up > 3 Tons','Shape Of Vehicle Chasis','Station 4 Wheel','Van','Cream','Wagon',
  'B.Silver','Beige Mitalic','Policy End Date_quarter','Black & Orange','Blue & Red','Blue Sky','Blue&White&Red','Burgundy','Champagne','Ashok Leyland','Astra','BRILLIANCE',
  'Rols Royce','Eket','MG','MINI COOPER','Man','Motorcycle.1','Opel','Porsche','REXTON','Raston','Renault','Scania','Bajaj','Seat','Skoda','Subaru','Suzuki','Tata',
  'Wrangler Jeep','Yamaha','ZOYTE',' IFAKO','AGBARA','Land Rover.','LIBERTY','KA','Jincheng','Black.1','Buik','CHANGAN','COMMANDER','Caddillac','Chrysler','DAF','Datsun',
  'Dodge','FOTON','GAC','GMC','Geely','Grand Cherokee','Howo','Hummer','Infiniti','Innson','Isuzu','Jaguar','Jeep','Ekeremor','Ughelli-North','Ekiti','Ekiti South-West','Aba-North',
  'Aba-South','Abia','Aboh-Mbaise.1','Abuja.1','Ado-Ekiti.1','Ado-Ota','Ajegunle-State','Ajeromi-Ifelodun.1','Akoko-West','Anambra-East','Aniocha-South','Asari-Toru.1','Awka-North',
  'Awka-South','Bauchi.1','Bayelsa','Warri-North','Calabar-Municipality','Central-Abuja','Cross-River','ENUGU-SOUTH','Ebonyi','Edo','Ekiti-East.1','AJAO-ESTATE','ABULE-EGBA',
  'kumbotso','Ukpoba','QuaAn Pan','RIVERS','Rogo','SANGO OTTA','SANGOTEDO','Shagamu','Somolu','Tai ','Udi Agwu','Udu','Ughelli North','Umuahia','Zaria ','Umuahia South','Uvwie',
  'Uyo','VGC','Warri-South','Warri North','Warri South','Wuse 11','Yenagoa','Yorro','Zaria','Ekiti-West','Enugu-North','Esan-Central','Ondo','N-A','Nasarawa','Ndokwa-East','Ngor-Okpala.1',
  'Nnewi-North','Nnewi-South','Nsit-Ubium','Obafemi-Owode.1','Ogba-Ndoni','Ogbmosho-South','Ogun-Waterside','Ondo-West','Kogi','Onitsha-North','Onitsha-South','Orile-Iganmu.1','Oshimili-North',
  'Ovia-SouthWest','Owerri-North','Owerri-West','Oyo-East','Oyo-West','QuaAn-Pan','Umuahia-South','Kwara','Kebbi','Esan-West','Ile-Oluji.1','Essien-Udim','Ethiope-East','Gombe.1','Ibadan-East',
  'Ibadan-North','Ibarapa-Central','Idemili-North','Idemili-south','Ife-North','Ijebu-East','Ijebu-North','Ilesha-East','Kano-Municipal','Ilesha-West','Ilorin-East','Ilorin-West',
  'Imo','Isoko-North','Isoko-south','Jos-North','Jos-South','Kaduna.1','Kaduna-South','Kano.1','Palm Groove','Oyo West','Oyo East','Ilesha','Ife North','Ijebu East',
  'Ijebu North','Ijora','Ikeja G.R.A','Ikenne','Ikot Ekpene','Ikotun','Ikwerre','Ile-Ife','Ile-Oluji','Ilesha East','Ifako-Agege','Ilesha West','Ilorin','Ilorin East','Ilorin West',
  'Irepodun','Isoko North','Isoko south','Iwo','Jibia','Jos North','Jos South','Udi-Agwu','Idemili south','Kaduna South','Gombe','Ekiti-East','Ekwusigo','Eleme','Enugu North','Epe',
  'Esan Central','Esan West','Essien Udim','Ethiope East','Garki','Garko','Goronyo','Idemili North','Hong','IBA','IBADAN NORTH EAST','IJAIYE','ISHAGA','Ibadan North','Ibadan North West',
  'Ibadan South East','Ibadan South West','Ibarapa Central','Idanre','Kaduna','Kajola','Oyo','Onitsha North','Ogun Waterside','Oguta','Ohaukwu','Ojodu','Okpe','Okpokwu','Okrika','Olamabolo',
  'Oluyole','Ondo West','Onitsha','Onitsha South','Ogbmosho South','Orile-Iganmu','Orolu','Orsu','Oshimili','Oshimili North','Osogbo','Oturkpo','Ovia SouthWest','Owerri North','Owerri West ','Oyi ',
  'Ogbomoso','Ogba/Egbema/Ndoni','Kano','Ngor-Okpala','Kano Municipal','Karu','Katagum','Keffi','Kuje','LGA','Car Vintage','Lokoja','MAGBORO','Marina','Ndokwa East','Niger State',
  'Obalende','Njaba','Nnewi','Nnewi North','Nnewi South','Nsit Ubium','Nwangele','OBANIKORO','ONDO','ONIRU','OWODE','Obafemi-Owode','Ilasamaja')

data3 = rbind(df_train,df_test[,colnames(df_train)])
um = umap::umap(data3)
colnames(um$layout) = paste0("umap_",1:2)


