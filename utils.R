
#### conect to SOAR package
fstoreconnect = function(subdir){
  oldLC = Sys.getenv("R_LOCAL_CACHE", unset = ".R_Cache")
  Sys.setenv(R_LOCAL_CACHe = subdir)
}
fstoreconnect("rstore")
tmp = Objects()

samp = fread(paste0(data.dir,"/SampleSubmission.csv"))
###
freq.encode = function(x ,xnew = x){
  if(is.factor(x) || is.character(x)){
    return(as.numeric(factor(xnew, levels = names(sort(table(x))))))
  }else{
    return(approxfun(density(x[!is.na(x)],n=length(x)/100))(xnew))
  }
}


group_age = function(x){
  if(x <= 1){
    return(1)
  }else if(x>1 & x<= 25){
    return(2)
  }else if(x>25 & x <=40 ){
    return(3)
  }else if(x>40 & x<= 65){
    return(4)
  }else if(x<65 & x <= 90){
    return(5)
  }else(return(6))
  
}


calc_entropy = function(df,group,subgrp,tgt_vn_prefix){
  df = as.data.table(df)
  sum1 = df[,.N,by =list(df[[group]],df[[subgrp]])]
  setnames(sum1,c(group,subgrp,"subgrpcnt"))
  sum2 = df[, .N, by = list(df[[group]])]
  setnames(sum2, c(group,"cnt"))
  sum2 = as.data.frame(sum2)
  sum3 = sum2 %>% left_join(sum1, by = c(group))
  sum3 = as.data.table(sum3)
  sum3[,entropy := -log(subgrpcnt *1/cnt)*subgrpcnt*1/cnt]
  sum3[is.na(entropy),entropy := 0]
  sum4 = sum3[,sum(entropy) ,by = list(sum3[[group]])]
  setnames(sum4, c(group,paste(tgt_vn_prefix,"entropy",sep = "_")))
  return(sum4)
}


RMS = function(x){
  cc = sqrt(mean(x^2,na.rm = T)) 
  return(cc)
}

min2 = function(x){
  m =median(x,na.rm = T)
  return(m)
}

max2 = function(x){
  m =max(x,na.rm = T)
  return(m)
}



mean2 = function(x){
  m =mean(x,na.rm = T)
  return(m)
}

sd2 = function(x){
  m = sd(x, na.rm = T)
  return(m)
}

quantile50 = function(x){
  q = quantile(x,0.5,na.rm = T)
  return(q)
}
quantile25 = function(x){
  q = quantile(x,0.25,na.rm = T)
  return(q)
}
quantile75 = function(x){
  q = quantile(x,0.75,na.rm = T)
  return(q)
}

map.func = function(x, y = 2){
  map = x %>% 
    sapply(FUN = function(x){strsplit(x, '[_]')[[1]][y]}) %>% 
    str_extract(., "^.{2}") 
  return(map)
}


#### XGB F1 score
eval_F1Score = function(preds,dtrain){
  actuals = getinfo(dtrain,"label")
  score = ModelMetrics::f1Score(actual = actuals,
                                predicted = preds,cutoff = 0.5)
  return(list(metric = "F1-Score", value = score))
}


#### my f2 cnt
my.f2cnt = function(th2,vn1,vn2, filter = TRUE){
  data = data.frame(f1= th2[,vn1],f2=th2[,vn2], filter = filter)
  colnames(data) = c("f1","f2","filter")
  sum1 = sqldf::sqldf("select f1,f2, count(*) as cnt from data where filter=1 group by 1,2")
  tmp = sqldf::sqldf("select b.cnt from data a left join sum1 b on a.f1=b.f1 and a.f2=b.f2")
  tmp$cnt[is.na(tmp$cnt)] = 0
  return(tmp$cnt)
  
}
