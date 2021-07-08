
import pandas as pd
import numpy as np
import datetime
 
import seaborn as sns
import matplotlib.pyplot as plt
 
import warnings
warnings.filterwarnings('ignore')


train = pd.read_csv('Data/Train.csv')
test = pd.read_csv('Data/Test.csv')
ss = pd.read_csv('Data/SampleSubmission.csv')


target = train.target
train = train.drop("target", axis = 1)


data=train.append(test)


date_col = ['Policy Start Date','Policy End Date','First Transaction Date']
for feat in date_col:
    data[feat] = pd.to_datetime(data[feat])
    
    
  # NUMBER OF DAYS BETWEEN POLICY END DATE AND START DATE
data['policy_span'] = (data['Policy End Date'] - data['Policy Start Date']).dt.days


data['policy_span'].loc[data['policy_span'] > 5000] = 348 


gender_dummies = pd.get_dummies(data.Gender)
data = pd.concat([data,gender_dummies], axis=1)
data = data.drop("Gender", axis=1)

Car_Category_dummies = pd.get_dummies(data.Car_Category)
data = pd.concat([data,Car_Category_dummies], axis=1)
data = data.drop("Car_Category", axis=1)
  
Subject_Car_Colour_dummies = pd.get_dummies(data.Subject_Car_Colour)
data = pd.concat([data,Subject_Car_Colour_dummies], axis=1)
data = data.drop("Subject_Car_Colour", axis=1)
  
Subject_Car_Make_dummies = pd.get_dummies(data.Subject_Car_Make)
data = pd.concat([data,Subject_Car_Make_dummies], axis=1)
data = data.drop("Subject_Car_Make", axis=1)
  
LGA_Name_dummies = pd.get_dummies(data.LGA_Name)
data = pd.concat([data,LGA_Name_dummies], axis=1)
data = data.drop("LGA_Name", axis=1)
  
State_dummies = pd.get_dummies(data.State)
data = pd.concat([data,State_dummies], axis=1)
data = data.drop("State", axis=1)
  
ProductName_dummies = pd.get_dummies(data.ProductName)
data = pd.concat([data,ProductName_dummies], axis=1)
data = data.drop("ProductName", axis=1)
  
del data['ID']


date_col = ['Policy Start Date','Policy End Date','First Transaction Date']

# Extract date features
def extract_date_info(df,cols,):
    for feat in cols:
        df[feat +'_year'] = df[feat].dt.quarter
        df[feat +'_day'] = df[feat].dt.day
        df[feat +'_month'] = df[feat].dt.month
        df[feat +'_quarter'] = df[feat].dt.quarter
        df[feat +'_weekday'] = df[feat].dt.weekday
        df[feat +'_week'] = df[feat].dt.week
    df.drop(columns=date_col,axis=1,inplace=True)


extract_date_info(data,date_col)

# rename duplicated columns if present (to prevent errors after one hot encoding)
cols=pd.Series(data.columns)
for dup in cols[cols.duplicated()].unique(): 
    cols[cols[cols == dup].index.values.tolist()] = [dup + '.' + str(i) if i != 0 else dup for i in range(sum(cols == dup))]
data.columns=cols


cols_to_remove = ['Bauchi','Abakaliki','Argungu','ENUGU SOUTH','Asa','Aba North','Aba South','AREPO','ALAPERE','Badagry','Asaba','Awka South','AWOYAYA','Bekwara','Asari-Toru',
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
                'Obalende','Njaba','Nnewi','Nnewi North','Nnewi South','Nsit Ubium','Nwangele','OBANIKORO','ONDO','ONIRU','OWODE','Obafemi-Owode','Ilasamaja']
                
                data = data.drop(columns=cols_to_remove, axis=1)


train=data.iloc[:12079,]
test= data.iloc[12079:,]
y = target


from lightgbm import LGBMClassifier


from sklearn.model_selection import StratifiedKFold
predictions = []
fold=StratifiedKFold(n_splits=8,shuffle=True, random_state=42)
for train_index, test_index in fold.split(train,y):
    X_train, X_test = train.iloc[train_index], train.iloc[test_index]
    Y_train, Y_test = y[train_index], y[test_index]
    classifier2=LGBMClassifier(learning_rate=0.1,n_estimators=1000, scale_pos_weight = 5.0,)
    classifier2.fit(X_train, Y_train)
    preds=classifier2.predict(X_test)
    predictions.append(classifier2.predict(test))
    
    
  predictions = pd.DataFrame(predictions)
  
  predictions = predictions.mode()
  
  
predictions = predictions.T
predictions

sub = predictions[0]
sub_file = ss.copy()
sub_file.target = sub
sub_file.to_csv('test1.csv',index=False)
