#%% CELL 1 : settings

from pymongo import MongoClient

import urllib3
import json

import os
import re
from nltk.tokenize import regexp_tokenize
from unidecode import unidecode

import pandas as pd
import math

import time

os.chdir('C:/Users/Willy/Desktop/Cours M2 S1/')

# URL to access to the cloud : https://cloud.mongodb.com

#%% CELL 2 : to create or access the database and the table

#we connect to the cloud
client = MongoClient("mongodb+srv://Willy:FTD@datascienceproject-fukxh.mongodb.net/test?retryWrites=true&w=majority")

db = client["Project"] #we create the database
collection = db.stocktwits #we create the table

#%% CELL 3 : to store the elements in the table

http = urllib3.PoolManager()

Id_List=[158995403] #put a value if we want to start from a specific ID (i.e. date)

for i in range(1,2000):

    if len(Id_List) == 0:
        url = "https://api.stocktwits.com/api/2/streams/symbol/AAPL.json"
        r = http.request('GET', url)
    else:
        url = "https://api.stocktwits.com/api/2/streams/symbol/AAPL.json?max=" + str(min(Id_List))
        r = http.request('GET', url)
    data = json.loads(r.data)

    for messages in data["messages"]:
        
        Id_List.append(messages["id"])
        
        #we retreive the sentiment set up by the user for his message
        try:
            sentiment = messages["entities"]["sentiment"]["basic"]
            if sentiment == "Bullish":
                sentiment = +1
            else :
                sentiment = -1
        except :
            sentiment = 0 # no sentiment declared
       
        #we retreive the tags of the message
        symbols_list=[]
        for symbols in messages["symbols"]:
            symbols_list.append(symbols["symbol"])
        
        #we retreive the number of likes
        try:
            likes = messages["likes"]["total"]
        except:
            likes = 0
        
        element_to_insert = {"Message_Id": messages["id"], "Message_PublicationDate": messages["created_at"],"Message_Body": messages["body"], "Message_NumberOfLikes": likes, "Message_Sentiment": sentiment, "Message_Symbols": symbols_list, "User_Username": messages["user"]["username"],"User_Id": messages["user"]["id"],"User_NumberOfFollowers": messages["user"]["followers"],"User_NumberOfLikes": messages["user"]["like_count"]}
        result = db.stocktwits.insert_one(element_to_insert)
    
    if(i==200 or i==400 or i==600 or i==800 or i==1000 or i==1200 or i==1400 or i==1600 or i==1800):
       time.sleep(3600)
    i=i+1

del Id_List, url, symbols_list, sentiment, likes, data, messages, symbols, element_to_insert, i
    
#%% CELL 4 : to know the minimum ID and the minimum DATE

cursor = db.stocktwits.find({})
List1=[]
for document in cursor:
    List1.append(document["Message_PublicationDate"])

df=pd.DataFrame(List1,columns=["TimeStamp"])
df.index=pd.to_datetime(df.TimeStamp, format="%Y-%m-%d")
df['count']=1
df=df.loc[:,['count']]
df['date']=df.index.date

min_date=df.index.min()
max_date=df.index.max()

number_of_messages=len(List1)
number_of_messages_by_date = df.groupby('date')['count'].sum()

########################################

cursor = db.stocktwits.find({})
List2=[]
for document in cursor:
    List2.append(document["Message_Id"])  

min_id=min(List2)

########################################

number_of_messages_by_date.plot(title  = 'number_of_messages_by_date')

del List1, List2, df, document

#%% CELL 5 : to retreive our stocktwits data (SOLUTION 1 : from the database)

# we create a dataframe from the database

cursor = db.stocktwits.find({})

List1=[]
List2=[]
List3=[]
List4=[]

for document in cursor:
    List1.append(document["Message_Id"])
    List2.append(document["Message_PublicationDate"])
    List3.append(document["User_Id"])
    List4.append(document["Message_Body"])
    
df1=pd.DataFrame(List1)
df2=pd.DataFrame(List2)
df3=pd.DataFrame(List3)
df4=pd.DataFrame(List4)

df=pd.concat([df1,df2,df3,df4],axis=1)

df.columns=['Message_Id','Message_Date','User_Id','Message_Body']

del document, List1, List2, List3, List4, df1, df2, df3, df4

#%% CELL 6 : to retreive our stocktwits data (SOLUTION 2 : from the a csv)

#we create a csv from the database

f=open("stocktwits.csv","w", encoding="utf-8")
f.writelines("Message_Id")
f.writelines(";")
f.writelines("Message_Date")
f.writelines(";")
f.writelines("User_Id")
f.writelines(";")
f.writelines("Message_Body")
f.writelines("\n")

cursor = db.stocktwits.find({})
for document in cursor:

# we clean the text when creating the csv
    
    text = document["Message_Body"]
    text = unidecode(text) 
    text = re.sub('http:\/\/\s+','linktag', text)
    text = re.sub('https:\/\/\s+','linktag', text)
    text = re.sub(';',':', text)
    text = re.sub("\n","",text)
    text = regexp_tokenize(text,'(\w+|\$[\d\.]+|\S+)')
    text = ' '.join(text)

    f.writelines(str(document["Message_Id"]))
    f.writelines(";")
    f.writelines(str(document["Message_PublicationDate"]))
    f.writelines(";")
    f.writelines(str(document["User_Id"]))
    f.writelines(";")
    f.writelines(str(text))
    f.writelines("\n")

# we create a dataframe from the csv

df = pd.read_csv('stocktwits.csv',sep = ';',encoding = 'latin1')
df['Message_Body'] = df['Message_Body'].astype(str)
df = df.dropna()

del document, text

#%% CELL 7 : to retreive our price data

# website URL to create the csv : "https://finance.yahoo.com/quote/AAPL/community?p=AAPL"

stock_prices = pd.read_csv('AAPL_stock_prices.csv', sep = ',', index_col=[0], parse_dates=True)
stock_prices = stock_prices.iloc[:,4]

# we complete the stock_prices dataframe (missing prices during the weekends) 

dates = pd.date_range('2018-09-20' , '2019-09-20', freq='D')
full_year = pd.DataFrame(index = dates)
stock_prices = full_year.join(stock_prices)

stock_prices.sort_index(ascending = True)
stock_prices.index = stock_prices.index.date

for i in range (1,365):
    if math.isnan(stock_prices.iloc[i,0])==True and math.isnan(stock_prices.iloc[i+1,0])==True :
        stock_prices.iloc[i,0]=stock_prices.iloc[i-1,0]
    if math.isnan(stock_prices.iloc[i,0])==True and math.isnan(stock_prices.iloc[i+1,0])==False :
        stock_prices.iloc[i,0]=stock_prices.iloc[i+1,0]
    i=i+1

stock_prices = stock_prices.iloc[1:-1,:]
stock_prices.columns = ['stock_prices']

del dates, full_year, i

#%% CELL 8 : to create the sentiment variable (SOLUTION 1 : using textblob)

from textblob import TextBlob

# we create a column with polarity and objectivity 
# polarity --> range of [-1,1] where 1 means positive statement and -1 means a negative statement
# subjectivity --> [0,1] refers to personal opinion, emotion or judgment (0 = very objective, 1 = very subjective)
df['Message_Sentiment'] = df['Message_Body'].apply(lambda tweet: TextBlob(tweet).sentiment)
  
# we want to have polarity and subjectivity as single columns
List_sentiment = df['Message_Sentiment'].tolist()
columns = ['polarity', 'subjectivity']
#we create a new dataframe of sentiment series
dfsentiment = pd.DataFrame(List_sentiment, columns=columns, index=df.index)
#combine two datasets and drop sentiment column
df = pd.merge(df, dfsentiment, left_index=True, right_index=True)
df = df.drop('Message_Sentiment', axis=1)

#we compute the average per day
df_daily = pd.DataFrame(df)
df_daily.index = pd.to_datetime(df_daily['Message_Date'], format="%Y-%m-%d")
df_daily = df.resample('D', level=0).mean()
df_daily = df_daily.loc[:, ['polarity']]

df_daily.index = df_daily.index.date
df_daily.columns = ['sentiment']

del List_sentiment, columns, dfsentiment

#%% CELL 9 : to create the sentiment variable (SOLUTION 2 : using vader)

from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer

analyzer = SentimentIntensityAnalyzer()
List_sentiment=[]

for sentence in df['Message_Body']:
    sentiment = analyzer.polarity_scores(sentence)
    List_sentiment.append(sentiment)

dfsentiment = pd.DataFrame(List_sentiment)

df = pd.concat([df,dfsentiment['compound']],axis=1)

df_daily = pd.DataFrame(df)
df_daily.index = pd.to_datetime(df_daily['Message_Date'], format="%Y-%m-%d")
df_daily = df_daily.resample('D', level=0).mean()
df_daily = df_daily.loc[:, ['compound']]
df_daily = df_daily.iloc[::-1]

df_daily.index = df_daily.index.date
df_daily.columns = ['sentiment']

del List_sentiment, sentence, sentiment, dfsentiment

#%% CELL 10 : to create the sentiment variable (SOLUTION 3 : using the teacher's lexicon)

df_lexicon = pd.read_csv('lexicon.csv',sep = ';',encoding = 'latin1')
df_lexicon = df_lexicon.replace(to_replace='positive', value='1')
df_lexicon = df_lexicon.replace(to_replace='negative', value='-1')
df_lexicon['sentiment'] = df_lexicon['sentiment'].astype(float)
df_lexicon.index = df_lexicon.keyword
df_lexicon = df_lexicon.loc[:,['sentiment']]

tweet = df['Message_Body'].astype(str).str.replace('\[|\]|\'', '')
tweet = tweet.str.split()
tweet = pd.DataFrame(tweet)

result = []

data = tweet.to_json(orient='index')
data = json.loads(data)

for index in data:
    score = 0
    number_of_words_found = 0
    message = data[index]
    for word in message['Message_Body']:
        for i in df_lexicon.index:
            if i == word:
#                print(word,df_lexicon.loc[word,'sentiment'])
                score = score+df_lexicon.loc[word,'sentiment']
                number_of_words_found = number_of_words_found+1
#    print('#############')
    if number_of_words_found != 0 :
        score = score / number_of_words_found
    result.append(score)

dfsentiment=pd.DataFrame(result,columns=['sentiment'])

df = pd.concat([df,dfsentiment['sentiment']],axis=1)

df_daily = pd.DataFrame(df)
df_daily.index = pd.to_datetime(df_daily['Message_Date'], format="%Y-%m-%d")
df_daily = df_daily.resample('D', level=0).mean()
df_daily = df_daily.loc[:, ['sentiment']]
df_daily = df_daily.iloc[::-1]

df_daily.index = df_daily.index.date

del df_lexicon, tweet, result, data, index, score, number_of_words_found, message, word, i, dfsentiment

#%% CELL 11 : models and analysis

model1 = pd.concat([df_daily, stock_prices], axis=1)
model1 = model1.dropna()

# we import the econometric libraries we need 
import statsmodels.tsa.stattools
import statsmodels.formula.api as smf
from statsmodels.tsa.arima_model import ARIMA
import statsmodels.api as sm

############## ADJ CLOSE PRICES ANALYSIS ##############

# graph : prices
AAPLprice=model1['stock_prices']
ax1 = AAPLprice.plot( color='red', title = 'AAPL adjusted close price \n from 20-03-2019 to 20-09-2019')

# graph : price growth rates
AAPLpriceGR = (model1.stock_prices / model1.stock_prices.shift(1)) - 1
AAPLpriceGR = AAPLpriceGR.dropna()
ax2 = AAPLpriceGR.plot( color = 'g' , title  = 'AAPL adjusted close price growth rate \n from 20-03-2019 to 20-09-2019' )

##### we test the stationarity of the serie "prices" 

# we do an adfuller test on the adj close prices (H0 : there is a unit root)
statsmodels.tsa.stattools.adfuller(AAPLprice, regression='nc') 
# the serie is not stationary (we cannot reject H0 because the p-value is too high)

statsmodels.tsa.stattools.adfuller(AAPLpriceGR, regression='nc')

# we do AIC BIC test to know which order is the best for our series to do an ARMA
ARMAfit = sm.tsa.arma_order_select_ic(AAPLpriceGR, ic=['aic', 'bic'], trend='nc', max_ma=0)
ARMAfit.aic_min_order
ARMAfit.bic_min_order
# result : the best order is 1

# we fit an AR(1) 
model_ar1 = ARIMA(AAPLpriceGR, order=(1,0,0))
model_ar1_fit = model_ar1.fit(disp=0)
model_ar1_fit.summary()


############## SENTIMENT ANALYSIS ##############

# graph : sentiments
AAPLsentiment=model1['sentiment']
ax3 = AAPLsentiment.plot( color='red', title = 'sentiment \n from 20-03-2019 to 20-09-2019')

# graph : sentiment growth rates
AAPLsentimentGR = (model1.sentiment / model1.sentiment.shift(1)) - 1
AAPLsentimentGR = AAPLsentimentGR.dropna()
ax4 = AAPLsentimentGR.plot( color = 'g' , title  = 'sentiment growth rate \n from 20-03-2019 to 20-09-2019' )

##### we test the stationarity of the serie "sentiment" 

# we do an adfuller test on the sentiments (H0 : there is a unit root)
statsmodels.tsa.stattools.adfuller(AAPLsentiment, regression='nc')
# the serie is not stationary (we cannot reject H0 because the p-value is too high)

statsmodels.tsa.stattools.adfuller(AAPLsentimentGR, regression='nc')

# we do AIC BIC test to know which order is the best for our series to do an ARMA 
ARMAfit = sm.tsa.arma_order_select_ic(AAPLsentimentGR, ic=['aic', 'bic'], trend='nc', max_ma=0)
ARMAfit.aic_min_order
ARMAfit.bic_min_order
# result : the best order is 1 

# we fit an AR(1) 
model_ar2 = ARIMA(AAPLsentimentGR, order=(1,0,0))
model_ar2_fit = model_ar2.fit(disp=0)
model_ar2_fit.summary()


############## VAR and GRANGER CAUSALITY TEST ##############

# we import the functions we need 
from statsmodels.tsa.stattools import grangercausalitytests
from statsmodels.tsa.api import VAR

# the VAR will be on the sentiments and stock_prices but we consider the stationary series (i.e. the growth rates) 
model2=pd.concat([AAPLpriceGR,AAPLsentimentGR], axis = 1)

# graph : price growth rates VS sentiment growth rates
ax5 = model2.plot(color = ('g','red'), title = 'AAPL : sentiment growth VS adjusted close price growth' )

# VAR
model_var = VAR(model2,freq='d')
print(model_var.select_order(maxlags = 15,trend = 'c')) # which lag order for the VAR ? lag order = 1
result_var = model_var.fit(1)
result_var.summary()

# granger causality test
print(result_var.test_causality('sentiment','stock_prices'))
print(result_var.test_causality('stock_prices','sentiment'))
# the sentiments does not granger cause the stock prices
# the stock prices does not affect the sentiment 
