# -*- coding: utf-8 -*-
"""
Created on Fri May  6 23:36:23 2022

@author: HP
"""
import os
import pandas as pd
import matplotlib
import numpy as np
import datetime
import matplotlib.pyplot as plt
import seaborn as sns


#1) Definir fuente datos:

Folder='C:\\Users\\HP\\Desktop\Futuros CME\\PL'
Futures_CSV=os.listdir(Folder)


#2) Consolidar archivos:

Split_Contracts=pd.DataFrame()
Aux=pd.DataFrame()
Count=0

for i in Futures_CSV:
    
    if Count==0: 
     Split_Contracts=pd.read_csv(Folder+'\\'+Futures_CSV[Count], names=['Date','Open','High','Low','Close','Volume','Interest'])
     Split_Contracts['Contract']=i.replace('.csv', '')
     Count=Count+1
    
    else: 
     Aux=pd.read_csv(Folder+'\\'+Futures_CSV[Count], names=['Date','Open','High','Low','Close','Volume','Interest'])
     Aux['Contract']=i.replace('.csv', '')
     Split_Contracts=pd.concat([Split_Contracts, Aux], axis='rows')
     Count=Count+1
     Aux=pd.DataFrame()

print('End of Consolidation')
print(i)
Split_Contracts['Date']=pd.to_datetime(Split_Contracts['Date'], format='%Y%m%d')

    #2.1) Calcular retornos diarios en escala log:
Split_Contracts=Split_Contracts.sort_values(by=['Contract', 'Date'])    
Split_Contracts['Close_(T-1)']=Split_Contracts['Close'].shift(periods=1)
Split_Contracts['Contract_(T-1)']=Split_Contracts['Contract'].shift(periods=1)
Split_Contracts['Log_Returns']=Split_Contracts.apply(lambda x: 
                              np.log(x['Close']/x['Close_(T-1)']) if 
                              x['Contract']==x['Contract_(T-1)']else np.nan, axis=1)
Split_Contracts['Contract_ID']=Split_Contracts['Contract'].str[0:2]+'__'+Split_Contracts['Contract'].str[9]
Split_Returns=Split_Contracts.dropna()

  

#3) Graficar retornos:

sns.boxplot(data=Split_Returns, x='Contract_ID', y='Log_Returns')    
sns.displot(data=Split_Contracts['Log_Returns'], kde="True", bins=50)


#4) Construir curva spot:
    
Spot_Curve=Split_Contracts.groupby(['Date'])['Volume'].max().reset_index()
Spot_Curve['Filter']=True
Spot_Curve=Spot_Curve.drop_duplicates()
  
Spot_Curve2=pd.merge(left=Split_Contracts, right=Spot_Curve, left_on=['Date', 'Volume'], right_on=['Date', 'Volume'], how='left')
Spot_Curve2=Spot_Curve2[Spot_Curve2['Filter']==True]
Spot_Curve2=Spot_Curve2[['Date', 'Contract_ID']]
Spot_Curve2=Spot_Curve2.groupby('Date').first().reset_index()


i=0
 
while i<10:
    
    Spot_Curve2['Contract_ID (T-i)']=Spot_Curve2['Contract_ID'].shift(periods=10-i)
    Spot_Curve2['Contract_ID (T+i)']=Spot_Curve2['Contract_ID'].shift(periods=-(10-i))
    Spot_Curve2['Aux']=Spot_Curve2.apply(lambda x: x['Contract_ID (T-i)']
                          if x['Contract_ID (T-i)']==x['Contract_ID (T+i)']
                          else x['Contract_ID'], axis=1)
    Spot_Curve2['Contract_ID']=Spot_Curve2['Aux']
    Spot_Curve2=Spot_Curve2.drop(['Contract_ID (T-i)', 'Contract_ID (T+i)', 'Aux'], axis='columns')
    i=i+1
    
print(i)
Spot_Curve2.to_csv('Prueba.csv')
