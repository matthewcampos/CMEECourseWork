#!/usr/bin/env python3

"""Wrangling and sorting the Logistic Growth Data"""
__author__ = 'Matthew Campos (matthew.campos19@imperial.ac.uk)'
__version__ = '0.0.1'

import pandas
import numpy as np

data= pandas.read_csv("../Data/LogisticGrowthData.csv")

def getIndex(data, value):
    """Get index positions of value in dataframe"""
    #empty vector to store index value
    row = []
    #find the index where value is
    count = 0
    for val in data.PopBio:
        count = count + 1
        if (val == value):
            row = count - 1 #python starts with 0
    # Return vector indicating the positions of value in the dataframe
    return row

#making negative times 0
negative_time_index= []
in_count = -1
for i in data.Time:
    in_count = in_count + 1
    if (i < 0):
        negative_time_index.append(in_count)
for j in negative_time_index:
    data.at[j,'Time'] = 0

#Change OD_595
row_index = []
d = -1
for k in data.PopBio_units:
    d = d + 1
    if (k == 'OD_595'):
        row_index.append(d)
for x in row_index:
    data.at[x,'PopBio'] = data.PopBio.loc[x] * 100

#Shifting and Logging Dataset
min_value= data['PopBio'].min() #finds min value in dataset
index = getIndex(data,min_value) #gets the index of this value
#print(data.loc[index,])
data = data.drop([index]) #removes the negative as it is too negative
new_min = data['PopBio'].min() #finds new min value in dataset
data.PopBio = data.PopBio - new_min
min_value_2= data['PopBio'].min()

#remove 0
new_index = getIndex(data,min_value_2)
data = data.drop([new_index]) #removes the negative as it is too negative

#Log the PopBio
data.PopBio = np.log10(data.PopBio)

data.to_csv("../Data/Growth_data.csv")
