import numpy as np
import csv, os, sys, datetime
from scipy import stats
from sklearn import datasets
from sklearn import svm
from sklearn.svm import SVR
from sklearn.svm import SVC
import pandas as pd
import matplotlib.pyplot as plt 

from sklearn.linear_model import Lasso
from sklearn.linear_model import ElasticNet
from sklearn.decomposition import PCA
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import StratifiedKFold

from yellowbrick.model_selection import RFECV 
from yellowbrick.regressor import PredictionError
from yellowbrick.model_selection import FeatureImportances

from sklearn.linear_model import Ridge
from sklearn.linear_model import RidgeCV
from sklearn.linear_model import ElasticNet
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import cross_validate
from sklearn.model_selection import KFold
from sklearn.preprocessing import Normalizer
from sklearn import preprocessing
from array import array
# https://scikit-learn.org/stable/modules/generated/sklearn.linear_model.ElasticNet.html
# https://www.pluralsight.com/guides/linear-lasso-ridge-regression-scikit-learn 
# https://www.kaggle.com/cast42/feature-selection-and-elastic-net

# be careful as you read resources, as  R glmnet package uses the same term (alpha) to mean something different to what sklearn uses (l1 ratio) as far as i remember


# worth noting that these data are inclusive of outliers, which is probably hurting things. could set those to the mean of the entire sample 
# something like this code will do it, but would need to go back and print the data from r from the df_orm2 data 
# this just runs on the whole sample and doesn't have a train vs test set. not sure if it makes sense to use a slightly differetn dataset for test (i.e. the free users from FYPs if there is enough, or even your CES-D data)


os.chdir('D:/Twitter_Depression_Kelley/Data/elasticNet/')
feature_data = pd.read_csv("liwc_features.csv",sep=",",usecols=range(0,86))
#SDS total score
#target_data = pd.read_csv("sds_target.csv",sep=",",usecols=range(0,1))
#individual SDS items
target_data = pd.read_csv("sds_target_individual.csv",sep=",",usecols=range(0,20))


for feature in feature_data.select_dtypes(exclude=['object']).columns:
       feature_data[feature].fillna(feature_data[feature].median(), inplace = True)


alphas = np.linspace(1,0.001,30)

# in this version we are not trying different values for the l1_ratio, which we may want to do. I think i tried it with values from 0.1 to 1 and it ended up 'choosing' 1
# we will want to play with this a bit depending on if you want feature selection vs prediction
cval = KFold(10)

f_data_scaled = preprocessing.scale(feature_data)


l1_ratio=[0.1, 0.5, 0.7, 0.9, 0.95, 0.99, 1]

'''
Elastic net for SDS_Total score 
'''
enetCV = ElasticNetCV(l1_ratio=0.9,alphas=alphas, precompute='auto', max_iter=100000, cv=cval, copy_X=True, verbose=1)

# y_pred_enet = enetCV.fit(f_data_scaled, target_data['SDS_Total'])
# index= list(feature_data)
# s = pd.Series(enetCV.coef_, index=index) 
# s.sort_values()

# enetCV.coef_
# enetCV.alphas_
# print(enetCV.l1_ratio_)

# y_pred = pd.DataFrame(s)

# #save variables from enet to csv file 
# y_pred.to_csv('enet_10f.csv',index = True)



#Elastic net for individual SDS items 

y_pred =  pd.Series([])

for colname in target_data.columns:
	print(colname)
	y_pred_enet = enetCV.fit(f_data_scaled, target_data[colname])
	index= list(feature_data)
	s = pd.Series(enetCV.coef_, index=index) 
	s.sort_values()
	y_pred =  pd.concat([y_pred, s], axis=1)
	print(enetCV.l1_ratio_)
	print(enetCV.intercept_)

enetCV.coef_
enetCV.alphas_
enetCV.l1_ratio_


index= list(feature_data)
s = pd.Series(enetCV.coef_, index=index)
s.sort_values()

y_pred = pd.DataFrame(y_pred)

#remove first column of dataframe 
y_pred = y_pred.iloc[:,1:22]

y_pred.columns = ['SDS_1', 'SDS_2', 'SDS_3','SDS_4','SDS_5','SDS_6','SDS_7','SDS_8','SDS_9','SDS_10','SDS_11','SDS_12','SDS_13',
               'SDS_14','SDS_15','SDS_16','SDS_17','SDS_18','SDS_19','SDS_20']

# print(y_pred.columns)
y_pred.to_csv('enet_10f_individual_l1_0.9.csv',index = True)
