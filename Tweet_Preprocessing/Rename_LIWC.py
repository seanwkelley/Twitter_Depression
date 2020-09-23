import os
from os import listdir
import pandas as pd 
from langdetect import detect
from langdetect import detect_langs
from langdetect import DetectorFactory 
DetectorFactory.seed = 0
from random import seed
from random import randint
from functools import reduce
seed(123)
###########################################################################################
#Set path directory and type of tweet in set_wd_path.txt 
with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0]
tweet_type = set_wd_path[1]
os.chdir(path)
###########################################################################################

#read in the sentiments from VADER, ANEW, and LIWC 
#rename columns that were altered by LIWC and drop unnecessary or redundant columns 


VADER_ANEW_LIWC = pd.read_csv(path + 'Data/Sentiments/' + tweet_type + '/' + 'VADER_ANEW_LIWC_complete.csv',encoding="utf-8")

#rename columns that are changed in LIWC 
VADER_ANEW_LIWC = VADER_ANEW_LIWC.rename({'A':'Date','B': 'Day', 'C': 'Twitter_Handle','D':'Tweets','E':'recruitment_type','F':'negative',
	'G':'neutral','H':'positive','I':'compound','J':'valence','K':'arousal','L':'dominance'}, axis=1)


#drop variables that are not needed (VADER sentiments) and puntucation that does not mark the end of a sentence 
VADER_ANEW_LIWC = VADER_ANEW_LIWC.drop(['negative','neutral','positive','compound','valence','Comma',
						'Colon','SemiC','Dash','Quote','Parenth','OtherP'],axis=1)

VADER_ANEW_LIWC.to_csv("Data/Sentiments/" + tweet_type + "/VADER_ANEW_LIWC_complete.csv",index = False)

