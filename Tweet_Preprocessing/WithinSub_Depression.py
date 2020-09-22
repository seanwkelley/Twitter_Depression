#Determine whether a day occurs within or outside a depressive episode within indiviudal time series 
#Merge depressive episodes together if separated by less than 2 weeks
#Exclude episodes that are shorter than 2 weeks 

import os
import re
import pandas as pd
import string 
import emoji
import json
import csv 
import numpy as np
import contractions 
import nltk
import seaborn as sns
import matplotlib.pyplot as plt
from bs4 import BeautifulSoup
from re import finditer
from nltk.corpus import stopwords
from nltk.corpus import wordnet
from textblob import TextBlob
from nltk.tokenize import word_tokenize 
from nltk.stem import WordNetLemmatizer 
import wordninja #great packagee to split any compound words that are primarily from hashtags 
from vaderSentiment.vaderSentiment import SentimentIntensityAnalyzer
from dateutil import parser
from dateutil.relativedelta import relativedelta
import matplotlib.pyplot as plt
import spacy
from spacy.tokenizer import Tokenizer
nlp = spacy.load('en_core_web_sm')
from nltk.tokenize import word_tokenize 
tokenizer = Tokenizer(nlp.vocab)
from scipy import interpolate
from functools import reduce 
import shutil
from shutil import rmtree
from sys import argv
import stat
import datetime, pytz
from pytz import timezone
#from datetime import datetime
from dateutil import tz
from langdetect import detect
from langdetect import detect_langs
from langdetect import DetectorFactory 
DetectorFactory.seed = 0
import random, string
from numpy.random import seed
from numpy.random import randint
import cryptography
from cryptography.fernet import Fernet
from io import StringIO
from timezonefinder import TimezoneFinder 
tf = TimezoneFinder()
import matplotlib.mlab as mlab
import matplotlib.pyplot as plt
##########################################################################################
###########################################################################################
#Set path directory and type of tweet in set_wd_path.txt 
with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0]
tweet_type = set_wd_path[1]

os.chdir(path)
###########################################################################################
def get_dates(handle,episode_number):
        try: 
            start,end = "1", "2"
            handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
            episode_1 = str(handle_row['Depression_Dates.1_'  + episode_number + '_' + start])
            episode_1 = episode_1.split(' ')[4]
            episode_1 = episode_1.split('\n')[0]
            episode_1 = datetime.datetime.strptime(episode_1, "%d/%m/%Y")
            episode_2 = str(handle_row['Depression_Dates.1_' + episode_number + '_' + end])
            episode_2 = episode_2.split(' ')[4]
            episode_2 = episode_2.split('\n')[0]
            episode_2 = datetime.datetime.strptime(episode_2, "%d/%m/%Y")
            episodes = [episode_1,episode_2]
            
        except (KeyError,IndexError,ValueError):
            episodes = []
        return (episodes)

def submission_date(handle):
    handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
    episode_1 = str(handle_row['EndDate'])
    episode_1 = (handle_row['EndDate']).to_string()

    episode_1 = episode_1.split(' ')
    episodes = []
    for i in episode_1:
        episodes.append(i)
    episodes = episodes[4]
    return (episodes)
###########################################################################################
#read in the sentiments from VADER, ANEW, and LIWC 
#rename columns that were altered by LIWC and drop unnecessary or redundant columns 
os.chdir(path)

file = open('Data/Participant_Data/key.key', 'rb')
key = file.read()
file.close()

#participants data file, FYP, CW and SG
with open('Data/Participant_Data/Twitter_Participants.csv.encrypted', 'rb') as f:
    data = f.read()

#de-encrypt the Participants.csv data file 
fernet = Fernet(key)
encrypted = fernet.decrypt(data)


participants_encrypted = str(encrypted,'utf-8')
data = StringIO(participants_encrypted) 

Participant_info = Dates = pd.read_csv(data)


file_names = list(Participant_info['Twitter_Handle'])
id_dict = dict(zip(Participant_info['Twitter_Handle'],Participant_info['Id']))

#results of sentiment analysis 
VADER_ANEW_LIWC = pd.read_csv(path + 'Data/Sentiments/' + tweet_type + '/' + 'VADER_ANEW_LIWC_complete_dep.csv',encoding="utf-8")

print(type(VADER_ANEW_LIWC))

#number of seconds in a day 
display_time = 3600*24

#duration of critical transition prior to depression onset 
ct = 60

Participant_info['missing_days'] = ""

start_dates = []
sentiments_dep = []

for i in range(0,len(file_names)):
    
    name = file_names[i]
    handle = name
    random_id = id_dict[handle]

    submission_date_initial = submission_date(handle) 
    submission_date2 = parser.parse(submission_date_initial)
    beginning_date = submission_date2 - relativedelta(years=1)
    min_date = datetime.datetime(beginning_date.year, beginning_date.month, beginning_date.day)
    

    print(handle)
    print(random_id)

    episodes = ["1","2","3","4","5"]
    start = []
    end = []
    for i in episodes:
        a = get_dates(handle,i)
        if (len(a) != 0):
            start_depression_initial = a[0]
            end_depression_initial = a[1]

            print(start_depression_initial)
            print(end_depression_initial)

            #set the start and end of depressive episodes in relation 
            start_depression = int(((start_depression_initial - min_date).total_seconds())/display_time) 
            end_depression = int(((end_depression_initial - min_date).total_seconds())/display_time)

            #remove depressive episode shorter than 2 weeks and depressive episodes that occured entirely outside of the past year 
            if (end_depression - start_depression < 13) | (start_depression < 0 and end_depression < 0):
                continue

            #if the start date of an episode occurs before submission_date - 1 year then set the start date to 0
            if(start_depression < 0 and end_depression >= 0):
                start_depression = 0

            #if the end date occurs after 365 (2 cases where a participant enters the day after the submission date), set the
            #end date to 365 
            if(start_depression >= 0 and end_depression > 365):
                end_depression = 365

            start.append(start_depression)
            end.append(end_depression)

    start.sort(); end.sort()
  
    #concatenate depressive episodes that are separated by less than or equal to 2 weeks
    dep_gap = 15

    while len(start) >= 2:
        if len(start) == 2 and len(end) == 2:
            if(start[1] - end[0] <= dep_gap):
                del start[1]; del end[0]
                continue
        #three depressive episodes 
        if len(start) == 3 and len(end) == 3:
            if(start[1] - end[0] <= dep_gap):
                del start[1]; del end[0]
                continue
            if(start[2] - end[1] <= dep_gap):
                del start[2]; del end[1]
                continue
        #four depressive episodes
        if len(start) == 4 and len(end) == 4:
            if(start[1] - end[0] <= dep_gap):
                del start[1]; del end[0]
                continue
            if(start[2] - end[1] <= dep_gap):
                del start[2]; del end[1]
                continue
            if(start[3] - end[2] <= dep_gap): 
                del start[3]; del end[2]
                continue
        #five depressive episodes 
        if len(start) == 5 and len(end) == 5:
            if(start[1] - end[0] <= dep_gap):
                del start[1]; del end[0]
                continue
            if(start[2] - end[1] <= dep_gap):
                del start[2]; del end[1]
                continue
            if(start[3] - end[2] <= dep_gap): 
                del start[3]; del end[2]
                continue
            if(start[4] - end[3] <= dep_gap):     
                del start[4]; del end[3]
                continue
        break
    

    depression_times = {}
    depression_times['Start'] = start
    depression_times['End'] = end
    depressive_episodes = pd.DataFrame.from_dict(depression_times)

    print(depressive_episodes)

    #subset text features to one participant 
    sentiments_handle = VADER_ANEW_LIWC[VADER_ANEW_LIWC['Twitter_Handle'] == random_id]
    sentiments_handle['Day']  = sentiments_handle['Day'].astype(str).astype(int)

    try: 
        day_range = set(range(np.min(sentiments_handle['Day']),np.max(sentiments_handle['Day'])+1))
        diff_days = day_range.symmetric_difference(sentiments_handle['Day'])

        df_ = pd.DataFrame(columns=sentiments_handle.columns)

        df_['Day'] = list(diff_days)
        df_['Twitter_Handle'] = sentiments_handle['Twitter_Handle'].iloc[0]
        df_['recruitment_type']  = sentiments_handle['recruitment_type'].iloc[0]

        sentiments_handle = sentiments_handle.append(pd.DataFrame(data = df_), ignore_index=True)
        sentiments_handle = sentiments_handle.sort_values(by=['Day'])
        sentiments_handle = sentiments_handle.reset_index(drop=True)
        sentiments_handle['Depressed_today'] = 0
        sentiments_handle['critical_period'] = 0
    except(TypeError):
        continue

    #Iterate over all depressive episodes 
    for j in range(0,len(depressive_episodes['Start'])):
        name = "Episode_" + str(j)
        name2 = "Period_" + str(j)

        sentiments_handle[name] = ((sentiments_handle['Day'] >= depressive_episodes['Start'][j]) & (sentiments_handle['Day'] <= depressive_episodes['End'][j]))
        #days in the critical transition period prior to the onset of a depressive episode 
        sentiments_handle[name2] = ((sentiments_handle['Day'] >= depressive_episodes['Start'][j]-ct) & (sentiments_handle['Day'] <= depressive_episodes['Start'][j]-1)) 


    #if date falls within a depressive episode, then set the depressed_today column value to 1
    nl = sentiments_handle.columns
    matching = [s for s in nl if "Episode_" in s]
    matching2 = [s for s in nl if "Period_" in s]
    for i in range(0,sentiments_handle.shape[0]):
        b = (list(sentiments_handle[matching].iloc[i]))
        c = (list(sentiments_handle[matching2].iloc[i]))
        if True in b:
            sentiments_handle['Depressed_today'].iloc[i] = 1
        if True in c:
            sentiments_handle['critical_period'].iloc[i] = 1
    sentiments_dep.append(sentiments_handle)



#remove columns with 'episode' in the name 
for i in range(0,len(sentiments_dep)):
    sentiments_dep[i] = sentiments_dep[i][sentiments_dep[i].columns.drop(list(sentiments_dep[i].filter(regex='Episode_')))]
    sentiments_dep[i] = sentiments_dep[i][sentiments_dep[i].columns.drop(list(sentiments_dep[i].filter(regex='Period_')))]

sentiments_dep = pd.concat(sentiments_dep)
sentiments_dep.to_csv('Data/Sentiments/'+ tweet_type + '/VADER_ANEW_LIWC_complete_dep.csv',index = False)
