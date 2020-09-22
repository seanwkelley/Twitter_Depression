#Get the duration of individual depressive episodes and the time between episodes 
#Episodes are recoded as not depressed periods if they are less than 2 weeks long 
#Episodes that are at least 2 weeks long but separated by less than 2 weeks are merged together 

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
            handle_row = Participant_info.loc[Participant_info['Twitter_Handle'] == handle]
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
    handle_row = Participant_info.loc[Participant_info['Twitter_Handle'] == handle]
    episode_1 = str(handle_row['EndDate'])
    episode_1 = (handle_row['EndDate']).to_string()

    episode_1 = episode_1.split(' ')
    episodes = []
    for i in episode_1:
        episodes.append(i)
    episodes = episodes[4]
    return (episodes)
###########################################################################################
#read in LIWC text features 
os.chdir(path)

file = open('Data/Participant_Data/key.key', 'rb')
key = file.read()
file.close()


#results of sentiment analysis 
VADER_ANEW_LIWC = pd.read_csv(path + 'Data/Sentiments/' + tweet_type + '/' + 'VADER_ANEW_LIWC_complete_dep.csv',encoding="utf-8")
VADER_ANEW_LIWC = VADER_ANEW_LIWC[~VADER_ANEW_LIWC['Date'].isnull()]

#list of participant Ids
id_unique = list(VADER_ANEW_LIWC['Twitter_Handle'].unique())


#participants data file 
with open('Data/Participant_Data/Twitter_Participants.csv.encrypted', 'rb') as f:
    data = f.read()

#de-encrypt the Participants.csv data file 
fernet = Fernet(key)
encrypted = fernet.decrypt(data)


participants_encrypted = str(encrypted,'utf-8')
data = StringIO(participants_encrypted) 

Participant_info =pd.read_csv(data)

#keep participants from free recruitment and those who successfully completed the attention check 
#Participant_info = Participant_info[(Participant_info['OCI_6'].isnull()) | (Participant_info['OCI_6'] == 1)]

#Remove participants who don't have enough Twitter data (< 5 days with Tweets or <50% of Tweets in English)
Participant_info = Participant_info[Participant_info['Id'].isin(id_unique)]


file_names = list(Participant_info['Twitter_Handle'])
id_dict = dict(zip(Participant_info['Twitter_Handle'],Participant_info['Id']))


display_time = 3600*24*1

Participant_info['missing_days'] = ""

start_dates = []
unique_ids = []

#does the date fall within a depressive episode?
dep_ep = []


diff_ep_main = []

#between depressive episode duration
bet_ep_duration = []

episode_start = []; episode_end = []
for name in file_names:
    
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

            #set the start and end of depressive episodes in relation to the beginning date 
            start_depression = int(((start_depression_initial - min_date).total_seconds())/display_time)
            end_depression = int(((end_depression_initial - min_date).total_seconds())/display_time)

            #remove depressive episode shorter than 2 weeks and depressive episodes that occured entirely outside of the past year 
            if (end_depression - start_depression < 13) | (start_depression < 0 and end_depression < 0):
                continue

            #if the start date of an episode occurs before submission_date - 1 year then set the start date to 0
            if(start_depression < 0 and end_depression > 0):
                start_depression = 0

            #if the end date occurs after 365 (2 cases where a participant enters the day after the submission date), set the
            #end date to 365 
            if(start_depression >= 0 and end_depression > 365):
                end_depression = 365

            start.append(start_depression)
            end.append(end_depression)

    start.sort(); end.sort()

    dep_gap = 15
    
    #merge depressive episodes that are separted by less than 2 weeks 
    print(start)
    print(end)
    while len(start) >= 2:
        if len(start) == 2 and len(end) == 2:
            if(start[1] - end[0] < dep_gap):
                del start[1]; del end[0]
                continue
        #three depressive episodes 
        if len(start) == 3 and len(end) == 3:
            if(start[1] - end[0] < dep_gap):
                del start[1]; del end[0]
                continue
            if(start[2] - end[1] < dep_gap):
                del start[2]; del end[1]
                continue
        #four depressive episodes
        if len(start) == 4 and len(end) == 4:
            if(start[1] - end[0] < dep_gap):
                del start[1]; del end[0]
                continue
            if(start[2] - end[1] < dep_gap):
                del start[2]; del end[1]
                continue
            if(start[3] - end[2] < dep_gap): 
                del start[3]; del end[2]
                continue
        #five depressive episodes 
        if len(start) == 5 and len(end) == 5:
            if(start[1] - end[0] < dep_gap):
                del start[1]; del end[0]
                continue
            if(start[2] - end[1] < dep_gap):
                del start[2]; del end[1]
                continue
            if(start[3] - end[2] < dep_gap): 
                del start[3]; del end[2]
                continue
            if(start[4] - end[3] < dep_gap):     
                del start[4]; del end[3]
                continue
        break

    depression_times = {}
    depression_times['Start'] = start
    depression_times['End'] = end
    depressive_episodes = pd.DataFrame.from_dict(depression_times)

    print(depressive_episodes)
    

    missing_days = []
    depression_episode = []
    diff_ep = []
    sdate = []; edate = []
    bet_depression_episode = []

    sentiments_handle = VADER_ANEW_LIWC[VADER_ANEW_LIWC['Twitter_Handle'] == random_id]
    sentiments_handle['Day']  = sentiments_handle['Day'].astype(str).astype(int)

    
    for start in start:
        #number of days in the 21/44 day period prior to the onset of a depressive episode (14 + rolling window)
        missing_days.append(len(list(x for x in sentiments_handle['Day'] if (start-60) <= x <= (start - 1))))
        participant_id = str(sentiments_handle['Twitter_Handle'].unique())
        participant_id = participant_id.split("'")[1]
        unique_ids.append(participant_id)
     
    #number of days of Tweets within depressive episodes 
    for j in range(0,len(depressive_episodes['Start'])):
        depression_episode.append(len(list(x for x in sentiments_handle['Day'] if (depressive_episodes['Start'][j] <= x <= depressive_episodes['End'][j]))))

    #number of days between depressive episodes - in absolute days not relative to the number of days with Tweets 
    try:
        diff_ep.append(depressive_episodes['Start'][0])
        sdate.append(0); edate.append(depressive_episodes['Start'][0])

        for j in range(1,len(depressive_episodes['Start'])):
            diff_ep.append(depressive_episodes['Start'][j] - depressive_episodes['End'][j-1])
            sdate.append(depressive_episodes['End'][j-1]); edate.append(depressive_episodes['Start'][j])

    except (IndexError):
        continue

    for j in range(1,len(depressive_episodes['Start'])):
        bet_depression_episode.append(len(list(x for x in sentiments_handle['Day'] if (depressive_episodes['Start'][j] <= x <= depressive_episodes['End'][j-1]))))


    print(bet_depression_episode)
    start_dates.append(missing_days); dep_ep.append(depression_episode); diff_ep_main.append(diff_ep)
    episode_start.append(sdate); episode_end.append(edate)

#number of days of tweets in the 14 day period prior to tranitioning from non-depressed to depressed 
transition_period = (reduce(lambda x,y: x+y,start_dates))
episode_period = (reduce(lambda x,y: x+y,dep_ep))
between_episode_period = (reduce(lambda x,y: x+y,diff_ep_main))
start_episode = (reduce(lambda x,y: x+y,episode_start))
end_episode = (reduce(lambda x,y: x+y,episode_end))
#print(reduce(lambda sum, j: sum  + (1 if j >= 14 else 0), transition_period, 0)) 

CSD  = pd.DataFrame(columns=['Id', 'Depressive_Episode', 'Critical_Transition'])
CSD['Id'] = unique_ids
CSD['Depressive_Episode'] = pd.DataFrame(episode_period)
CSD['Critical_Transition'] = pd.DataFrame(transition_period)
CSD['Between_Episodes'] = pd.DataFrame(between_episode_period)
CSD['Start'] = pd.DataFrame(start_episode)
CSD['End'] = pd.DataFrame(end_episode)

CSD.to_csv(path + 'Data/Sentiments/' + tweet_type + '/' + 'CSD_Episodes_FYPSG.csv',index = False)

