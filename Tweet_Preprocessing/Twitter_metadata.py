#Sentiment Analysis of all participants in Participants.csv file 

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
from datetime import datetime
from dateutil import tz
from langdetect import detect
from langdetect import detect_langs
from langdetect import DetectorFactory 
DetectorFactory.seed = 0
import random, string
from numpy.random import seed
from numpy.random import randint
from timezonefinder import TimezoneFinder 

tf = TimezoneFinder()

import cryptography
from cryptography.fernet import Fernet
from io import StringIO
#need to download the stopwords the first time 
###########################################################################################
#Set path directory and type of information to run sentiment analysis on likes (0), retweets (1), tweets (2)
with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0]
tweet_type = set_wd_path[1]

followers = [] ; followees = [];volume = []; reply = []
tweet_num = []; like_num = []; retweet_num = []
###########################################################################################


def local_timezone_correction(time_object,timezone,hours):
    if hours == "no":
        handle_row = Participant_info.loc[Participant_info['Twitter_Handle'] == handle]

        timezone = find_timezone(handle)

        #timezone = str(handle_row['Timezone'])
        local_timezone =  tz.gettz(timezone) # get pytz tzinfo
        utc_time = datetime.strptime(time_object, "%Y-%m-%d %H:%M:%S")
        local_time = utc_time.replace(tzinfo=pytz.utc).astimezone(local_timezone)
        local_time2 = local_time.replace(tzinfo = None)
    if hours == "yes":
        handle_row = Participant_info.loc[Participant_info['Twitter_Handle'] == handle]

        timezone = find_timezone(handle)
        #timezone = str(handle_row['Timezone'])

        local_timezone =  tz.gettz(timezone) # get pytz tzinfo
        utc_time = datetime.strptime(time_object, "%Y-%m-%d %H:%M:%S")
        local_time = utc_time.replace(tzinfo=pytz.utc).astimezone(local_timezone)
        local_time2 = local_time.replace(tzinfo = None)
        local_time2 = local_time2.strftime('%H:%M')
    return(local_time2)

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


def recruitment_type(handle):
    handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
    recruit =  (handle_row['recruitment_type']).to_string()
    recruit = recruit.split(" ")
    recruit_2 = recruit[len(recruit) -1]
    return (recruit_2)

def alpha_numeric_string(num):
    id_list = []
    for i in range(0,num):
        x = ''.join(random.choice(string.ascii_uppercase + string.ascii_lowercase + string.digits) for _ in range(16))
        id_list.append(x)
    return(id_list)

def find_timezone(handle):
    handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]

    latitude =  (handle_row['LocationLatitude']).to_string()
    longitude =  (handle_row['LocationLongitude']).to_string()

    latitude = latitude.split(" ")
    longitude = longitude.split(" ")

    latitude = float(latitude[len(latitude) -1])
    longitude = float(longitude[len(longitude) -1])
    timezone = tf.timezone_at(lng=longitude, lat = latitude)

    return(timezone)

###########################################################################################
os.chdir(path)

Participant_info = Dates = pd.read_csv('Data/Participant_Data/FYP.SG_Twitter_Participants.csv')

#get twitter handles from file names 
file_names = list(Participant_info['Twitter_Handle'])

for i in range(0,len(file_names)):
    handle = file_names[i]

    os.chdir(path)
    print(handle)

    recruitment = recruitment_type(handle)
    print(recruitment)
    display_time = 3600*24
    # twitter_json = pd.read_json("Data/Raw_Tweets/" + recruitment + '/' + handle + "/" + handle + '.json',encoding="utf-8",lines=True)
    # user_profile = str(twitter_json['user'][0])
    # followers_count = int((re.search("'followers_count':(.*), 'friends_count':", user_profile)).group(1))
    # followees_count = int((re.search("'friends_count':(.*), 'listed_count':", user_profile)).group(1))

    twitter_vader = pd.read_csv("Data/Raw_Tweets/" + recruitment + '/' + handle + "/" + handle + '_tweets.csv',encoding="utf-8")
    twitter_vader.columns = ['id','time','tweets','favorites','reply']
    twitter_vader['time'] = np.vectorize(local_timezone_correction)(twitter_vader['time'],handle,"no")

    submission_date_initial = submission_date(handle)

    twitter_vader['TimeZone'] = find_timezone(handle)
    #Add UTC offset to GMT times
    submission_date2 = parser.parse(submission_date_initial)
    beginning_date = submission_date2 - relativedelta(years=1)
    
    print(submission_date2)
    print(beginning_date)

    #restrict range of tweets to the past year 
    twitter_vader = twitter_vader[(twitter_vader['time'] >= beginning_date) & (twitter_vader['time'] <= submission_date2)]
    min_date = datetime(beginning_date.year, beginning_date.month, beginning_date.day)
    twitter_vader['min_date'] = min_date
    twitter_vader['delta_time'] =  ((twitter_vader['time']) -  (twitter_vader['min_date']))/np.timedelta64(1, 's')

    #sort the times from beginning to end 
    twitter_vader = twitter_vader.sort_values(by=['delta_time'], ascending=True)
    twitter_vader['delta_time'] = (twitter_vader['delta_time']/(display_time)).astype(int)

    #find the volume of tweets posted per day, excluding likes since the time indicates when the tweet was posted not liked
    twitter_volume = twitter_vader[twitter_vader['favorites']!='like']
    volume_tweets = twitter_volume.groupby('delta_time').size()
    volume_tweets = volume_tweets.mean()


    #proportion of reply posts per day 
    twitter_reply = twitter_vader[(twitter_vader['favorites']=='tweet') & (twitter_vader['reply'] == 'reply')]
    reply_tweets = twitter_reply.groupby('delta_time').size()
    reply_tweets = reply_tweets.mean()

    tweet_num.append(twitter_vader[(twitter_vader['favorites']=='tweet')].shape[0])
    retweet_num.append(twitter_vader[(twitter_vader['favorites']=='retweet')].shape[0])
    like_num.append(twitter_vader[(twitter_vader['favorites']=='like')].shape[0])

    # followers.append(followers_count)
    # followees.append(followees_count)
    volume.append(volume_tweets)
    reply.append(reply_tweets)

# Participant_info['followers'] = pd.DataFrame(followers)
# Participant_info['followees'] = pd.DataFrame(followees)
Participant_info['volume'] = pd.DataFrame(volume)
Participant_info['reply'] = pd.DataFrame(reply)


Participant_info['like'] = pd.DataFrame(like_num)
Participant_info['retweet'] = pd.DataFrame(retweet_num)
Participant_info['tweet'] = pd.DataFrame(tweet_num)

#use times from all tweets to derive the following results
#insomnia index 

Participant_info.to_csv('Data/Participant_Data/FYP.SG_Twitter_Participants2.csv',index = False)