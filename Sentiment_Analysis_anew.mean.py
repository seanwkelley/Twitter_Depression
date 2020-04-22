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


import cryptography
from cryptography.fernet import Fernet
from io import StringIO

from timezonefinder import TimezoneFinder 

tf = TimezoneFinder()
#need to download the stopwords the first time 
###########################################################################################
#Set path directory and type of information to run sentiment analysis on likes (0), retweets (1), tweets (2)
with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0]
tweet_type = set_wd_path[1]

VADER = []; ANEW = []
tweet_times = []

###########################################################################################
#define all functions 

def Clean_Tweets(input_text,sentiment_type) :

    #remove b from string 
    input_text = str(input_text)
    input_text = input_text[2:len(input_text)-1]
    input_text = " ".join((input_text).split())

    if sentiment_type == "ANEW":
        input_text =  ' '.join([input_text]).lower()


    #remove @s
    replies = re.findall("@[\w]*",input_text)
    for i in replies:
        input_text = re.sub(i,'',input_text)

    #remove URLs 
    urls = re.findall("https?://[A-Za-z0-9./]+",input_text)
    for i in urls:
        input_text = re.sub(i,'',input_text)
    
    #remove newlines 

    a = []
    if  len(re.findall(r"\\n", input_text)) == 0:  
        pass
    if  len(re.findall(r"\\n", input_text)) > 0:    
        for match in finditer(r'\\n',input_text):
            a.append(match.start())
            b = (np.asarray(a)) 
            b1 = (np.asarray(a)) + 1
            alpha = np.zeros(shape=(len(a),2))
            alpha[:,0] = b
            alpha[:,1] = b1
            alpha = (np.concatenate(alpha))
            alpha = [int(x) for x in alpha]
            newline_list = list(input_text)

        for i in range(len(alpha)):
            newline_list[alpha[i]] = " "
        newline_list = ''.join(newline_list)
        newline_list = ' '.join(newline_list.split())
        input_text = newline_list.strip()


    #convert unicode to ascii 
    input_text = (input_text.
            replace('\\xe2\\x80\\x99', "'").
            replace('\\xc3\\xa9', 'e').
            replace('\\xe2\\x80\\x90', '-').
            replace('\\xe2\\x80\\x91', '-').
            replace('\\xe2\\x80\\x92', '-').
            replace('\\xe2\\x80\\x93', '-').
            replace('\\xe2\\x80\\x94', '-').
            replace('\\xe2\\x80\\x94', '-').
            replace('\\xe2\\x80\\x98', "'").
            replace('\\xe2\\x80\\x9b', "'").
            replace('\\xe2\\x80\\x9c', '"').
            replace('\\xe2\\x80\\x9c', '"').
            replace('\\xe2\\x80\\x9d', '"').
            replace('\\xe2\\x80\\x9e', '"').
            replace('\\xe2\\x80\\x9f', '"').
            replace('\\xe2\\x80\\xa6', '...').#
            replace('\\xe2\\x80\\xb2', "'").
            replace('\\xe2\\x80\\xb3', "'").
            replace('\\xe2\\x80\\xb4', "'").
            replace('\\xe2\\x80\\xb5', "'").
            replace('\\xe2\\x80\\xb6', "'").
            replace('\\xe2\\x80\\xb7', "'").
            replace('\\xe2\\x81\\xba', "+").
            replace('\\xe2\\x81\\xbb', "-").
            replace('\\xe2\\x81\\xbc', "=").
            replace('\\xe2\\x81\\xbd', "(").
            replace('\\xe2\\x81\\xbe', ")")

                 )
    
    #remove emojis 
    a = []
    if  len(re.findall(r"\\x", input_text)) == 0:
        input_text = input_text
    if  len(re.findall(r"\\x", input_text)) > 0:    
        for match in finditer(r'\\x',input_text):
            a.append(match.start())
        b = (np.asarray(a)) 
        b1 = (np.asarray(a)) + 1
        b2 = (np.asarray(a)) + 2
        b3 = (np.asarray(a)) + 3

        alpha = np.zeros(shape=(len(a),4))
        alpha[:,0] = b
        alpha[:,1] = b1
        alpha[:,2] = b2
        alpha[:,3] = b3

        alpha = (np.concatenate(alpha))

        l = [i for i in range(len(input_text))]
        beta = (np.in1d(l, alpha))
        gamma = list(input_text)
        gamma_indx = []
        for i in range(len(beta)-1):
            if (((beta[i] == True) & (beta[i+1] == False)) | ((beta[i-1] == False) & (beta[i] == True))):
                gamma_indx.append(i) 
                gamma[i] = " "
        gamma = (''.join(gamma))

        res = [i for i, val in enumerate(beta) if not val] 
        new = gamma_indx + res
        new.sort()
        string1 = []
        for i in new:
            string1.append(gamma[i])
        string1  = ''.join(string1)
        string1 = ' '.join(string1.split())
        input_text = string1.strip()

    #fix contractions    
    input_text = contractions.fix(input_text)
    
    #remove punctuation, but keep periods, question marks, and exlclamation points
    #replace punctuation characters with a space - ensure words remain separated  
    punctuations = '''()-=+[]{};:"\,<>/|¦`$%^&*_~'''
    no_punct = ""
    for char in input_text:
       if char in punctuations:
           char = ' '
       if char not in punctuations:
           no_punct = no_punct + char
    input_text = " ".join(no_punct.split())

    #hastags
    input_text = tokenizer(input_text)
    a = []
    for token in input_text:
        a.append(token.text)
        ' '.join(a)
    for i in range(len(a)):
        if("#" in a[i]):
            t = word_tokenize(a[i])
            if(len(t) > 1):
                t = ' '.join(wordninja.split(t[1]))
                a[i] = t 
            else:
                  a[i] = ""     
    a = ' '.join(a)
    input_text = " ".join(a.split()) 

    #remove numbers 
    input_text = re.sub(r'\d+', '', input_text)

    #remove RT marker for retweets and any leading whitespaces 
    input_text = (input_text.replace('RT','')).lstrip()

    #replace amp with and  
    input_text = (input_text.replace('amp','and')).lstrip()

    return(input_text)

def sentiment_analyzer_scores(sentence):
    analyser = SentimentIntensityAnalyzer()
    score = analyser.polarity_scores(sentence)
    return score 

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

def get_diagnosis(handle):
    handle_row = Participant_info.loc[Participant_info['Twitter_Handle'] == handle]
    diagnosis = (handle_row['Depression_Physician']).to_string()
    diagnosis = diagnosis.split(" ")
    a = len(diagnosis) -1 

    if(diagnosis[a] == "no"):
        Depression_diagnosis = 0
    if(diagnosis[a] == "yes"):
        Depression_diagnosis = 1
    return (Depression_diagnosis)

def ever_episode(handle):
    handle_row = Participant_info.loc[Participant_info['Twitter_Handle'] == handle]
    diagnosis = (handle_row['Depressive_episode']).to_string()
    diagnosis = diagnosis.split(" ")
    a = len(diagnosis) -1 

    if(diagnosis[a] == "no"):
        Depressive_episode = 0
    if(diagnosis[a] == "yes"):
        Depressive_episode = 1
    return (Depressive_episode)

#Percent of tweets from an individual accoutnt that are in English
def is_english(sentiment_tweets):
    a = sentiment_tweets_vader['tidy_tweet']
    sum_english = []
    for i in a:
        try:
            if detect(i) == "en":
                sum_english.append(i)
        except:
            pass
    percent_english = len(sum_english)/len(a)
    return(percent_english)

def stopword_lemma(input_text):
    def get_wordnet_pos(word):
        tag = nltk.pos_tag([word])[0][1][0].upper()
        tag_dict = {"J": wordnet.ADJ,
                    "N": wordnet.NOUN,
                    "V": wordnet.VERB,
                    "R": wordnet.ADV}

        return tag_dict.get(tag, wordnet.NOUN)
    lemmatizer = WordNetLemmatizer()  
    sentence = [lemmatizer.lemmatize(w, get_wordnet_pos(w)) for w in nltk.word_tokenize(input_text)]
    filtered_sentence = ' '.join(sentence)
    return(filtered_sentence)   

def weighted_mean(values):
    weighted_avg = np.mean(values)
    return(weighted_avg)

def ANEW_Sentiment(test):
    ANEW = pd.read_csv('Data/ANEW_Library/ANEW2017All.txt', sep="\t")
    ANEW_words = list(ANEW['Word'])
    test = word_tokenize(test)
    val = []; aro = []; dom = []

    for word in ANEW_words:
        for token in test:
            if(token == word):
                i = ANEW_words.index(token)

                valence = ANEW.ValMn[i]
                arousal = ANEW.AroMn[i]
                dominance = ANEW.DomMn[i]

                val.append(valence)
                aro.append(arousal)
                dom.append(dominance)
    return(val,aro,dom)

#get the dates of all depressive episodes self-reported in the past year        
def get_dates(handle,episode_number):
        try: 
            handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
            episode_1 = str(handle_row['Dates_' + episode_number + '_episode'])
            episode_1 = (handle_row['Dates_' + episode_number + '_episode']).to_string()
            episode_1 = episode_1.split(' ')
            episodes = []
            for i in episode_1:
                if(len(i) > 3):
                    episodes.append(i)
        # except (KeyError,IndexError):
            episodes = episodes[0].split('-')
        except (KeyError,IndexError):
            pass
        return (episodes)
#replace Twitter handle with a random alphanumeric string  
def alpha_numeric_string():
    x = ''.join(random.choice(string.ascii_uppercase + string.ascii_lowercase + string.digits) for _ in range(16))
    return(x)

def recruitment_type(handle):
    handle_row = Dates.loc[Dates['Twitter_Handle'] == handle]
    recruit =  (handle_row['recruitment_type']).to_string()
    recruit = recruit.split(" ")
    recruit_2 = recruit[len(recruit) -1]
    return (recruit_2)

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
################################################################################################
os.chdir(path)

file = open('Data/Participant_Data/key.key', 'rb')
key = file.read()
file.close()

with open('Data/Participant_Data/FYP_Twitter_Participants.csv.encrypted', 'rb') as f:
    data = f.read()

#de-encrypt the Participants.csv data file 
fernet = Fernet(key)
encrypted = fernet.decrypt(data)


participants_encrypted = str(encrypted,'utf-8')
data = StringIO(participants_encrypted) 

Participant_info = Dates = pd.read_csv(data)

#get twitter handles from file names 
#dictionary of random ids and twitter handles 
id_dict = dict(zip(Participant_info['Twitter_Handle'],Participant_info['Id']))

file_names = list(Participant_info['Twitter_Handle'])


for i in range(0,len(file_names)):
    handle = file_names[i]
    os.chdir(path)

    print(handle)
    print(submission_date(handle))

    random_id = id_dict[handle]

    submission_date_initial = submission_date(handle) #study submission date
    recruitment = recruitment_type(handle)
    display_time = 3600*24*1

    print(recruitment) #recruitment type 

    #read in individual tweets 
    twitter_vader = pd.read_csv("Data/Raw_Tweets/" + recruitment + "/" + handle + "/" + handle + '_tweets.csv',encoding="utf-8")
    twitter_vader.columns = ['id','time','tweets','favorites','reply']

    #if running on all tweets (likes/retweets/tweets) then we want to skip the subsetting process
    if tweet_type != 'all_tweets':
        twitter_vader = twitter_vader[twitter_vader['favorites'] == tweet_type]
    #no tweets of desired sub-type then want to skip 
    if len(twitter_vader['favorites'] == tweet_type) == 0:
        continue
    else:
        pass

    print(random_id)

    #get times of all tweets (tweets/retweets/likes)    

    '''
    hours = np.vectorize(local_timezone_correction)(twitter_vader['time'],handle,"yes")
    hours = pd.DataFrame(hours)
    if tweet_type != 'all_tweets':
        hours['tweet_type'] = tweet_type
    else:
        hours['tweet_type'] = twitter_vader['favorites']
    hours['Twitter_Handle'] = random_id
    hours.columns = ['time','tweet_type','Twitter_Handle']
    tweet_times.append(hours)
    '''
    ######################################################################
    twitter_vader['time'] = np.vectorize(local_timezone_correction)(twitter_vader['time'],handle,"no")
    #Add UTC offset to GMT times
    submission_date2 = parser.parse(submission_date_initial)
    beginning_date = submission_date2 - relativedelta(years=1)
    
    #restrict range of tweets to the past year 
    twitter_vader = twitter_vader[(twitter_vader['time'] >= beginning_date) & (twitter_vader['time'] <= submission_date2)]
    min_date = datetime(beginning_date.year, beginning_date.month, beginning_date.day)
    twitter_vader['min_date'] = min_date
    twitter_vader['delta_time'] =  ((twitter_vader['time']) -  (twitter_vader['min_date']))/np.timedelta64(1, 's')

    #sort the times from beginning to end 
    twitter_vader = twitter_vader.sort_values(by=['delta_time'], ascending=True)
    twitter_vader['delta_time'] = (twitter_vader['delta_time']/(display_time)).astype(int)

    #if there are no tweets of the desired type in the past year then skip to the next participant 
    if(len(twitter_vader['favorites'] == tweet_type) == 0):
        continue  
    else:
        pass

    #ANEW processing diverges from VADER processing here  
    twitter_anew = twitter_vader

   
    twitter_vader['tidy_tweet'] = np.vectorize(Clean_Tweets)(twitter_vader['tweets'],"VADER")
 
    sentiment_tweets_vader = pd.DataFrame(twitter_vader.groupby('delta_time')['tidy_tweet'].apply(list))
    sentiment_tweets_vader.index.name = "Day"
    sentiment_tweets_vader.reset_index(inplace=True)


    sentiment_tweets_vader.tidy_tweet = ([' '.join(x) for x in sentiment_tweets_vader.tidy_tweet])
    sentiment_tweets_vader['scores'] = np.vectorize(sentiment_analyzer_scores)(sentiment_tweets_vader['tidy_tweet'])


    scores_list = []
    for i in sentiment_tweets_vader.scores:
        scores =list(i.values())
        scores_list.append(scores)

    df = pd.DataFrame(np.array(scores_list).reshape(len(scores_list),4),columns =  ['negative','neutral','positive','compound'])
    sentiment_tweets_vader = pd.concat([sentiment_tweets_vader.reset_index(drop=True), df.reset_index(drop=True)], axis=1)
    sentiment_tweets_vader = sentiment_tweets_vader.drop('scores',axis=1)

    #add type of recruitment here either clickworker or free 
    sentiment_tweets_vader['recruitment_type'] = recruitment

    todays_date = []
    for date in sentiment_tweets_vader['Day']:
        todays_date.append(beginning_date + relativedelta(days = date))

    sentiment_tweets_vader['Date'] = pd.DataFrame(todays_date)
    sentiment_tweets_vader = sentiment_tweets_vader[['Date', 'Day', 'tidy_tweet', 'negative', 'neutral','positive','compound','recruitment_type']]

    #number of days with Tweets in the past year 
    number_days = int(len(sentiment_tweets_vader['Day']))
       
    #####################################################################################################
    #ANEW Sentiment Analysis
    #####################################################################################################
    twitter_anew['tidy_tweet'] = np.vectorize(Clean_Tweets)(twitter_anew['tweets'],"ANEW")
    twitter_anew['tidy_tweet'] = np.vectorize(stopword_lemma)(twitter_anew['tidy_tweet'])
    
    sentiment_tweets_anew = pd.DataFrame(twitter_anew.groupby('delta_time')['tidy_tweet'].apply(list))
    sentiment_tweets_anew.index.name = 'Day'
    sentiment_tweets_anew.reset_index(inplace=True)
    sentiment_tweets_anew.tidy_tweet = ([' '.join(x) for x in sentiment_tweets_anew.tidy_tweet])

    #sentiment analysis using ANEW

    valence = []; arousal = []; dominance = []

    for i in sentiment_tweets_anew.tidy_tweet:
        i = str(i)
        if(i != "nan"):
            valence.append(weighted_mean(ANEW_Sentiment(i)[0]))
            arousal.append(weighted_mean(ANEW_Sentiment(i)[1]))
            dominance.append(weighted_mean(ANEW_Sentiment(i)[2]))
        if(i == "nan"):
            valence.append("NaN")
            arousal.append("NaN")
            dominance.append("NaN")


    sentiment_tweets_anew['valence'] = valence
    sentiment_tweets_anew['arousal'] = arousal
    sentiment_tweets_anew['dominance'] = dominance
    #################################################################
    #################################################################

    if number_days >= 5  and is_english(sentiment_tweets_vader) >= 0.50:
        #the LIWC is a copy of VADER output 
        VADER_df = sentiment_tweets_vader
        ANEW_df = sentiment_tweets_anew

        ANEW_df['Twitter_Handle'] = random_id
        VADER_df['Twitter_Handle'] = random_id

        VADER.append(VADER_df)  
        ANEW.append(ANEW_df)  


VADER = pd.concat(VADER)
ANEW = pd.concat(ANEW)
ANEW = ANEW.drop("tidy_tweet",axis=1)


VADER_ANEW_LIWC = pd.merge(VADER, ANEW, on=['Twitter_Handle','Day']) 

VADER_ANEW_LIWC = VADER_ANEW_LIWC[['Date','Day', 'Twitter_Handle', 'tidy_tweet', 'recruitment_type','negative','neutral',
'positive','compound','valence','arousal','dominance']]

VADER_ANEW_LIWC.to_csv('Sentiments/' +tweet_type + '/VADER_ANEW_LIWC.csv',index = False)

# #times when tweets were posted (tweets/retweets/likes) 
# #like times reflect when the original tweet was posted and not when the tweet was liked  
# tweet_times = pd.concat(tweet_times)
# tweet_times.to_csv('Tweet_Times/'+ tweet_type + '/tweet_times.csv',index = False)