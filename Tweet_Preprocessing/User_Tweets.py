#Download last 3200 Tweets and 3200 likes from a given user's account 
 
#!/usr/bin/env python
# encoding: utf-8

import tweepy #https://github.com/tweepy/tweepy
import csv
import pandas as pd
import json
import os 
import numpy as np
import re

with open("set_wd_path.txt","r") as f:
    path = f.readlines()
set_wd_path = [x.strip('\n') for x in path]

path = set_wd_path[0] + "Raw_Tweets/"

os.chdir(path)

#Twitter API credentials, need to add Keys but removed for public distribution 
consumer_key = ''
consumer_secret = ''
access_key = ''
access_secret = ''

#determine if a tweet is a retweet 
def find_retweets(input_text):
	input_text = str(input_text)
	value = 'tweet'
	patternObj = re.compile("RT")
	match = patternObj.search(input_text)
	try: 
	    if ((match.span()[0] == 2) & (match.span()[1] == 4)):
	        value = 'retweet'
	except AttributeError:
	    pass
	return(value)

#determine if a tweet is a reply to another tweet 
def find_replies(input_text):
	input_text = str(input_text)
	value = 'not_reply'
	patternObj = re.compile("@")
	match = patternObj.search(input_text)
	try: 
	    if ((match.span()[0] == 2) & (match.span()[1] == 3)):
	        value = 'reply'
	except AttributeError:
	    pass
	return(value)


def get_all_tweets(screen_name):
	#Twitter only allows access to a users most recent 3240 tweets with this method
	os.mkdir(screen_name)
	os.chdir(screen_name)
	auth = tweepy.OAuthHandler(consumer_key, consumer_secret)
	auth.set_access_token(access_key, access_secret)
	api = tweepy.API(auth)
	

	alltweets = []	
	
	#make initial request for most recent tweets (200 is the maximum allowed count)
	new_tweets = api.user_timeline(screen_name = screen_name, count=200, tweet_mode='extended')
	
	#save most recent tweets
	alltweets.extend(new_tweets)
	
	#save the id of the oldest tweet less one
	oldest = alltweets[-1].id - 1
	
	#keep grabbing tweets until there are no tweets left to grab
	while len(new_tweets) > 0:
		try:
			print("getting tweets before %s" % (oldest))
			
			#all subsiquent requests use the max_id param to prevent duplicates
			new_tweets = api.user_timeline(screen_name = screen_name,count=200, tweet_mode='extended', max_id=oldest)
			
			#save most recent tweets
			alltweets.extend(new_tweets)
			
			#update the id of the oldest tweet less one
			oldest = alltweets[-1].id - 1
			
			print("...%s tweets downloaded so far" % (len(alltweets)))
		except tweepy.TweepError:
		    print('exception raised, waiting 15 minutes')
		    print('(until:', dt.datetime.now()+dt.timedelta(minutes=15), ')')
		    time.sleep(15*60)
	
	#transform the tweepy tweets into a 2D array that will populate the csv	
	outtweets = [[tweet.id_str, tweet.created_at, tweet.full_text.encode("utf-8")] for tweet in alltweets]

	#output tweets as a .json file 
	filename = screen_name + ".json" 
	with open(filename, 'a') as f:
	    for tweet in alltweets:
	        json.dump(tweet._json, f)
	        f.write('\n')
	pass

	##################################################################################################
	#favorite Tweets, get likes from account 
	alltweets = []	
	
	#make initial request for most recent tweets (200 is the maximum allowed count)
	new_tweets = api.favorites(screen_name = screen_name, count=200, tweet_mode='extended')
	
	#save most recent tweets
	alltweets.extend(new_tweets)
	
	#save the id of the oldest tweet less one
	try: 
		oldest = alltweets[-1].id - 1
		
		#keep grabbing tweets until there are no tweets left to grab
		while len(new_tweets) > 0:
			
			try:
				print("getting tweets before %s" % (oldest))
				
				#all subsiquent requests use the max_id param to prevent duplicates
				new_tweets = api.favorites(screen_name = screen_name,count=200, tweet_mode='extended', max_id=oldest)
				
				#save most recent tweets
				alltweets.extend(new_tweets)
				
				#update the id of the oldest tweet less one
				oldest = alltweets[-1].id - 1
				
				print("...%s tweets downloaded so far" % (len(alltweets)))
			
			except tweepy.TweepError:
			    print('exception raised, waiting 15 minutes')
			    print('(until:', dt.datetime.now()+dt.timedelta(minutes=15), ')')
			    time.sleep(15*60)
		#transform the tweepy tweets into a 2D array that will populate the csv	
		outtweets2 = [[tweet.id_str, tweet.created_at, tweet.full_text.encode("utf-8")] for tweet in alltweets]

		#save additional metadata into a json file 
		filename = screen_name + "_favorites.json" 
		with open(filename, 'a') as f:
		    for tweet in alltweets:
		        json.dump(tweet._json, f)
		        f.write('\n')
		
		#tweets
		outtweets_pd = pd.DataFrame(outtweets, columns = ["id","created_at","text"])
		outtweets_pd['created_at'] = pd.to_datetime(outtweets_pd['created_at'])
		outtweets_pd['favorites'] = 1 
		outtweets_pd = outtweets_pd.sort_values(by='created_at',ascending = False)

		#likes 
		outtweets2_pd = pd.DataFrame(outtweets2, columns = ["id","created_at","text"])
		outtweets2_pd['created_at'] = pd.to_datetime(outtweets2_pd['created_at'])
		outtweets2_pd['favorites'] = 0 
		outtweets2_pd = outtweets2_pd.sort_values(by='created_at',ascending = False)

		final = pd.concat([outtweets_pd, outtweets2_pd])

		tweets = final[final['favorites'] == 1]
		likes = final[final['favorites'] == 0]
		likes['tweet_type'] = 'like'
		tweets['tweet_type'] = np.vectorize(find_retweets)(tweets['text'])

		tweets.reset_index(drop=True, inplace=True)
		likes.reset_index(drop=True, inplace=True)
		twitter2=pd.concat([likes,tweets], axis=0)

		twitter2 = twitter2.sort_values(by=['tweet_type'])
		twitter2 = twitter2.drop(columns="favorites")
		twitter2 = twitter2.sort_values(by=['tweet_type','created_at'])

		twitter2['reply'] = np.vectorize(find_replies)(twitter2['text'])

		twitter2.to_csv(screen_name + "_tweets.csv",index = False)

	except IndexError:
		outtweets_pd = pd.DataFrame(outtweets, columns = ["id","created_at","text"])
		outtweets_pd['created_at'] = pd.to_datetime(outtweets_pd['created_at'])
		outtweets_pd['favorites'] = 1 
		outtweets_pd = outtweets_pd.sort_values(by='created_at',ascending = False)

		
		final = outtweets_pd

		tweets = final[final['favorites'] == 1]
		tweets['tweet_type'] = np.vectorize(find_retweets)(tweets['text'])

		tweets.reset_index(drop=True, inplace=True)
		twitter2=tweets

		twitter2 = twitter2.sort_values(by=['tweet_type'])
		twitter2 = twitter2.drop(columns="favorites")
		twitter2 = twitter2.sort_values(by=['tweet_type','created_at'])

		twitter2['reply'] = np.vectorize(find_replies)(twitter2['text'])

		twitter2.to_csv(screen_name + "_tweets.csv",index = False)

	pass
	
if __name__ == '__main__':
	#pass in the username of the account you want to download
	get_all_tweets("")