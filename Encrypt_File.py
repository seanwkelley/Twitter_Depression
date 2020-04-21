import os
import xlsxwriter
import xlrd
import pandas as pd
import numpy as np
import random, string
from numpy.random import seed
from numpy.random import randint
import cryptography
from cryptography.fernet import Fernet
from io import StringIO
#need to download the stopwords the first time 
#############################################################################################################
#Set path directory and type of information to run sentiment analysis on likes (0), retweets (1), tweets (2)
# path="D:/Analysis2_Oct2019/"
path="D:/Twitter_Depression_Kelley/Data/Participant_Data/"
os.chdir(path)

##############################
#generate a key for encyrption 
##############################

# key = Fernet.generate_key()
# # print(key)

# file = open('key.key', 'wb') #wb = write bytes
# file.write(key)
# file.close()


file = open('key.key', 'rb')
key = file.read()
file.close()


# #####################################
# #Encrypt the Participants.csv
# #Open the file to encrypt
with open('Data/Participant_Data/FYP_Twitter_Participants.csv', 'rb') as f:
    data = f.read()

fernet = Fernet(key)
encrypted = fernet.encrypt(data)

# Write the encrypted file
with open('Data/Participant_Data/FYP_Twitter_Participants.csv.encrypted', 'wb') as f:
    f.write(encrypted)

# file = open('key.key', 'rb')
# key = file.read()
# file.close()

#####################################
#Open the encrypted file to decrypt
# with open('Participants.csv.encrypted', 'rb') as f:
#     data = f.read()

# fernet = Fernet(key)
# encrypted = fernet.decrypt(data)


# s=str(encrypted,'utf-8')

# data = StringIO(s) 

# Participants_info=pd.read_csv(data)


# print(Participants_info.head())
# # Open the decrypted file
# with open('secretdf.csv.decrypted', 'wb') as f:
#     f.write(encrypted)