# Within-subject changes in network connectivity occur during an episode of depression: evidence from a longitudinal analysis of social media posts.

The analysis code is separated into 4 folders: Tweet Preprocessing, Network Analysis, Control Analysis, and Graphs.
Tweet Preprocessing contains all the code necessary to download Tweets from an individual user account and process those Tweets into a format that is 
suitable for text analysis. Network Analysis has the code to construct personalised networks for all participants and for within/outside depressive 
episode periods in a subset of participants. Control Analysis contains code to run a permutation test of within-episode days, generate random networks for within and outside 
depressive episode periods, and construct a network without 3rd person pronouns. Finally, Graphs has the code to reproduce all figures and tables in the 
main and supplementary text. 

## OS Requirements
This software is supported for macOs. The package has been tested on the following system:

macOS: Mojave (10.14.1)


## Python Dependencies

pandas 
numpy 
nltk
dateutil
datetime
langdetect
seaborn 
re
spacy
shutil
stat

## R Dependencies

dplyr
ggplot2
lmerTest
broom
reshape2
stringr
roll
zoo
rlang)
bootnet
qraph
graphicalVAR

## Tweet Preprocessing

### 1. User_Tweets.py 

Downloads the last 3,200 Tweets and 3,200 likes from a participants account. It also determines whether an individual Tweets is 
also a reply to another Tweet or a like

### 2. Textual_Analysis.py 

Cleans all the Tweets, e.g. removes URLS, emojis, unnecessary puntuation, and other alphanumeric characters for participants with at least 5 days of Tweets
in the past year and at least 50% of Tweets in English. Once the Tweets are cleaned, they are appended to a single csv file that contains the date the tweet was 
written and the text for all participants. This csv file is then fed into the LIWC which analyses all the text and outputs 87 different text features

### 3. Rename_LIWC.py 

Renames the columns that the LIWC changes in the process of text analysis

### 4. Percent_Tweets_English.py 

Calculates the percentage of Tweets from each account that are in English

### 5. Encrypt_File 

Encrypts and de-encrpts participant data  

### 6. Twitter_Metadata.py 

Gets additional, non-text, information from participant accounts including the number of followees/followers and absolute number of likes/retweets/tweets

## Network Analysis

### 1. personalizednetworks_depressive_episode.R 

Constructs personalised networks within and outside an episode for participants with at least 15 days of Tweets in both periods (N = 286). 
Networks are constructed from the following 9 text features that were found to be significantly associated with current depression in de Choudhury et al.(2013):
1st person singular/plural, 2nd person singular, 3rd person pronouns, negative and positive emotions, articles, swear words, and negation words
We examined the association between individual and global network connectivity within vs. outside a depressive episode.

### 2. personalizednetworks_depression_severity.R

Contructs personalised networks using the same 9 text features as above for all participants with at least 30 days of Tweets (N = 946).
We then correlated the individual and global network connectivity with current depression severity. 

## Control Analysis 

### 1. randomize_episode_identifier.R 

A permutation test that randomly shuffles the episode period identifer (within vs. outside a depressive episode) within-subject. This
preserves the total number of days that are within/outside an outisde and just changes the associated text features associated with those days. 
Networks are still constructed using the same 9 LIWC text features used in the main network analysis.

### 2. random_networks.R 

Generates personalised networks constructed from features that are either significantly or not-significantly associated with current depression severity. 
Using these new features, networks are constructed for both within and outside episode periods. This allows us to test the specificity of our findings to the particular 
selection of LIWC text features. 

### 3. personalised_networks_depression_severity_no_pro3.R

Personalised networkes of all participants (N = 946) with at least 30 days of Tweets constructed from 8 LIWC text features, omitting 3rd person pronouns. 

### 4. hierarchical_random_networks.R

100 random random networks from text features that are either significantly or not-significantly associated with current depression severity, excluding LIWC supra-categories


## Graphs

### 1. Figure1.R 

Associations between current depression severity and 9 LIWC text features averaged over the past year

### 2. Figure2AB.R/Figure2C.R 

Figure 2 is the association between individual and global network connectivity with current depression severity. 

### 3. Figure3.R 

Association between within-episode period and individual/global network strength 

### 4. Figure4.R 

The generality of within-subject changes in network connectivity to other depression-relevant networks

### 5. Table1.R 

Demographic and Twitter characteristics of full sample and split by participants who did/did not report a depressive episode in the past year

### 6. Table2.R 

Association between mean text features and depression severity

### 7. FigureS1A.R/FigureS1BC.R

Half-Split reliability of primary 9 node network.

### 8. FigureS2.R

Random networks of 9 text features either significantly (‘Depression Relevant’) or not significantly (‘Depression Irrelevant’) associated with current depression excluding LIWC supra-categories. 

### 9. FigureS2.R

The association between network connectivity and depression severity after removal of 3rd person pronouns 

### 10. FigureS3.R

The effect of days on global network connectivity.

### 11. FigureS4.R

Tolerance of within subject analysis to the removal of 3rd person pronouns

### 12. FigureS5.R

Bootstrapped regression coefficient of change in network connectivity within a depressive episode from 80% random sub-samples of the data repeated 1,000 times

### 13. FigureS6.R

Permutation test randomizing within versus outside episode indicators. 

### 14. TableS1.R

Demographic and Twitter use characteristics of participants through either paid or free recruitment channels 

### 15. TableS2.R

Proportion of days with non-zero values for each of the 9 a priori text features

### 16. TableS3.R

Association between depression severity and increased network connectivity after controlling for number of days 

### 17. TableS4.R

Mixed effects models of mean level changes in 9 LIWC text features within a depressive episdode  

### 18. TableS5.R

Bivariate correlation between current depression severity and all LIWC text features 





