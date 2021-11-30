#--------------------------------------------------
#Table S2. Proportion of days with non-zero values for each of the 9 a priori text features
#--------------------------------------------------

#Load necessary packages 
library(lmerTest)
library(readr)
library(tidyr)
library(ggplot2)
library(Hmisc)
library(plyr)
library(dplyr)
library(RColorBrewer)
library(reshape2)
library(patchwork)
library(qgraph)
library(bootnet)
library(DescTools)
library(extrafont)
loadfonts(device = "win")


setwd('/Users/seankelley/Twitter_Depression_Kelley/')

#remove outliers 
remove_outliers <- function(x, na.rm = TRUE, ...) {
  s <- sd(x)
  m <- mean(x)
  y <- x
  y[x > (m + 3*s)] <- NA
  y[x < (m - 3*s)] <- NA
  y
}
# function to apply to all rows
remove_all_outliers <- function(d){
  d[] <- lapply(d, function(x) if (is.numeric(x))
    remove_outliers(x) else x)
  d
}

############################################################

#load in strength centralities of participants with >= 30 days of TWeets
dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_net$Id <- dc_net[,1]
dc_net <- dc_net[,-1]
dc_net[,1:10] <- remove_all_outliers(dc_net[1:10])


#set up empty dataframe 
coeff.df = data.frame(matrix(vector(),10 , 7,
                             dimnames=list(c(), c("LIWC_Feature","Beta_unadj","SE_unadj",
                                                  "pval_unadj","Beta_adj","SE_adj","pval_adj"))),
                      stringsAsFactors=F)


#Unadjusted glm models
MC.unadj <- summary(glm(Mean_Centrality ~ Depression_zscore,data = dc_net))
negemo.unadj <- summary(glm(ngm~ Depression_zscore,data = dc_net))
posemo.unadj <- summary(glm(psm ~ Depression_zscore,data = dc_net))
i.unadj <- summary(glm(i ~ Depression_zscore,data = dc_net))
we.unadj <- summary(glm(we ~ Depression_zscore,data = dc_net))
pro3.unadj <- summary(glm(pr3 ~ Depression_zscore,data = dc_net))
you.unadj <- summary(glm(you ~ Depression_zscore,data = dc_net))
swear.unadj <- summary(glm(swr ~ Depression_zscore,data = dc_net))
article.unadj <- summary(glm(art ~ Depression_zscore,data = dc_net))
negate.unadj <- summary(glm(ngt ~ Depression_zscore,data = dc_net))

#adjusted for days glm models
MC.adj <- summary(glm(Mean_Centrality ~ Depression_zscore + Days,data = dc_net))
negemo.adj <- summary(glm(ngm~ Depression_zscore + Days,data = dc_net))
posemo.adj <- summary(glm(psm ~ Depression_zscore + Days,data = dc_net))
i.adj <- summary(glm(i ~ Depression_zscore + Days,data = dc_net))
we.adj <- summary(glm(we ~ Depression_zscore + Days,data = dc_net))
pro3.adj <- summary(glm(pr3 ~ Depression_zscore + Days,data = dc_net))
you.adj <- summary(glm(you ~ Depression_zscore + Days,data = dc_net))
swear.adj <- summary(glm(swr ~ Depression_zscore + Days,data = dc_net))
article.adj <- summary(glm(art ~ Depression_zscore + Days,data = dc_net))
negate.adj <- summary(glm(ngt ~ Depression_zscore + Days,data = dc_net))

coeff.df[1,1] <- "Global Network Strength"; coeff.df[1,2] <- coefficients(MC.unadj)[2]; coeff.df[1,3] <- coefficients(MC.unadj)[4]; coeff.df[1,4] <- coefficients(MC.unadj)[8]
coeff.df[1,5] <- coefficients(MC.adj)[2]; coeff.df[1,6] <- coefficients(MC.adj)[5]; coeff.df[1,7] <- coefficients(MC.adj)[11]

coeff.df[2,1] <- "Article"; coeff.df[2,2] <- coefficients(article.unadj)[2]; coeff.df[2,3] <- coefficients(article.unadj)[4]; coeff.df[2,4] <- coefficients(article.unadj)[8]
coeff.df[2,5] <- coefficients(article.adj)[2]; coeff.df[2,6] <- coefficients(article.adj)[5]; coeff.df[2,7] <- coefficients(article.adj)[11]

coeff.df[3,1] <- "1st Person Singular"; coeff.df[3,2] <- coefficients(i.unadj)[2]; coeff.df[3,3] <- coefficients(i.unadj)[4]; coeff.df[3,4] <- coefficients(i.unadj)[8]
coeff.df[3,5] <- coefficients(i.adj)[2]; coeff.df[3,6] <- coefficients(i.adj)[5]; coeff.df[3,7] <- coefficients(i.adj)[11]

coeff.df[4,1] <- "Negation"; coeff.df[4,2] <- coefficients(negate.unadj)[2]; coeff.df[4,3] <- coefficients(negate.unadj)[4]; coeff.df[4,4] <- coefficients(negate.unadj)[8]
coeff.df[4,5] <- coefficients(negate.adj)[2]; coeff.df[4,6] <- coefficients(negate.adj)[5]; coeff.df[4,7] <- coefficients(negate.adj)[11]

coeff.df[5,1] <- "Negemo"; coeff.df[5,2] <- coefficients(negemo.unadj)[2]; coeff.df[5,3] <- coefficients(negemo.unadj)[4]; coeff.df[5,4] <- coefficients(negemo.unadj)[8]
coeff.df[5,5] <- coefficients(negemo.adj)[2]; coeff.df[5,6] <- coefficients(negemo.adj)[5]; coeff.df[5,7] <- coefficients(negemo.adj)[11]

coeff.df[6,1] <- "Posemo"; coeff.df[6,2] <- coefficients(posemo.unadj)[2]; coeff.df[6,3] <- coefficients(posemo.unadj)[4]; coeff.df[6,4] <- coefficients(posemo.unadj)[8]
coeff.df[6,5] <- coefficients(posemo.adj)[2]; coeff.df[6,6] <- coefficients(posemo.adj)[5]; coeff.df[6,7] <- coefficients(posemo.adj)[11]

coeff.df[7,1] <- "3rd Person"; coeff.df[7,2] <- coefficients(pro3.unadj)[2]; coeff.df[7,3] <- coefficients(pro3.unadj)[4]; coeff.df[7,4] <- coefficients(pro3.unadj)[8]
coeff.df[7,5] <- coefficients(pro3.adj)[2]; coeff.df[7,6] <- coefficients(pro3.adj)[5]; coeff.df[7,7] <- coefficients(pro3.adj)[11]

coeff.df[8,1] <- "Swear"; coeff.df[8,2] <- coefficients(swear.unadj)[2]; coeff.df[8,3] <- coefficients(swear.unadj)[4]; coeff.df[8,4] <- coefficients(swear.unadj)[8]
coeff.df[8,5] <- coefficients(swear.adj)[2]; coeff.df[8,6] <- coefficients(swear.adj)[5]; coeff.df[8,7] <- coefficients(swear.adj)[11]

coeff.df[9,1] <- "1st Person Plural"; coeff.df[9,2] <- coefficients(we.unadj)[2]; coeff.df[9,3] <- coefficients(we.unadj)[4]; coeff.df[9,4] <- coefficients(we.unadj)[8]
coeff.df[9,5] <- coefficients(we.adj)[2]; coeff.df[9,6] <- coefficients(we.adj)[5]; coeff.df[9,7] <- coefficients(we.adj)[11]

coeff.df[10,1] <- "2nd Person"; coeff.df[10,2] <- coefficients(you.unadj)[2]; coeff.df[10,3] <- coefficients(you.unadj)[4]; coeff.df[10,4] <- coefficients(you.unadj)[8]
coeff.df[10,5] <- coefficients(you.adj)[2]; coeff.df[10,6] <- coefficients(you.adj)[5]; coeff.df[10,7] <- coefficients(you.adj)[11]

#Output coeff.df dataframe to text file
write.table(coeff.df, file = "Data/Results/all_tweets/TableS2.txt", sep = ";", quote = FALSE, row.names = F)










