#--------------------------------------------------
#Figure 2: Personalised network connectivity is associated with self-reported depression severity  
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

raincloud_theme2 = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


episode_depression = theme(
  panel.background = element_blank(),
  legend.title =  element_blank(),
  legend.text = element_text(size=46),
  legend.key=element_blank(),
  legend.justification = "right",
  plot.margin = unit(c(1,1,1,1), "cm"),
  text = element_text(size = 46, family = "Arial"),
  axis.title.x = element_text(size = 46,vjust=-2),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  plot.tag = element_text(size = 18,face = "bold"),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))



cbpalette <- c("#DD8D29","#E2D200","#46ACC8","#0B775E","#C51B7D","#B40F20","#8C510A",
               "#08306B","#7FBC41")
##################################################################################################

#load in strength centralities of participants with >= 30 days of TWeets
dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')
dc_net$Id <- dc_net[,1]
dc_net <- dc_net[,-1]
dc_net[,1:10] <- remove_all_outliers(dc_net[1:10])

###################################################################
###################################################################
my_datal <- melt(dc_net, id.vars = c("Id","Depression_zscore"), 
                 measure.vars = c("ngm", "psm", "i", "we","pr3","you","swr",
                                  "art","ngt"),
                 variable.name = "NetworkVariable", value.name = "NodeStrength")


my_datal <- my_datal %>% mutate(NetworkVariable=recode(NetworkVariable, 
                                                       "ngm"="negemo",
                                                       "psm"="posemo",
                                                       "pr3" = "pro3",
                                                       "swr" = "swear",
                                                       "art" = "article",
                                                       "ngt" = "negate"))


#--------------------------------------------------------------------------------------------------
#--------------------------------------------------------------------------------------------------

mod.ngm <- (glm(ngm ~ Depression_zscore,data = dc_net))
mod.psm <- (glm(psm ~ Depression_zscore,data = dc_net))
mod.i <- (glm(i ~ Depression_zscore,data = dc_net))
mod.we <- (glm(we ~ Depression_zscore,data = dc_net))
mod.pro3 <- (glm(pr3 ~ Depression_zscore,data = dc_net))
mod.you <- (glm(you ~ Depression_zscore,data = dc_net))
mod.swr <- (glm(swr~ Depression_zscore,data = dc_net))
mod.art <- (glm(art ~ Depression_zscore,data = dc_net))
mod.ngt <- (glm(ngt~ Depression_zscore,data = dc_net))

ints <- c(coefficients(summary(mod.ngm))[1],coefficients(summary(mod.psm))[1],coefficients(summary(mod.i))[1],
          coefficients(summary(mod.we))[1],coefficients(summary(mod.pro3))[1],coefficients(summary(mod.you))[1],
          coefficients(summary(mod.swr))[1],coefficients(summary(mod.art))[1],coefficients(summary(mod.ngt))[1])


betas <- c(coefficients(summary(mod.ngm))[2],coefficients(summary(mod.psm))[2],coefficients(summary(mod.i))[2],
           coefficients(summary(mod.we))[2],coefficients(summary(mod.pro3))[2],coefficients(summary(mod.you))[2],
           coefficients(summary(mod.swr))[2],coefficients(summary(mod.art))[2],coefficients(summary(mod.ngt))[2])

lwr.ci <- c(confint(mod.ngm,parm = "Depression_zscore")[1],confint(mod.psm,parm = "Depression_zscore")[1],
            confint(mod.i,parm = "Depression_zscore")[1],confint(mod.we,parm = "Depression_zscore")[1],
            confint(mod.pro3,parm = "Depression_zscore")[1],
            confint(mod.you,parm = "Depression_zscore")[1],confint(mod.swr,parm = "Depression_zscore")[1],
            confint(mod.art,parm = "Depression_zscore")[1],confint(mod.ngt,parm = "Depression_zscore")[1])

upr.ci <- c(confint(mod.ngm,parm = "Depression_zscore")[2],confint(mod.psm,parm = "Depression_zscore")[2],
            confint(mod.i,parm = "Depression_zscore")[2],confint(mod.we,parm = "Depression_zscore")[2],
            confint(mod.pro3,parm = "Depression_zscore")[2],
            confint(mod.you,parm = "Depression_zscore")[2],confint(mod.swr,parm = "Depression_zscore")[2],
            confint(mod.art,parm = "Depression_zscore")[2],confint(mod.ngt,parm = "Depression_zscore")[2])


zscore_ci = data.frame(matrix(vector(), 9, 4,
                               dimnames=list(c(), c("Beta","lwr.ci","upr.ci","Sentiment"))),
                        stringsAsFactors=F)

zscore_ci$Beta <- betas; zscore_ci$lwr.ci <- lwr.ci; zscore_ci$upr.ci <- upr.ci
zscore_ci$Sentiment <- c("Neg. Emo.", "Pos. Emo.", "1st Pers. Sing.",
                         "1st Pers. Pl.","3rd Pers.","2nd Pers.","Swear","Article","Negate")

zscore_ci$Sentiment <- factor(zscore_ci$Sentiment, levels = zscore_ci$Sentiment[order(zscore_ci$Sentiment,decreasing = T)])

zscore.plot <- ggplot(data = zscore_ci,aes(x=Sentiment, y=Beta)) + 
  geom_errorbar(aes(ymin=lwr.ci, ymax=upr.ci), width=.25,size=1.5,color = cbpalette) +  
  geom_point(color = cbpalette,size=3) +
  episode_depression  + geom_point(color = cbpalette) + xlab("LIWC Text Feature\n") + 
  ylab(expression("Regression Coefficient (95% CI)")) + coord_flip() +
  geom_hline(yintercept = 0, linetype="dotted", 
             color = "navyblue", size=1.5)  



g3 <- ggplot(dc_net,aes(y = Mean_Centrality,x=Depression_zscore)) + geom_point(alpha = 0.80,size=3) +  xlab("SDS Summed Score") + raincloud_theme2 + 
  geom_smooth(method = "lm",se=FALSE,size=3,color="navyblue") + xlab("Depression Symptom Severity (z-score)") + 
  ylab("Global Network Strength\n")+ episode_depression


dc_net %>%
  ggplot( aes(x=Mean_Centrality)) +
  geom_histogram(fill = "black", color="black", alpha=0.8, position = 'identity',bins = 15) +
  labs(fill="") + xlab("Beta") +  
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm"))


combined <- g3 + zscore.plot + plot_layout(widths = c(1.25, 1))

combined


png("Figures/Figure2.png", units="cm", width=75, height=30, res=600)
combined 
dev.off()

