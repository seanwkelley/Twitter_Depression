#------------------------------------------------
#Figure 4: Random networks of 9 text features either significantly or not significantly associated with current depression 

#------------------------------------------------


#Load necessary packages 
library(ggplot2)
library(patchwork)
library(here)
library(dplyr)


TD_theme.nolegend = theme(
  panel.background = element_blank(),
  text = element_text(size = 46, family = "Arial"),
  legend.position = "none",
  plot.margin=unit(c(1,1,0,0),"cm"),
  axis.title.x = element_text(size = 46),
  axis.title.y = element_text(size = 46),
  axis.text = element_text(size = 46),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black",size = 46),
  axis.text.y = element_text(colour = "black",size = 46),
  plot.title = element_text(lineheight=.8, face="bold", size = 46),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))


setwd('/Users/seankelley/Twitter_Depression_Kelley/')

#100 random networks based on text features with/without a significant association with depression 
#for within-episode effect on global network connectivity 

model2_coeff_dep <- read.csv('Data/Results/all_tweets/model2_coeff_rand_dep_pro3_0510.csv',header = F)
model2_coeff_nodep <- read.csv('Data/Results/all_tweets/model2_coeff_rand_nodep_pro3_0510.csv',header = F)

colnames(model2_coeff_dep) = colnames(model2_coeff_nodep) <- c("variables","beta","se","pval")


length(which(abs(model2_coeff_dep$beta) > 0.02435613 ))/length(model2_coeff_dep$beta)
length(which(abs(model2_coeff_nodep$beta) > 0.01536438))/length(model2_coeff_nodep$beta)


model2_coeff_dep$network_type <- "depression";model2_coeff_nodep$network_type <- "no_depression"
model2_coeff <- as.data.frame(rbind(model2_coeff_dep,model2_coeff_nodep))


summary(glm(beta ~ network_type,data = model2_coeff))


m2_beta <- model2_coeff %>%
  ggplot( aes(x=beta, fill=network_type)) +
  geom_histogram( color="black", alpha=0.8, position = 'identity',bins = 15) +
  scale_fill_manual(values=c("grey54","grey20"),labels = c("Significant","Non-Significant")) +
  labs(fill="") + xlab("Beta") +  
  geom_vline(aes(xintercept = mean(model2_coeff_dep$beta)),color="grey54", linetype="dashed", size=4) +
  geom_vline(aes(xintercept = mean(model2_coeff_nodep$beta)),color="grey20", linetype="dashed", size=4) +
  geom_vline(aes(xintercept = 0.02435613),color="darkblue", linetype="dashed", size=4) + 
  theme(text = element_text(size=40),
        legend.position = "none",
        axis.text.x = element_text(colour = "black"),
        axis.text.y = element_text(colour = "black"),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"),
        plot.margin=unit(c(0,1,0,0),"cm")) +
  scale_x_continuous(breaks=c(-0.03, -0.02, -0.01, 0, 0.01,0.02,0.03))

m2_beta



png("Figures/Figure4A.png", units="cm", width=60, height=30, res=600)
m2_beta
dev.off()

#---------------------------------------------------------


tweet_type = "all_tweets"
path = paste0('Data/Sentiments/',tweet_type,"/VADER_ANEW_LIWC_complete_dep.csv",collapse = "")


#sentiment analysis results based on tweet type 
FYP_df <- read.csv(path,stringsAsFactors = FALSE)
colnames(FYP_df)[which(colnames(FYP_df) == 'Twitter_Handle')] = 'Id'
participants <- read.csv('Data/Participant_Data/Twitter_Participants.csv')
dc_ep <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_episodepastyear.csv')

colnames(dc_ep)[1] <- "Id"
FYP_df <- FYP_df %>% filter(Id %in% unique(dc_ep$Id))
FYP_df <- FYP_df[which(FYP_df$Date != ''),]
FYP_df$pro3 <- (FYP_df$shehe + FYP_df$they)/2

#############################################################
network_centrality <- list(); edge_num <- list()
length.days <- list(); network_stability <- list()

dep_array <- list()

for(id in unique(FYP_df$Id)){
  
  print(paste0(which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(i,Clout,ppron,function.,tentat,negemo,power,negate,achieve)
  
  SDS <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    dep_array[[id]] <- net1$PCC
    
  }
}

#---------------------------------------------------------------------------------------------------

strength_theme = theme(
  panel.background = element_blank(),
  legend.title = element_text(size = 20),
  legend.text = element_text(size=20),
  legend.key=element_blank(), 
  text = element_text(size = 20),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 20),
  axis.text.y = element_text(colour = "black"),
  axis.text = element_text(size = 20),
  axis.text.x = element_text(angle = 0, vjust = 0.5,colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

network_centrality <- list(); edge_num <- list()
length.days <- list(); network_stability <- list()

dep_array <- list()

for(id in unique(FYP_df$Id)){
  
  print(paste0(which(unique(FYP_df$Id) == id)/length(unique(FYP_df$Id))," ",id))
  
  en_var <- FYP_df %>% filter(Id == id) %>% select(i,Clout,ppron,function.,tentat,negemo,power,negate,achieve)
  
  SDS <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Depression_zscore))
  Dep_ep <- as.numeric(participants %>% dplyr::filter(Id == id) %>% dplyr::select(Dep_ep_pastyear))
  
  en_var <- as.matrix(en_var)
  
  #series length must be a minimum of 30 days and each node must have some variance 
  if(dim(en_var)[1] >= 30 & all(apply(en_var, 2, sd) != 0)){
    print(dim(en_var)[1])
    
    try(net1 <- graphicalVAR(en_var, nLambda = 10, verbose = TRUE, gamma = 0,scale = TRUE, maxit.in = 100,
                             maxit.out = 100),silent = TRUE)
    
    dep_array[[id]] <- net1$PCC
    
  }
}


dep_array_pcc <- array(as.numeric(unlist(dep_array)), dim=c(9, 9, 946))
dep_array_mean <- as.data.frame(apply(dep_array_pcc, c(1,2), mean))

#new names best network 
colnames(dep_array_mean) <- c("1st Pers. Sing.","Clout","Pers. Pro.","Function","Tentative","Neg. Emo.",
                              "Power","Negate","Achieve")

rownames(dep_array_mean) <- colnames(dep_array_mean)
dep_array_mean <- dep_array_mean %>%  dplyr::select(sort(current_vars())) %>% dplyr::arrange(rownames(dep_array_mean))


png("Figures/Figure4B.png",width = 8, height = 8, units = 'in', res = 600)

qgraph(dep_array_mean,fade=F,layout = "circle",
       color= "#ffffff",
       vTrans = 255,
       borders = TRUE,minimum = 0.008,labels = "",
       border.color = "#808080",
       border.width = 3,
       label.norm = "OOOOOO",label.cex = 2.5,
       label.fill.vertical = 1,label.fill.horizontal = 1,
       label.color = "black",label.font = 2,
       node.width = 1.25,edge.width = 5,esize = 5, lty = 1,font = 2)

dev.off()





model2_coeff_dep <- model2_coeff_dep[1:100,]
model2_coeff_nodep <- model2_coeff_nodep[1:100,]


top100 <- model2_coeff_dep %>% separate(variables, paste0(rep("V1_",9),seq("1",9)))
top100 <- as.matrix(top100)

top100_var <- as.data.frame(table(melt(top100[1:100,1:9])$value))
top100_var <- top100_var[order(top100_var$Freq,decreasing = T)[1:20],]

top100_var$Var1 <- c("Tentative","Time","Conjunction","Health","Achieve","Biological","Number",
                     "1st Pers. Pl.","Work","2nd Pers.","Relative","Dictionary","Pers. Pron.",
                     "Anger","Body","Negate","Pos. Emo.","Preposition","Verb","Cause")

png("Figures/Figure4C.png", units="cm", width=40, height=40, res=650)

ggplot(data = top100_var, aes(x = reorder(Var1, Freq/100), y = Freq/100)) +
  geom_bar(stat="identity" ,aes(y = Freq/100),
           size=1.2, position = "dodge2",width=0.8,color = "black",fill = "black") + ylab("\nProportion in Top 100\nDepression Relevant Networks") +    
  coord_flip() + xlab("LIWC Text Feature\n") + TD_theme.nolegend 

dev.off()


###########################################

