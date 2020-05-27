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



setwd('D:/Twitter_Depression_Kelley/')

"%||%" <- function(a, b) {
  if (!is.null(a)) a else b
}
geom_flat_violin <- function(mapping = NULL, data = NULL, stat = "ydensity",
                             position = "dodge", trim = TRUE, scale = "area",
                             show.legend = NA, inherit.aes = TRUE, ...) {
  layer(
    data = data,
    mapping = mapping,
    stat = stat,
    geom = GeomFlatViolin,
    position = position,
    show.legend = show.legend,
    inherit.aes = inherit.aes,
    params = list(
      trim = trim,
      scale = scale,
      ...
    )
  )
}

GeomFlatViolin <-
  ggproto("GeomFlatViolin", Geom,
          setup_data = function(data, params) {
            data$width <- data$width %||%
              params$width %||% (resolution(data$x, FALSE) * 0.9)
            
            # ymin, ymax, xmin, and xmax define the bounding rectangle for each group
            data %>%
              group_by(group) %>%
              mutate(ymin = min(y),
                     ymax = max(y),
                     xmin = x,
                     xmax = x + width / 2)
            
          },
          
          draw_group = function(data, panel_scales, coord) {
            # Find the points for the line to go all the way around
            data <- transform(data, xminv = x,
                              xmaxv = x + violinwidth * (xmax - x))
            
            # Make sure it's sorted properly to draw the outline
            newdata <- rbind(plyr::arrange(transform(data, x = xminv), y),
                             plyr::arrange(transform(data, x = xmaxv), -y))
            
            # Close the polygon: set first and last point the same
            # Needed for coord_polar and such
            newdata <- rbind(newdata, newdata[1,])
            
            ggplot2:::ggname("geom_flat_violin", GeomPolygon$draw_panel(newdata, panel_scales, coord))
          },
          
          draw_key = draw_key_polygon,
          
          default_aes = aes(weight = 1, colour = "grey20", fill = "white", size = 0.5,
                            alpha = NA, linetype = "solid"),
          
          required_aes = c("x", "y")
  )
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

raincloud_theme = theme(
  panel.background = element_blank(),
  text = element_text(size = 10),
  axis.title.x = element_blank(),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  legend.title=element_text(size=16),
  legend.text=element_text(size=16),
  legend.position = "right",
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

raincloud_theme2 = theme(
  panel.background = element_blank(),
  legend.position = "none",
  text = element_text(size = 10),
  axis.title.x = element_text(size = 16),
  axis.title.y = element_text(size = 16),
  axis.text = element_text(size = 14),
  axis.text.x = element_text(angle = 45, vjust = 0.5,colour = "black"),
  axis.text.y = element_text(colour = "black"),
  plot.title = element_text(lineheight=.8, face="bold", size = 16),
  panel.border = element_blank(),
  panel.grid.minor = element_blank(),
  panel.grid.major = element_blank(),
  axis.line.x = element_line(colour = 'black', size=0.5, linetype='solid'),
  axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))
##################################################################################################
#Clickworker and FYP data only 
dc_net <- read.csv('Data/Results/all_tweets/Node.Strength_dechoudhury_criticaltransition_nonchrono_ct30_15d_g10.csv')

dc_net <- dc_net[,-1]
dc_net <- dc_net %>% select(colnames(dc_net)[1:10],Mean_Strength,Period,Id)
dc_net[,1:11] <- remove_all_outliers(dc_net[1:11])
dc_net$Period <- ifelse(dc_net$Period == "close",1,0)

summary(lmer(Mean_Strength ~ Period + (1|Id),data = dc_net))
summary(lmer(negemo ~ Period+ (1|Id),data = dc_net))
summary(lmer(posemo ~ Period+ (1|Id),data = dc_net))
summary(lmer(i ~ Period+ (1|Id),data = dc_net))
summary(lmer(we ~ Period+ (1|Id),data = dc_net))
summary(lmer(shehe ~ Period+ (1|Id),data = dc_net))
summary(lmer(they ~ Period+ (1|Id),data = dc_net))
summary(lmer(you ~ Period+ (1|Id),data = dc_net))
summary(lmer(swear ~ Period+ (1|Id),data = dc_net))
summary(lmer(article ~ Period+ (1|Id),data = dc_net))
summary(lmer(negate ~ Period+ (1|Id),data = dc_net))

my_datal <- melt(dc_net, id.vars = c("Id","Period"), 
                 measure.vars = c("negemo", "posemo", "i", "we","shehe","they","you","swear",
                                  "article","negate"),
                 variable.name = "NetworkVariable", value.name = "NodeStrength")



##################################################################################################
#################################################################################################


g1 <- ggplot(data = dc_net, aes(y = Mean_Strength, x = as.factor(Period), fill = as.factor(Period))) +
  geom_flat_violin(position = position_nudge(x = .2, y = 0), alpha = .8) +
  geom_point(aes(y = Mean_Strength, color = as.factor(Period)), position = position_jitter(width = .15), size = 1.25, alpha = 0.8) +
  geom_boxplot(width = .2, guides = FALSE, outlier.shape = NA, alpha = 0.5) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  theme_bw() +
  raincloud_theme + xlab("Depressed Period in Past Year") + ylab("Global Network Strength\n") +
  scale_x_discrete(labels=c("0" = "Farthest Period", "1" = "Closest Period"))


g2 <- ggplot(data = my_datal, aes(y = NodeStrength, x = NetworkVariable, fill = as.factor(Period))) +
  geom_point(aes(y = NodeStrength, color = as.factor(Period)), position = position_jitterdodge(jitter.width = 0.5,dodge.width = 0.75), size = 1, alpha = .7) +
  geom_boxplot(width = .5, guides = FALSE, outlier.shape = NA, alpha = 0.5,position=position_dodge(width=0.75)) +
  expand_limits(x = 1) +
  guides(fill = FALSE) +
  guides(color = FALSE) +
  scale_color_brewer(palette = "Paired") +
  scale_fill_brewer(palette = "Paired") +
  # coord_flip() +
  theme_bw() +
  raincloud_theme2 + xlab("\nNetwork Sentiment") + ylab("Individual Node Strength\n")


combined <- (g2 + g1)


combined