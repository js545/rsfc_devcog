# Potential descriptive plots to add in manuscript

# MAIN RESOURCE: https://hopstat.wordpress.com/2015/07/09/line-plots-of-longitudinal-summary-data-in-r-using-ggplot2-2/

# Plot per age bin

df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/Yeo7_descriptive_figures.csv')

df = subset(df, !is.na(Connectivity))

library(ggplot2)
library(plyr)

agg = ddply(df, .(Age_bin, Age),
            function(x){c(mean=mean(x$Connectivity),
                          sd = sd(x$Connectivity))}
            )

agg$lower = agg$mean + agg$sd
agg$upper = agg$mean - agg$sd

agg$Age_bin = as.factor(agg$Age_bin)
# agg$num_time = as.numeric(as.character(agg$Age))
agg$num_time[ is.na(agg$num_time) ] = -1
unique(agg$num_time)

gbase  = ggplot(agg, aes(y=mean, colour=Age_bin)) + 
  geom_errorbar(aes(ymin=lower, ymax=upper), width=.3)
gline = gbase + geom_line() 
print(gline + aes(x=Age))

# Plot per Yeo Atlas subdivision 



# Plot DEP and PTS by Adjusted Slope Estimate

library(ggplot2)

df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/MPlus_Outputs/v2/Y17_RSN17/age_sex_site_anx_dep_pts/individual_estimates_yeo17.csv')

tiff('F:/DevCoG_DevMIND_RS_fMRI/Results/yeo17_dep_slope.png', units='in', width=5, height=5, res=300)
plot(df$DEP, df$S, 
     main = "Dorsal DMN Yeo 17",
     xlab = "DEP",
     ylab = "Adjusted Slope of DMN Connectivity",
     ylim = c(-.125, .125))

abline(lm(df$S ~ df$DEP), col = "red")
dev.off()

tiff('F:/DevCoG_DevMIND_RS_fMRI/Results/yeo17_pts_slope.png', units='in', width=5, height=5, res=300)
plot(df$PTS, df$S, 
     main = "Dorsal DMN Yeo 17",
     xlab = "PTS",
     ylab = "Adjusted Slope of DMN Connectivity",
     ylim = c(-.125, .125))

abline(lm(df$S ~ df$PTS), col = "red")
dev.off()


df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/MPlus_Outputs/v2/Y7_RSN7/age_sex_site_anx_dep_pts/individual_estimates_yeo7.csv')

tiff('F:/DevCoG_DevMIND_RS_fMRI/Results/yeo7_dep_slope.png', units='in', width=5, height=5, res=300)
plot(df$DEP, df$S, 
     main = "DMN Yeo 7",
     xlab = "DEP",
     ylab = "Adjusted Slope of DMN Connectivity",
     ylim = c(-.125, .125))

abline(lm(df$S ~ df$DEP), col = "red")
dev.off()

tiff('F:/DevCoG_DevMIND_RS_fMRI/Results/yeo7_pts_slope.png', units='in', width=5, height=5, res=300)
plot(df$PTS, df$S, 
     main = "DMN Yeo 7",
     xlab = "PTS",
     ylab = "Adjusted Slope of DMN Connectivity",
     ylim = c(-.125, .125))

abline(lm(df$S ~ df$PTS), col = "red")
dev.off()


df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/MPlus_Outputs/v2/individual_estimates_Yeo7_Yeo17.csv')

theme(plot.title = element_text(hjust = .5))

tiff('F:/DevCoG_DevMIND_RS_fMRI/Results/yeo7_17_pts_slope.png', units='in', width=7, height=5, res=300)
df %>%
  ggplot(aes(x=PTS, y=S, color=Atlas)) +
  xlab('Posttraumatic Score')+
  ylab('Residual Connectivity')+
  geom_point(alpha=.5) +
  geom_smooth(method='lm', se=TRUE)+
  theme(plot.title = element_text(hjust = .5))+
  theme(legend.justification = 'top')+
  theme(axis.text = element_text(face='bold'))+
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour='black'))+
  geom_hline(yintercept=0, linetype='dashed', color='gray')
dev.off()














