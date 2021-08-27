df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/DevCoG_FC_3atlases_modified_for_R.csv')

df = subset(df, !is.na(RSN1))

data_count = as.data.frame(table(df$URSI))
data_count = subset(data_count, Freq > 2)

df = df[is.element(df$URSI, data_count$Var1),]

# keep_ursi = c()
# 
# for (item in unique(df$URSI)){
# 
#   mini_tab = subset(df, URSI == item)
# 
#   if (subset(mini_tab, Year_Index == '1')$RSN7 > subset(mini_tab, Year_Index == '2')$RSN7 &
#       subset(mini_tab, Year_Index == '2')$RSN 7 > subset(mini_tab, Year_Index == '3')$RSN7){
# 
#     keep_ursi = c(keep_ursi, item)
# 
#   }
# 
# }
# 
# df = df[is.element(df$URSI, keep_ursi), ]

######

# Demographics

demo_df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/DevCoG_FC_3atlases_FINAL.csv')

demo_df = demo_df[is.element(demo_df$URSI, data_count$Var1), ]

merged_df = merge(df, demo_df, by='URSI')

######

# https://drsimonj.svbtle.com/plotting-individual-observations-and-group-means-with-ggplot2
# https://johnmuschelli.com/intro_to_r/Data_Visualization/Data_Visualization.pdf

library(dplyr)

merged_df = merged_df %>% 
  mutate(Anxiety_Group = if_else(Y1_TSCC_ANXIETY > 5, 'High', 'Low'))

group_df <- merged_df %>%
  group_by(Anxiety_Group, Year_Index, SexN.x) %>%
  summarise(RSN1 = mean(RSN1))

# qplot(x=Year_Index, y=RSN1, colour = factor(URSI), facets = ~ Anxiety_Group, data=merged_df, geom='line') + 
#   theme(legend.position = 'none') + geom_line(data = group_df, alpha = .8, size = 3)

ggplot(merged_df, aes(x=Year_Index, y=RSN1, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)

# BY AGE BIN

group_df <- merged_df %>%
  group_by(Anxiety_Group, Age_bin) %>%
  summarise(RSN1 = mean(RSN1))

tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn1.jpg', sep=''), units='in', width=5, height=5, res=300)
ggplot(merged_df, aes(x=Age_bin, y=RSN1, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
dev.off()

# BY AGE BIN GROUP

group_df <- merged_df %>%
  group_by(Anxiety_Group, Age_bin_group) %>%
  summarise(RSN1 = mean(RSN1))

ggplot(merged_df, aes(x=Age_bin, y=RSN1, color=Age_bin_group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)

#### Loop Figures

for (subscore in c('Y1_TSCC_ANXIETY', 'Y1_TSCC_DEP', 'Y1_TSCC_PTS')){
  
  if (subscore == 'Y1_TSCC_ANXIETY'){
    threshold = 5
    group_label = 'anxiety'
  }
  
  if (subscore == 'Y1_TSCC_DEP'){
    threshold = 3
    group_label = 'depression'
  }
  
  merged_df = merged_df %>% 
    mutate(Anxiety_Group = if_else(merged_df[[subscore]] > threshold, 'High', 'Low'))
  
  #RSN1
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN1 = mean(RSN1))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn1.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN1, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN1 = mean(RSN1))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn1.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN1, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN1 = mean(RSN1))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn1.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN1, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  #RSN2
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN2 = mean(RSN2))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn2.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN2, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN2 = mean(RSN2))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn2.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN2, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN2 = mean(RSN2))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn2.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN2, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  #RSN3
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN3 = mean(RSN3))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn3.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN3, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN3 = mean(RSN3))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn3.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN3, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN3 = mean(RSN3))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn3.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN3, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  #RSN4
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN4 = mean(RSN4))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn4.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN4, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN4 = mean(RSN4))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn4.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN4, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN4 = mean(RSN4))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn4.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN4, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  #RSN5
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN5 = mean(RSN5))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn5.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN5, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN5 = mean(RSN5))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn5.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN5, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN5 = mean(RSN5))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn5.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN5, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  #RSN6
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN6 = mean(RSN6))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn6.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN6, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN6 = mean(RSN6))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn6.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN6, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN6 = mean(RSN6))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn6.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN6, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  #RSN7
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Year_Index) %>%
    summarise(RSN7 = mean(RSN7))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_rsn7.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Year_Index, y=RSN7, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin) %>%
    summarise(RSN7 = mean(RSN7))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label, '_agebin_rsn7.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN7, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3)
  dev.off()
  
  group_df <- merged_df %>%
    group_by(Anxiety_Group, Age_bin, SexN.x) %>%
    summarise(RSN7 = mean(RSN7))
  
  tiff(paste('F:/DevCoG_DevMIND_RS_fMRI/Results/', group_label,'_sex_rsn7.jpg', sep=''), units='in', width=5, height=5, res=300)
  ggplot(merged_df, aes(x=Age_bin, y=RSN7, color=Anxiety_Group)) + geom_line(aes(group=URSI)) + geom_line(data = group_df, alpha=.8, size=3) + facet_wrap(~SexN.x)
  dev.off()
  
  # qplot(x=Year_Index, y=RSN1, colour = factor(URSI), facets = ~ Anxiety_Group, data=merged_df, geom='line') + 
  #   theme(legend.position = 'none') + geom_line(data = group_df, alpha = .8, size = 3)
  
}



##############################################################################
#### Latent Variable Modeling / Growth Curve Modeling / Mixed Effects Modeling

library(lavaan)
library(tidySEM)
library(ggplot2)
library(dplyr)
library(semPlot)
library(statmod)

# rsfc.model <- 'intercept =~ 1*Y1_TSCC_ANXIETY + 1*Y1_TSCC_DEP + 1*Y1_TSCC_PTS
# slope =~ 0*Y1_TSCC_ANXIETY + 1*Y1_TSCC_DEP + 2*Y1_TSCC_PTS
# '
# fit <- growth(rsfc.model, data=merged_df)
# summary(fit, fit.measures=TRUE)
# 
# # Should be w/i network functional connectivity 
# # Need to reorganize data
# 
df = read.csv('F:/DevCoG_DevMIND_RS_fMRI/DevCoG_FC_3atlases_FINAL.csv')

# Create interaction columns for sex and TSCC
df$tscc_anx_sex <- df$SexN * df$Y1_TSCC_ANXIETY
df$tscc_dep_sex <- df$SexN * df$Y1_TSCC_DEP
df$tscc_pts_sex <- df$SexN * df$Y1_TSCC_PTS

# Define model and parameters

complex_model <- '
slope =~ 0*Y1_WNFC_RSN5 + 1*Y2_WNFC_RSN5 + 2*Y3_WNFC_RSN5
intercept =~ 1*Y1_WNFC_RSN5 + 1*Y2_WNFC_RSN5 + 1*Y3_WNFC_RSN5
# slope + intercept ~ Y1_TSCC_ANXIETY + Y1_TSCC_DEP + Y1_TSCC_PTS + tscc_anx_sex + tscc_dep_sex + tscc_pts_sex + AgeY1 + SexN
# Y1_TSCC_ANXIETY + Y1_TSCC_DEP + Y1_TSCC_PTS + tscc_anx_sex + tscc_dep_sex + tscc_pts_sex ~ AgeY1 + SexN
# slope + intercept ~ Y1_TSCC_ANXIETY + Y1_TSCC_DEP + Y1_TSCC_PTS + tscc_anx_sex + tscc_dep_sex + tscc_pts_sex
'


modelfit = growth(complex_model, data=df)
summary(modelfit)
# fitMeasures(modelfit)

# semPaths(modelfit, 'std', edge.label.cex = .5)
semPaths(modelfit, what='std', edge.label.cex = .8, label.cex = 3, fade=FALSE,
         rotation=1, intercepts=FALSE, layout='tree')
# graph_sem(model = modelfit)

# Examine covariance matrix 
inspectSampleCov(modelfit, data=df)

# Examine correlation matrix of model 
lavInspect(modelfit, 'cor.all')
















