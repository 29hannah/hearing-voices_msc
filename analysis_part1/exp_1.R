library(ggplot2)
library(ggpubr)
library(tidyverse)
library(rstatix)
library(lme4)
library(sjPlot)
library(ggExtra)
library(emmeans)
library(gridExtra)
library(ggpubr)
library(grid)

# Loading the data ------------------------------------------------------
data_dir<-"/Users/hannahsmacbook/AVH/Analysis/"
data_name<-"summarised-data_pf_all-participants.csv"
data <- read.csv(paste0(data_dir, data_name), header=T, na.strings = "NaN")

# Remove questionnaire columns
drops <- c("X","Unnamed..0", "CAPS_short", "CAPS_long", "LSHS", "SPQ")
data<-data[ , !(names(data) %in% drops)]

# Loading the questionnaire results
ques_dir<-"/Users/hannahsmacbook/PycharmProjects/AVH/analysis/data_experiment/"
ques_file<-"questionnaires/Questionnaire_data.csv"
questionnaire_data <- read.csv(paste0(ques_dir,ques_file), header=T, na.strings = "NaN",  sep = ';')
names(questionnaire_data)[names(questionnaire_data) == 'Id'] <- 'subj'

# Merging questionnaire data to experiment data
data <- merge(data, questionnaire_data, by = "subj") 

# Preparing the data ------------------------------------------------------
# Define factors
data$subj <- as.factor(data$subj)
data$spkr <- as.factor(data$spkr)
data$continuum <- as.factor(data$continuum)

#Define numeric columns
data$CAPS_long <- as.numeric(data$CAPS_long) 
data$LSHS_short <- as.numeric(data$LSHS_short)
data$SPQ <- as.numeric(data$SPQ)
data$CAPS_auditory_short = as.numeric(data$CAPS_auditory_short)

# Add Loudspeaker Position 
data<-data %>%
  mutate(pos = case_when(
    spkr == 3 ~ "Left",
    spkr == 15 ~ "Front",
    spkr == 27 ~ "Right",
    spkr == 68 ~ "Back",
    TRUE ~ "All")
  )

# Rename continua 
data<-data %>%
  mutate(continuum = case_when(
    continuum == "babble" ~ "Babble",
    continuum == "whisper_2" ~ "Whisper",
    TRUE ~ "All")
  )

# Scale/center the data
data$CAPS_short_Z = scale(data$CAPS_short)
data$CAPS_long_Z = scale(data$CAPS_long)
data$LSHS_Z = scale(data$LSHS_short)
data$SPQ_Z = scale(data$SPQ)

# Exclude PSE when outside of measurements
data<-filter(data, PSE >= 0.0 & PSE <= 1.0)

# Exclude participant 1
data<-filter(data, subj!= "1_2_current")

# Replace empty cells with NA value
data[data==""]<-NA
data[data=="n/a"]<-NA
data[data=="NaN"]<-NA

# Delete unnecessary variables 
rm(data_dir, 
   data_name,
   drops,
   ques_dir,
   ques_file,
   questionnaire_data)


# Subset the data ---------------------------------------------------------
# Only condition specific PSE values
data_con <- data[ which(data$continuum!='All'& data$spkr != 'all'), ]

# PSE values over all conditions
data_all<- data[ which(data$continuum=='All'& data$spkr == 'all'), ]

# Exploring the data set --------------------------------------------------
# Normal distribution of variables
data_all%>%shapiro_test(CAPS_short)
ggqqplot(data_all, "CAPS_short", ggtheme = theme_bw())
# CAPS short scores are normally distributed

data_all  %>%shapiro_test(CAPS_long)
ggqqplot(data_all, "CAPS_long", ggtheme = theme_bw())
# CAPS longs scores are normally distributed

data_all  %>%shapiro_test(SPQ)
ggqqplot(data_all, "SPQ", ggtheme = theme_bw())
# SPQ scores are normally distributed

data_all  %>%shapiro_test(LSHS_short)
ggqqplot(data_all, "LSHS_short", ggtheme = theme_bw())
# LSHS scores are not normally distributed

data_all%>%shapiro_test(PSE)
ggqqplot(data_all, "PSE", ggtheme = theme_bw())



# Set plotting parameters--------------------------------------------------------
theme_set(theme_bw())
theme_update(text = element_text(size=20, family="Times New Roman"))

plot_dir<-"/Users/hannahsmacbook/Desktop/Master's Thesis/Figures M.Sc. Thesis/Results/"

# Questionnaire score distribution --------------------------------------------------------

# Currently data include 41 participants, 4 removed due to missing values for 
# psychometric function

# CAPS short 
his_caps_short<-ggplot(data_all, aes(x=CAPS_short))+
    geom_histogram(binwidth=1, fill= "#D98374",color="black" ) +
      ylab("Counts")+ xlab("CAPS score (short)")+ xlim(-1,33) +ylim(0,7)
his_caps_short

# CAPS long 
his_caps_long<-ggplot(data_all, aes(x=CAPS_long)) + 
  geom_histogram(binwidth=1, fill= "#D98374",color="black", lwd=0.25 )+
  ylab("Counts")+ xlab("CAPS score (long)") +  xlim(-1,200)+ ylim(0,7)
his_caps_long


# LSHS
his_lshs<-ggplot(data_all, aes(x=LSHS_short)) + 
  geom_histogram(binwidth=1, fill= "#D3E7F4",color="black" )+
  ylab("Counts")+ xlab("LSHS score")+ xlim(-0.5,64) +ylim(0,5)
his_lshs

# SPQ
his_spq<-ggplot(data_all, aes(x=SPQ)) + 
  geom_histogram(binwidth=1, fill= "#EBEBEB",color="black" )+
  ylab("Counts")+ xlab("SPQ score")+ xlim(31.5,160) +ylim(0,7)
his_spq


distribution_questionnaires<-ggarrange(his_caps_short, his_caps_long+ rremove("ylab"),
          his_lshs, his_spq+ rremove("ylab"),
          labels = c("A", "B", "C", "D"),ncol=2, nrow=2)

distribution_questionnaires

ggsave(paste0(plot_dir,"distribution_questionnaires.png"), width = 10, 
       height = 10, dpi=600)

rm(his_caps_short, his_caps_long, his_lshs, his_spq)

# Correlation of questionnaires --------------------------------------------------------
# In the current state of analysing the questionnaire data, there are 6 possible
# Correlations to be tested

# CAPS short and LSHS
CAPS_short_LSHS<-ggplot(data_all, aes(x = CAPS_short, y = LSHS_short)) + 
  geom_point() +
  xlab("CAPS score (short)")+ ylab("LSHS score")+
  geom_smooth(method = "lm", col = "black")
CAPS_short_LSHS

# CAPS short and SPQ
CAPS_short_SPQ<-ggplot(data_all, aes(x = CAPS_short, y = SPQ)) + 
  geom_point() + 
  xlab("CAPS score (short)")+ ylab("SPQ score")+
  geom_smooth(method = "lm", col = "black")
CAPS_short_SPQ

# CAPS long and LSHS
CAPS_long_LSHS<-ggplot(data_all, aes(x = CAPS_long, y = LSHS_short)) + 
  xlab("CAPS score (long)")+ ylab("LSHS score")+
  geom_point() +
  geom_smooth(method = "lm", col = "black")
CAPS_long_LSHS


# CAPS long and SPQ
CAPS_long_SPQ<-ggplot(data_all, aes(x = CAPS_long, y = SPQ)) + 
  xlab("CAPS score (long)")+ ylab("SPQ score")+
  geom_point() +
  geom_smooth(method = "lm", col = "black")
CAPS_long_SPQ

# CAPS short and CAPS long
CAPS_short_long<-ggplot(data_all, aes(x = CAPS_short, y = CAPS_long)) +
  xlab("CAPS score (short)")+ ylab("CAPS score (long)")+
  geom_point() +
  geom_smooth(method = "lm", col = "black")
CAPS_short_long


# LSHS and SPQ
LSHS_SPQ<-ggplot(data_all, aes(x = LSHS_short, y = SPQ)) + 
  geom_point() +
  xlab("LSHS score")+ ylab("SPQ score")+
  geom_smooth(method = "lm", col = "black")
LSHS_SPQ


# Overall figure
correlation_questionnaires<-ggarrange(CAPS_short_SPQ, CAPS_short_LSHS,LSHS_SPQ,
                                      CAPS_long_SPQ,CAPS_long_LSHS,CAPS_short_long,
                    labels = c("A", "B", "C", "D", "E", "F"), ncol=3, nrow=2)
correlation_questionnaires

ggsave(paste0(plot_dir,"correlation_questionnaires.png"),dpi=600)

rm(CAPS_short_SPQ, CAPS_short_LSHS, CAPS_long_SPQ, CAPS_long_LSHS,
   LSHS_SPQ,CAPS_short_long)


# Statistical analysis of the correlation
# Data is not normally distributed
# Data is not independent 
cor.test(x=data_all$CAPS_short,y=data_all$SPQ,method="pearson")
cor.test(x=data_all$CAPS_short,y=data_all$LSHS_short,method="pearson")
cor.test(x=data_all$CAPS_long,y=data_all$SPQ,method="pearson")
cor.test(x=data_all$CAPS_long,y=data_all$LSHS_short,method="pearson")
cor.test(x=data_all$LSHS_short,y=data_all$SPQ,method="pearson")
cor.test(x=data_all$CAPS_short,y=data_all$CAPS_long,method="pearson")


# Analysis PSE and CAPS short  ---------------------------------------------------
# CAPS short and PSE over all conditions
CAPS_short_PSE_all<-ggplot(data_all, aes(x = CAPS_short, y = PSE)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black", lwd=1)+  
  ylim(c(0.05, 1.05))+ 
  xlab("CAPS score (short) ")
CAPS_short_PSE_all
ggsave(paste0(plot_dir,"CAPS_short_PSE_all.png"), width = 7, height=7, 
       dpi=600)


CAPS_short_PSE_all_marg<- ggMarginal(CAPS_short_PSE_all, type="histogram")
CAPS_short_PSE_all_marg
ggsave(paste0(plot_dir,"CAPS_short_PSE_all_marginal.png"), width = , height=10, 
       dpi=600)

cor.test(x=data_all$CAPS_short,y=data_all$PSE, method="pearson")


# CAPS short and PSE per condition
# Define conditions
conditions <- expand.grid(continuum = c("Whisper", "Babble"),
                          pos = c("Left", "Front", "Right", "Back"))
# Initialize lists to store models and plots
plots <- list()
cor_results <- list()
# Loop over each combination of continuum and position
for (i in 1:nrow(conditions)) {
  # Subset data
  sub_data <- data[which(data$continuum == conditions$continuum[i] &
                           data$pos == conditions$pos[i]), ]
  # Create the plot
  plot <- ggplot(sub_data, aes(x = CAPS_short, y = PSE)) + 
    geom_point() +
    geom_smooth(method = "lm", col = "black") +  
    scale_x_continuous(limits = c(-0.05, 23)) + 
    scale_y_continuous(limits = c(-0.05, 1.05)) 
  # Store the plot
  plots[[paste(conditions$continuum[i], conditions$pos[i], sep = "_")]] <- plot
  # Conduct Pearson correlation test
  cor_result <- cor.test(x = sub_data$CAPS_short, y = sub_data$PSE, method = "pearson")
  # Store the correlation result
  cor_results[[paste(conditions$continuum[i], conditions$pos[i], sep = "_")]] <- cor_result
}


# Create a figure to summarising the correlation over conditions
CAPS_short_PSE_cond<-ggarrange(plots[["Whisper_Left"]]+ rremove("xlab"),
                               plots[["Whisper_Right"]]+ rremove("ylab") + rremove("xlab"),
                               plots[["Whisper_Front"]]+ rremove("ylab")+ rremove("xlab"),
                               plots[["Whisper_Back"]]+ rremove("ylab")+ rremove("xlab"),
                               plots[["Babble_Left"]]+ rremove("xlab"),
                               plots[["Babble_Right"]]+ rremove("ylab")+ rremove("xlab"),
                               plots[["Babble_Front"]]+ rremove("ylab")+ rremove("xlab"),
                               plots[["Babble_Back"]]+ rremove("ylab")+ rremove("xlab"),
                               ncol=4, nrow=2, common.legend = TRUE, 
                              labels = c("A", "B", "C", "D", "E", "F", "G", "H"),
                                      widths = c(1,1, 1, 1, 1),
                                      align = "hv")

# Create a common x=label
x_label <- textGrob("CAPS score (short)", 
                    gp = gpar(fontfamily = "Times New Roman", fontsize = 16))

# Add the common x-axis label to the arranged plots
final_plot <- annotate_figure(CAPS_short_PSE_cond,bottom = x_label)
final_plot <- final_plot + theme(plot.margin = margin(t = 10, r = 0, b = 40, l =10 ))
final_plot

ggsave(paste0(plot_dir,"CAPS_short_PSE_cond.png"),width = 20, height = 10,
       dpi=600)

# Check the results of the correlation 
for (i in 1:nrow(conditions)) {
  print(paste(conditions$continuum[i], conditions$pos[i], sep = "_"))
  print(
    cor_results[[paste(conditions$continuum[i], conditions$pos[i], sep = "_")]]
    )
} 
rm(cor_result, cor_results, i, plots, x_label)



# Analysis PSE over all conditions and CAPS short sub-scales ---------------------------------------------------
# Analysis of CAPS short auditory sub-scales

CAPS_sub_PSE<-ggplot(data_all, aes(x = CAPS_auditory_short, y = PSE)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black")+
  ylim(c(0.05, 1.05))+ 
  xlab("CAPS score (short)")
CAPS_sub_PSE

ggsave(paste0(plot_dir,"CAPS_short_sub_PSE_all.png"), width = 7, height=7, 
       dpi=600)

cor.test(x=data_all$CAPS_auditory_short,y=data_all$PSE, method="pearson")


# Single CAPS items

# Do you ever hear voices commenting on what you are thinking or doing?
item_bxp11<-ggplot(data_all[!is.na(data_all$CAPS_AVH1),], aes(x=CAPS_AVH1,y=PSE))+
  geom_boxplot(outlier.shape = NA) +
  geom_point(position=position_jitter(width=0.1), alpha = 0.3)+ 
  scale_y_continuous(limits = c(-0.05, 1.05))+
  xlab("Item 11")
item_bxp11

# Do you ever hear voices saying words or sentences when there is no-one around[...]?
item_bxp13<-ggplot(data_all[!is.na(data_all$CAPS_AVH2),], aes(x=CAPS_AVH2,y=PSE))+
  geom_boxplot() +
  geom_point(position=position_jitter(width=0.1), alpha = 0.3)+ 
  scale_y_continuous(limits = c(-0.05, 1.05))+
  xlab("Item 13")
item_bxp13

# Have you ever heard 2 or more unexplained voices talking with each other?
item_bxp28<-ggplot(data_all[!is.na(data_all$CAPS_AVH3),],aes(x=CAPS_AVH3,y=PSE))+
  geom_boxplot() +
  geom_point(position=position_jitter(width=0.1), alpha = 0.3)+ 
  scale_y_continuous(limits = c(-0.05, 1.05))+ 
  xlab("Item 28")
item_bxp28


# Overall figure
single_items<-ggarrange(item_bxp11, item_bxp13+ rremove("ylab") ,
                        item_bxp28+ rremove("ylab") ,
                        labels = c( "A","B", "C"),
                        ncol=3, nrow=1,align = "hv")
single_items
ggsave(paste0(plot_dir,"single_items_CAPS_PSE.png"), width = 10, 
       height = 5, dpi=600)



# Get the mean PSE per response
mean_per_item11 <- data_all %>%
  group_by(CAPS_AVH1) %>%
  summarise(mean_value = mean(PSE))
mean_per_item11

# Statistics
# Only for item 11 because of more than 3 yes answers
# Mann Whitney U
wilcox.test(data_all$PSE ~ data_all$CAPS_AVH1, exact=TRUE)
group_counts <- data_all %>%
  count(CAPS_AVH1)
print(group_counts)

# Inter individual differences in the PSE: Analysis of differences between conditions ---------------------------------------------------
# Continuum and loudspeaker position
bxp_continuum_spkr<-ggplot(data_con, aes(y=PSE, x=pos, fill=continuum))+
  geom_boxplot() +
  geom_point(position=position_jitterdodge(jitter.width=0.25), alpha = 0.3)+ 
  ylim(c(0.05, 1.05))+ 
  xlab("Loudspeaker Position")+ 
  scale_fill_manual(values=c("#CEE8F5", "#E16351"))+
  guides(fill=guide_legend(title="Continuum"))

bxp_continuum_spkr
ggsave(paste0(plot_dir,"bxp_continuum_spkr.png"), width = 7, 
       height = 5, dpi=600)

 # Loudspeaker position
bxp_spkr <- ggplot(data_con, aes(x = pos, y = PSE)) +
        geom_boxplot(fill = "white", outlier.shape = NA) +
        geom_point(position = position_jitter(width = 0.2),  
        color = "black",  alpha = 0.3) + 
        ylim(c(0.05, 1.05)) + 
        xlab("Loudspeaker Position")

bxp_spkr
ggsave(paste0(plot_dir,"bxp_spkr.png"), width = 7, 
       height = 5, dpi=600)

# Continuum
bxp_continuum <- ggplot(data_con, aes(x = continuum, y = PSE)) +
  geom_boxplot(fill = "white", outlier.shape = NA) +
  geom_point(position = position_jitter(width = 0.2),  
             color = "black",  alpha = 0.3) + 
  ylim(c(0.05, 1.05)) + 
  xlab("Stimulus Continuum")

bxp_continuum
ggsave(paste0(plot_dir,"bxp_continuum.png"), width = 7, 
       height = 5, dpi=600)


# Create an overall figure
bxp_PSE_differences<-ggarrange( bxp_spkr, bxp_continuum,labels = c("A", "B"),
                                   ncol = 2, nrow = 1)

bxp_PSE_differences
ggsave(paste0(plot_dir,"bxp_spkr_continuum_sep.png"), width = 10, 
       height = 5, dpi=600)


# Statistical analysis: Mixed model 
# Define the model
full_model = lmer(PSE ~ continuum + pos + (1|subj), data=data_con)

# Check residuals 
plot(full_model)
qqnorm(residuals(full_model))  
qqline(residuals(full_model))  

# Get the model results 
summary(full_model)
tab_model(full_model)

# Plot the results of the model
plot_model(full_model, type = "pred", terms = c("pos","continuum"))
plot_model(full_model, type = "pred", terms = c("pos"))
plot_model(full_model, type = "pred", terms = c("continuum"))

# Removing continuum
baseline1 = lmer(PSE ~pos + (1|subj), data=data_con)
anova(baseline1, full_model)
emmeans(full_model, pairwise ~continuum)

# Removing loudspeaker position
baseline2 = lmer(PSE ~continuum + (1|subj), data=data_con)
anova(baseline2, full_model)
emmeans(full_model, pairwise ~pos)

# Include an interaction between position and continuum 
baseline3 = lmer(PSE ~continuum * pos + (1|subj), data=data_con)
anova(baseline3, full_model)
rm(baseline1, baseline2, baseline3, full_model)
# Analysis of PSE and other questionnaires  -----------------------------------------------
# CAPS long and PSE over all conditions
CAPS_long_PSE_all<-ggplot(data_all, aes(x = CAPS_long, y = PSE)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black", lwd=1)+  
  ylim(c(0.05, 1.05))+ 
  xlab("CAPS score (long)")
CAPS_long_PSE_all

# LSHS and PSE over all conditions
LSHS_PSE_all<-ggplot(data_all, aes(x = LSHS_short, y = PSE)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black", lwd=1)+  
  ylim(c(0.05, 1.05))+ 
  xlab("LSHS")
LSHS_PSE_all

# SPQ and PSE over all conditions
SPQ_PSE_all<-ggplot(data_all, aes(x = SPQ, y = PSE)) + 
  geom_point() +
  geom_smooth(method = "lm", col = "black", lwd=1)+  
  ylim(c(0.05, 1.05))+ 
  xlab("SPQ")
SPQ_PSE_all


CAPS_short_PSE_all
ggarrange(CAPS_short_PSE_all,CAPS_long_PSE_all+ rremove("ylab"), LSHS_PSE_all, SPQ_PSE_all+ rremove("ylab"),
          labels = c("A", "B", "C", "D"),ncol=2, nrow=2)
ggsave(paste0(plot_dir,"correlation_other-questionnaires_PSE.png"),dpi=600)

# Test for a correlation 
cor.test(x=data_all$CAPS_long,y=data_all$PSE, method="pearson")
cor.test(x=data_all$LSHS_short,y=data_all$PSE, method="pearson")
cor.test(x=data_all$SPQ,y=data_all$PSE, method="pearson")


