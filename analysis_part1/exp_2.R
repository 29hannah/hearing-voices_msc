# LMER ANALYSIS FOR VOICE HEARING (EXP 2)
library(lme4)
library(emmeans)
library(sjPlot)
library(tidyverse)
library(ggpubr)


# Loading and preparing the data ------------------------------------------
data <- read.csv("/Users/hannahsmacbook/AVH/analysis/summarised_both-experiment.csv", header=T, na.strings = "NaN")

# Loading the questionnaire results
ques_dir<-"/Users/hannahsmacbook/PycharmProjects/AVH/analysis/data_experiment/"
ques_file<-"questionnaires/Questionnaire_data.csv"
questionnaire_data <- read.csv(paste0(ques_dir,ques_file), header=T, na.strings = "NaN",  sep = ';')
names(questionnaire_data)[names(questionnaire_data) == 'Id'] <- 'participant'

# Merging questionnaire data to experiment data
data <- merge(data, questionnaire_data, by = "participant") 

# Define Factors
data$participant <- as.factor(data$participant)
data$target_spkr <- as.factor(data$target_spkr)
data$distractor <- as.factor(data$distractor)
data$target_digit <- as.factor(data$target_digit)

# Define numeric values
data$CAPS_short <- as.numeric(data$CAPS_short)
data$LSHS_short <- as.numeric(data$LSHS_short)
data$CAPS_auditory_short <- as.numeric(data$CAPS_auditory_short)
data$SPQ <- as.numeric(data$SPQ)

# Scale questionnaire scores
data$CAPS_Z_short = scale(data$CAPS_short)
data$LSHS_Z = scale(data$LSHS_short)
data$SPQ_Z = scale(data$SPQ)
data$SNR_Z = scale(data$SNR)
data$CAPS_auditory_short_z <- scale(data$CAPS_auditory_short)

# Add a new column with the loudspeaker positions
data<-data %>%
  mutate(pos = case_when(
    target_spkr == 3 ~ "Left",
    target_spkr == 15 ~ "Front",
    target_spkr == 27 ~ "Right",
    target_spkr == 68 ~ "Back",
    TRUE ~ "All")
  )

# Remove the values for the unmasked condition
data<- data %>% filter(distractor != "unmasked")

# Rename the Distractor Column 
data<-data %>%
  mutate(Distractor = case_when(
    distractor == "noise_distractor" ~ "Noise",
    distractor == "voice_distractor" ~ "Ambiguous",
    TRUE ~ "All")
  )
data$Distractor <- as.factor(data$Distractor)
Distractor = matrix(c(1/2, -1/2))

# Exclude first participant and exclude participant 7 as well, somehow mixed up data
data<-filter(data, participant!= "1_2_current" )
data<-filter(data, participant!= "jfkd6u" )

# Exclude nan values in PSE from data set to ensure that all models fitted
# on same size data sets
data_sub_PSE <- data %>%filter(!is.na(PSE))
data_sub <- data_sub_PSE %>%filter(!is.na(CAPS_Z_short))

# Exclude values when PSE outside the tested morph ratios
data_sub<-filter(data_sub, PSE >= 0.0 & PSE <= 1.0)

# Check the data ----------------------------------------------------------
length(unique(data_sub$participant))
nrow(data)

# Set plotting parameters--------------------------------------------------------
theme_set(theme_bw())
theme_update(text = element_text(size=20, family="Times New Roman"))

plot_dir<-"/Users/hannahsmacbook/Desktop/Master's Thesis/Figures M.Sc. Thesis/Results/"

# Mixed model predicting performance --------------------------------------
full_model = glmer(corr1 ~ CAPS_Z_short * Distractor + PSE*Distractor + pos + 
                     SNR_Z+ (1|participant), data=data_sub, family = binomial)


# Check the residuals of the model 
# Deviance residuals
residuals_deviance <- residuals(full_model, type = "deviance")

# Response residuals (optional)
residuals_response <- residuals(full_model, type = "response")

plot(fitted(full_model), residuals_deviance,
     xlab = "Fitted Values",
     ylab = "Deviance Residuals",
     main = "Residuals vs. Fitted Values")
abline(h = 0, col = "red")  # Add a horizontal line at 0


# Get the model results
tab_model(full_model)
summary(full_model)


# Plotting PSE and performance
PSE_performance<-plot_model(full_model, type = "pred", terms = c("PSE", "Distractor")) +
  labs(x = "PSE", y = "Porobability of Response Accuracy", title = NULL)
PSE_performance

# Plotting hallucination-proneness and performance
CAPS_performance<-plot_model(full_model, type = "pred", terms = c("CAPS_Z_short", "Distractor")) +
  labs(x = "z-scored CAPS score (short)", y = "Probability of Response Accuracy", title = NULL)
CAPS_performance

# Main results figure --------------------------------------
ggarrange(PSE_performance+
            scale_y_continuous(limits = c(0.45, 1),labels = scales::percent), 
          CAPS_performance+rremove("ylab")+
            scale_y_continuous(limits = c(0.45, 1),labels = scales::percent), 
          labels = c("A", "B"),ncol=2, nrow=1, common.legend = TRUE, 
          legend = "right")

ggsave(paste0(plot_dir,"cocktail_party_PSE-and-CAPS.png"),dpi=600)


###CONTROL CONDITIONS###
#  SNR --------------------------------------
plot_model(full_model, type = "pred", terms = c("SNR_Z", "Distractor")) +
  labs(x = "SNR_Z", y = "Probability of Response Accuracy", title = NULL)

# Loudspeaker position --------------------------------------
baseline = glmer(corr1 ~ CAPS_Z_short * Distractor + PSE*Distractor  + 
                    SNR_Z+ (1|participant), data=data_sub, family = binomial)
anova(baseline, full_model)

#Plotting loudspeaker differences
plot_model(full_model, type = "pred", terms = c("pos")) +
  labs(x = "Loudspeaker Position", y = "Probability of Response Accuracy", title = NULL)

# Post-hoc test
emmeans(full_model, pairwise ~pos)

# Distractor type --------------------------------------
plot_model(full_model, type = "pred", terms = c("Distractor")) +
  labs(x = "Distractor", y = "Probability of Response Accuracy", title = NULL)
emmeans(full_model, pairwise ~Distractor)


# Models with other questionnaires ----------------------------------------
full_model_LSHS = glmer(corr1 ~ LSHS_Z * Distractor + PSE*Distractor + pos + 
                     SNR_Z+ (1|participant), data=data, family = binomial)
tab_model(full_model_LSHS)
LSHS_performance<-plot_model(full_model_LSHS, type = "pred", terms = c("LSHS_Z", "Distractor")) +
  labs(x = "z-scored LSHS score", y = "Probability of Response Accuracy", title = NULL)


full_model_SPQ_Z = glmer(corr1 ~ SPQ_Z * Distractor + PSE*Distractor + pos + 
                          SNR_Z+ (1|participant), data=data, family = binomial)
tab_model(full_model_SPQ_Z)
SPQ_performance<-plot_model(full_model_SPQ_Z, type = "pred", terms = c("SPQ_Z", "Distractor")) +
  labs(x = "z-scored SPQ score", y = "Probability of Response Accuracy", title = NULL)

ggarrange(LSHS_performance+
            scale_y_continuous(limits = c(0.45, 1),labels = scales::percent), 
          SPQ_performance+rremove("ylab")+
            scale_y_continuous(limits = c(0.45, 1),labels = scales::percent), 
          labels = c("A", "B"),ncol=2, nrow=1, common.legend = TRUE, 
          legend = "right")


ggsave(paste0(plot_dir,"cocktail_party_LSHS-and_SPQ.png"),dpi=600)

# Models with CAPS auditory subscale ----------------------------------------
full_model_auditory_short = glmer(corr1 ~ CAPS_auditory_short_z * Distractor + PSE*Distractor + pos + 
                          SNR_Z+ (1|participant), data=data, family = binomial)
tab_model(full_model_auditory_short)
subscale_performance<-plot_model(full_model_auditory_short, type = "pred", terms = c("CAPS_auditory_short_z", "Distractor")) +
  labs(x = "z-scored CAPS score on Auditory Subscale", y = "Probability of Response Accuracy", title = NULL)
subscale_performance

