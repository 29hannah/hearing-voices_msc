# Analysis of behavioural data (in combination with EEG data)
library(BayesFactor)
library(plotrix) 
library(lme4)
library(lmerTest)
library(emmeans)
library(afex)
library(car)
library(sjPlot)
library(brms)
library(plotly)
library(performance)
library(ggeffects)  
require(gridExtra)
library(ggpubr)

# Set parameters for plotting-------------------------------------------------------------
theme_set(theme_bw())
theme_update(text = element_text(size=20, family="Times New Roman"))
plot_dir<-"/Users/hannahsmacbook/Desktop/Master's Thesis/Figures M.Sc. Thesis/Results/"

# Read the behavioural data -------------------------------------------------------------
data_behav <- read.csv("/Users/hannahsmacbook/EEG_voice/behavioural_data.csv",
                       header=T, na.strings = "NaN")
data_behav$Morph.ratio <- as.factor(data_behav$Morph.ratio)

# Plot the behavioural data -------------------------------------------------------------
behav_bxp<-ggplot(data_behav, aes(x=Morph.ratio,y=X.Voice)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(width = 0.1, size = 2, color = "grey", alpha = 0.6)+
  scale_y_continuous(limits = c(-0.05, 1.05))+
  labs(x = "Morph ratio",
       y = "% Voice responses")
behav_bxp

summary_stats <- data_behav %>%
  group_by(Morph.ratio) %>%
  summarise(
    mean= mean(X.Voice),
    median = median(X.Voice),
    sd= sd(X.Voice), 
    iqr=IQR(X.Voice)
  )
summary_stats

iqr_plot<-ggplot(summary_stats, aes(x = as.factor(Morph.ratio), y = iqr)) +
  geom_point(size = 3, color = "white")+
  geom_line(aes(group = 1), color = "black", size = 1)+
  labs(x = "Morph ratio",
       y = "Interquartile Range")
iqr_plot

figure_behav<-ggarrange(behav_bxp+ rremove("xlab"), iqr_plot,
                  labels = c("A", "B"),
                  ncol=1, nrow=2,align = "hv", heights = c(2, 1))
figure_behav
ggsave(paste0(plot_dir,"EEG_voice_behaviour.png"),dpi=800)

#  Read the data foranalysis of Amplitude Measurements -------------------------------------------------------------
data <- read.csv("/Users/hannahsmacbook/EEG_voice/EEG_data.csv", header=T, na.strings = "NaN")

# Analysis of Amplitude Measurements -------------------------------------------------------------
# Fit the Mixed Model 
full_model <- lmer(X.Voice ~ amp_126.450 + condition + (1|subj), data=data)
summary(full_model)
tab_model(full_model)

# Check the assumptions 
plot(full_model)
qqnorm(resid(full_model))
qqline(resid(full_model)) 

# Plot the model output
pred_amp<-plot_model(full_model, type = "pred", terms = c("amp_126.450"), colors="black")+
  labs(title = NULL) + ylim(c(0.0,1.0))

pred_amp_full<-pred_amp+geom_point(data = data, aes(x = amp_126.450, y = X.Voice),
                              color = "black", size = 2)+ 
  ylab("% Voice responses")+ xlab("GFP Amplitude (0.126-0.450 s)")
pred_amp_full


# Analysis of Amplitude Measurements Shorter Time Frame (Earlier)------------------------------------------------------------
# Fit the Mixed Model 
model_early_frame <- lmer(X.Voice ~ amp_126.250 + condition + (1|subj), data=data)
summary(model_early_frame)
tab_model(model_early_frame)

# Check the assumptions 
plot(model_early_frame)
qqnorm(resid(model_early_frame))
qqline(resid(model_early_frame)) 

# Plot the model output
pred_amp<-plot_model(model_early_frame, type = "pred", terms = c("amp_126.250"), colors="black")+
  labs(title = NULL) + ylim(c(0.0,1.0))

pred_amp_early<-pred_amp+geom_point(data = data, aes(x = amp_126.250, y = X.Voice),
                              color = "black", size = 2)+ 
  ylab("% Voice responses")+ xlab("GFP Amplitude (0.126-0.250)")
pred_amp_early

# Analysis of Amplitude Measurements Shorter Time Frame (Later)-------------------------------------------------------------
# Fit the Mixed Model 
model_late_frame <- lmer(X.Voice ~ amp_250.450 + condition + (1|subj), data=data)
summary(model_late_frame)
tab_model(model_late_frame)

# Check the assumptions 
plot(model_late_frame)
qqnorm(resid(model_late_frame))
qqline(resid(model_late_frame)) 

# Plot the model output
pred_amp<-plot_model(model_late_frame, type = "pred", terms = c("amp_250.450"), colors="black")+
  labs(title = NULL) + ylim(c(0.0,1.0))

pred_amp_late<-pred_amp+geom_point(data = data, aes(x = amp_250.450, y = X.Voice),
                              color = "black", size = 2)+ 
  ylab("% Voice responses")+ xlab("GFP Amplitude (0.250-0.450)")
pred_amp_late

# Final Figure: Correlation Amplitude and Behaviour-------------------------------------------------------------
theme_update(text = element_text(size=20, family="Times New Roman"))
figure_amp<-ggarrange(pred_amp_full + xlim(c(0.0,3.5)), 
                      pred_amp_early+ xlim(c(0.0,3.5)),
                      pred_amp_late+ xlim(c(0.0,3.5)), 
                        labels = c("A", "B", "C"),
                        ncol=3, nrow=1,align = "hv", heights = c(2, 1))
figure_amp
ggsave(paste0(plot_dir,"EEG-amp_voice_behaviour.png"),dpi=800)

# Control Conditions: Morph ratio and amplitude-------------------------------------------------------------
control1 <- lmer(amp_126.450 ~condition + (1|subj), data=data)
control2 <- lmer(amp_126.250 ~condition + (1|subj), data=data)
control3 <- lmer(amp_250.450 ~condition + (1|subj), data=data)

tab_model(control1)
tab_model(control2)
tab_model(control3)
