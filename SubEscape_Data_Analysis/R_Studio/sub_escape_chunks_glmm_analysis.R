# Load data
#data = read.csv('C:/Users/PC/Documents/Projects/Github/SubEscape/SubEscape_Data_Analysis/Pickel_n_CSV_files/df_chunks949.csv')
data = read.csv('H:/Project/SubEscape/SubEscape_Data_Analysis/Pickel_n_CSV_files/ChunkedData.csv')
View(data)

# Convert to nominal factor 
data$PtxID = factor(data$PtxID)
data$Chunk = factor(data$Chunk)
data$Group = factor(data$Group)

summary(data)

#install.packages("plyr")
library(plyr)

# Explore data in light of EndError metric 
ddply(data, ~ Chunk * Group, function(data) summary(data$MAE_Chunk))
ddply(data, ~ Chunk * Group, summarise, MAE_Chunk = mean(MAE_Chunk), sd = sd(MAE_Chunk))

# Histograms of data
hist(data[data$Chunk == "14" & data$Group == "NoReward",]$MAE_Chunk)
hist(data[data$Chunk == "24" & data$Group == "NoReward",]$MAE_Chunk)
# hist(data[data$Phase == "Washout" & data$Group == "NoReward",]$MAE_Chunk)

hist(data[data$Chunk == "14" & data$Group == "Reward",]$MAE_Chunk)
hist(data[data$Chunk == "24" & data$Group == "Reward",]$MAE_Chunk)
# hist(data[data$Phase == "Washout" & data$Group == "Reward",]$MAE_Chunk)

#height_reorder <- with(data, reorder(data$height=="low", data$height=="mid", data$height=="high", FUN=mean))

# Reorder box plot height values 
# data$height2 <- factor(data$height, levels = c("low", "mid", "high"))
# data$tempos2 <- factor(data$tempos, levels = c("80", "120", "160"))
# data$TargetID2 <- factor(data$TargetID, levels = c("row_A1", "row_A2", "row_A3","row_A4", "row_A5", "row_A6",
#                                                    "row_B1", "row_B2", "row_B3","row_B4", "row_B5", "row_B6",
#                                                    "row_C1", "row_C2", "row_C3","row_C4", "row_C5", "row_C6"))

boxplot(data$MAE_Chunk ~ data$Chunk)
boxplot(data$MAE_Chunk ~ data$Group)
with(data, interaction.plot(Chunk, Group, MAE_Chunk))

# install.packages("statmod")
# install.packages("lme4")
# install.packages("lmerTest")
# install.packages("car")
library(lme4)
library(lmerTest)
library(car)

# Post hoc pairwise comparisons packages 
# install.packages("multcomp")
# install.packages("lsmeans")
library(multcomp)
library(lsmeans)

# Set sum-to-zero contrasts for the Anova cells 
# contrasts(data$Phase) <- "contr.sum"
# contrasts(data$Group) <- "contr.sum"
# contrasts(data$Trial) <- "contr.sum"


# LMM order effect test
# Subject is a random effect 
#m = lmer(EndError_cleaned ~ (tempos * height * TargetID)/TrialNum + (1|PtxID), data=data)
#m = lmer(EndError_cleaned ~ (tempos * height * TargetID) + (1|height:tempos:TargetID:TrialNum) + (1|PtxID), data=data)

# -------------------------------------------------------------------------------------------------
# -------------------------------------- Mean Absolute Error --------------------------------------
# -------------------------------------------------------------------------------------------------

# Clean up data frame i.e. remove NaNs and inf values
df = data
df <- na.omit(df)
df <- df[!is.infinite(rowSums(df)),]

# see if new Errors data seems Gamma-distributed
# install.packages("fitdistrplus")
library(fitdistrplus)
fit = fitdist(df[df$Chunk == "Baseline" & df$Group == "NoReward",]$MAE_Chunk, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Chunk == "Adaptation" & df$Group == "NoReward",]$MAE_Chunk, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Chunk == "Washout" & df$Group == "NoReward",]$MAE_Chunk, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Chunk == "Baseline" & df$Group == "Reward",]$MAE_Chunk, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Chunk == "Adaptation" & df$Group == "Reward",]$MAE_Chunk, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Chunk == "Washout" & df$Group == "Reward",]$MAE_Chunk, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test


# m = glmer(MeanAbsErr ~ (Phase + Group) + (1|PtxID), data=data)
m = glmer(MAE_Chunk ~ Group, data=data, family = gaussian)
Anova(m, type=3, test.statistic = "F")

# not in Coursera video; treat "Trial" as a nested random effect.
# m = glmer(Errors ~ (Keyboard * Posture) + (1|Keyboard:Posture:Trial) + (1|Subject), data=mbltxttrials, family=poisson, nAGQ=0) # new, correct syntax
# Anova(m, type=3)

# m = glmer(MeanAbsErr/max(MeanAbsErr) ~ (Phase  + Group) + (1|PtxID), data=data, family = binomial(link = "logit")) # Convert y values to be between 0 and 1
# m = glmer(MeanAbsErr ~ (Phase  + Group) + (1|PtxID), data=data, family = Gamma(link = "inverse"))
#m = glmer(MeanAbsErr ~ (Phase  + Group) + (1|PtxID), data=data, family = inverse.gaussian(link = "1/mu^2"))
# m = glmer(MeanAbsErr ~ (Phase  + Group) + (1|PtxID), data=data, family = poisson)
# m = glmer(MeanAbsErr ~ (Phase  + Group) + (1|PtxID), data=data, family = Gamma)
# Anova(m, type=3, test.statistic = "F")
# summary(m)

# Post Hoc Analysis part 
# Positional Error post hoc analysis
summary(glht(m, lsm(pairwise ~ Chunk * Group)), test=adjusted(type="holm"))
with(df, interaction.plot(Chunk, Group, MAE_Chunk))


#library(multcomp) # for glht
#library(emmeans) # for emm

# perform post hoc pairwise comparisons
#with(df, interaction.plot(Chunk, Group, MAE_Chunk, ylim=c(0, max(df$MAE_Chunk)))) # for convenience
#summary(glht(m, emm(pairwise ~ Chunk * Group)), test=adjusted(type="holm"))








# # -------------------------------------------------------------------------------------------------
# # -------------------------------------- Path Offset ----------------------------------------------
# # -------------------------------------------------------------------------------------------------
# 
# m2 = glmer(PathOffsetNoLag_cleaned ~ (TargetID * tempos * height) + (1|PtxID), data=data)
# Anova(m2, type=3, test.statistic = "F")
# 
# # Path offset post hoc analysis
# summary(glht(m2, lsm(pairwise ~ TargetID * tempos)), test=adjusted(type="holm"))
# with(data, interaction.plot(TargetID, height, EndError_cleaned))
# 
# 
# # -------------------------------------------------------------------------------------------------
# # -------------------------------------- Angular Error ----------------------------------------------
# # -------------------------------------------------------------------------------------------------
# 
# dataAng = read.csv('E:/Projects/QuestAccuracyAnalysis/AngularError.csv')
# # Convert to nominal factor 
# data$Ptx = factor(data$Ptx)
# data$Tempo = factor(data$Tempo)
# data$Joint = factor(data$Joint)
# data$AngErr_NoLag = factor(data$AngErr_NoLag)
# summary(dataAng)
# 
# m2 = glmer(AngErr_NoLag ~ (Tempo * Joint) + (1|Ptx), data=dataAng)
# Anova(m2, type=3, test.statistic = "F")
# 
# # Path offset post hoc analysis
# summary(glht(m2, lsm(pairwise ~ Tempo + Joint)), test=adjusted(type="holm"))
# with(dataAng, interaction.plot(Tempo, Joint, AngErr_NoLag))
