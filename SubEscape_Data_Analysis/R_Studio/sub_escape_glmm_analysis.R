# Load data
data = read.csv('C:/Users/PC/Documents/Projects/Github/SubEscape/SubEscape_Data_Analysis/Pickel_n_CSV_files/MeanAbsErrDF.csv')
View(data)

# Convert to nominal factor 
data$PtxID = factor(data$PtxID)
data$Phase = factor(data$Phase)
data$group = factor(data$group)
data$trial = factor(data$trial)

summary(data)

#install.packages("plyr")
library(plyr)

# Explore data in light of EndError metric 
ddply(data, ~ Phase * group, function(data) summary(data$MeanAbsErr))
ddply(data, ~ Phase * group, summarise, MeanAbsErr.mean = mean(MeanAbsErr), sd = sd(MeanAbsErr))

# Histograms of data
hist(data[data$Phase == "Baseline" & data$group == "NoReward",]$MeanAbsErr)
hist(data[data$Phase == "Adaptation" & data$group == "NoReward",]$MeanAbsErr)
hist(data[data$Phase == "Washout" & data$group == "NoReward",]$MeanAbsErr)

hist(data[data$Phase == "Baseline" & data$group == "Reward",]$MeanAbsErr)
hist(data[data$Phase == "Adaptation" & data$group == "Reward",]$MeanAbsErr)
hist(data[data$Phase == "Washout" & data$group == "Reward",]$MeanAbsErr)

#height_reorder <- with(data, reorder(data$height=="low", data$height=="mid", data$height=="high", FUN=mean))

# Reorder box plot height values 
# data$height2 <- factor(data$height, levels = c("low", "mid", "high"))
# data$tempos2 <- factor(data$tempos, levels = c("80", "120", "160"))
# data$TargetID2 <- factor(data$TargetID, levels = c("row_A1", "row_A2", "row_A3","row_A4", "row_A5", "row_A6",
#                                                    "row_B1", "row_B2", "row_B3","row_B4", "row_B5", "row_B6",
#                                                    "row_C1", "row_C2", "row_C3","row_C4", "row_C5", "row_C6"))

boxplot(data$MeanAbsErr ~ data$Phase)
boxplot(data$MeanAbsErr ~ data$group)
with(data, interaction.plot(Phase, group, MeanAbsErr))

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
library(multcomp)# for glht
library(lsmeans)
library(emmeans) # for emm

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
fit = fitdist(df[df$Phase == "Baseline" & df$group == "NoReward",]$MeanAbsErr, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Phase == "Adaptation" & df$group == "NoReward",]$MeanAbsErr, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Phase == "Washout" & df$group == "NoReward",]$MeanAbsErr, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Phase == "Baseline" & df$group == "Reward",]$MeanAbsErr, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Phase == "Adaptation" & df$group == "Reward",]$MeanAbsErr, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test
fit = fitdist(df[df$Phase == "Washout" & df$group == "Reward",]$MeanAbsErr, "gamma", discrete=TRUE)
gofstat(fit) # goodness-of-fit test



# MeanSqErr
# m = glmer(MeanAbsErr ~ (Phase + Group) + (1|PtxID), data=data)
#m = glmer(Intercept ~ (Phase + Group) + (1|Phase:Group:Trial) + (1|PtxID), data=df, family = gaussian)
m = glmer(MeanAbsErr ~ (Phase * group) + (1|PtxID), data=df, family = gaussian)
Anova(m, type=2, test.statistic = "F")

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
#summary(glht(m, lsm(pairwise ~ TargetID * height)), test=adjusted(type="holm"))
#with(df, interaction.plot(TargetID, height, EndError_cleaned))


# perform post hoc pairwise comparisons
with(df, interaction.plot(Phase, group, MeanAbsErr, ylim=c(0, max(df$MeanAbsErr)))) # for convenience
summary(glht(m, emm(pairwise ~ Phase * group)), test=adjusted(type="holm"))








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
