library(tidyverse)
library(lmerTest)
library(dplyr)
library(ordinal)
library(rcompanion)

setwd("/Users/dariabikina/Documents/Harvard/ExperimentUniqueness/code")
eng <- read.csv("eng_2.csv")
rus <- read.csv("rus_2.csv")

#Comparing maximal and non-maximal plurals in English
plot_plural_english_confidence_intervals <- eng %>% filter(Context == "exp3" | Context == "exp8") %>% ggplot(aes(x=Context, y=Rate)) + geom_boxplot(notch=FALSE) + scale_x_discrete(labels = c('Maximality met','Maximality not met')) + labs(title = "Acceptability rates of English definite plural NPs: maximality vs. non-maximality", subtitle = "Mean rates and 95% confidence intervals") +xlab(NULL)
only_eng_plurals <- eng %>% filter(Context == "exp3" | Context == "exp8")  
model_eng_plurals = lmer(Rate ~ Context + (1|Participant), data = only_eng_plurals)
only_eng_plurals$Rate <- only_eng_plurals$Rate - 1
only_eng_plurals$Rate <- as.factor(only_eng_plurals$Rate)
plot_eng_percentage_plurals <- ggplot(only_eng_plurals, aes(Context, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "English plural NPs: maximality vs. non-maximality", subtitle = "Percentage distribution of ratings", fill = "Rate") + scale_x_discrete(labels = c("Maximality met", "Maximality not met")) + xlab(NULL) + ylab(NULL)

#Comparing maximal and non-maximal plurals in Russian
only_rus_plurals <- rus %>% filter(Context == "exp3" | Context == "exp8")
model_rus_plurals = lmer(Rate ~ Context + (1|Participant), data = only_rus_plurals)
plot_plurals_rus_confidence_interval <- only_rus_plurals %>% ggplot(aes(x=Context, y=Rate)) + geom_boxplot(notch=FALSE) + scale_x_discrete(labels = c("Maximality met", "Maximality not met")) + labs(title = "Acceptability rates of Russian bare plural NPs: maximality vs. non-maximality", subtitle = "Mean rates and 95% confidence intervals") +xlab(NULL)
only_rus_plurals$Rate <- as.factor(only_rus_plurals$Rate)
plot_rus_percentage_plurals <- ggplot(only_rus_plurals, aes(Context, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "Russian plural NPs: maximality vs. non-maximality", subtitle = "Percentage distribution of ratings", fill = "Rate") + scale_x_discrete(labels = c("Maximality met", "Maximality not met")) + xlab(NULL) + ylab(NULL)

#Comparing unique and non-unique singulars in Russian
rus_exp1vs2 <- rus %>% filter(Context == "exp1" | Context == "exp2")
model_rus_exp1vs2 <- lmer(Rate ~ Context + (1|Participant), data = rus_exp1vs2)
plot_rus_sg_uniqueness_confidence <- rus_exp1vs2 %>% ggplot(aes(x=Context, y=Rate)) + geom_boxplot(notch=FALSE) + scale_x_discrete(labels = c('Uniqueness met', 'Uniqueness not met')) + labs(title = "Acceptability rate of Russian bare singular NPs: uniqueness vs. non-uniqueness", subtitle = "Mean rates and 95% confidence intervals") + xlab(NULL)
rus_exp1vs2$Rate <- as.factor(rus_exp1vs2$Rate)
plot_rus_sg_uniqueness <- rus_exp1vs2 %>% ggplot(aes(Context, fill= forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "Russian singular NPs: uniqueness vs. non-uniqueness", subtitle = "Percentage distribution of ratings", fill="Rate")+scale_x_discrete(labels = c("Uniqueness met", "Uniqueness not met")) + xlab(NULL) + ylab(NULL)

#Comparing unique and non-unique singulars in English
eng_exp1vs2 <- eng %>% filter(Context == "exp1" | Context == "exp2")
plot_eng_sg_uniqueness_confidence <- eng_exp1vs2 %>% ggplot(aes(x=Context, y=Rate)) + geom_boxplot(notch=FALSE) + scale_x_discrete(labels = c('Uniqueness met', 'Uniqueness not met')) + labs(title = "Acceptability rate of English definite singular NPs: uniqueness vs. non-uniqueness", subtitle = "Mean rates and 95% confidence intervals") + xlab(NULL)
model_eng_exp1vs2 <- lmer(Rate ~ Context + (1|Participant), data = eng_exp1vs2)
eng_exp1vs2$Rate <- as.factor(eng_exp1vs2$Rate)
plot_eng_sg_uniqueness <- eng_exp1vs2 %>% ggplot(aes(Context, fill= forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "English definite singular NPs: uniqueness vs. non-uniqueness", subtitle = "Percentage distribution of ratings", fill="Rate")+scale_x_discrete(labels = c("Uniqueness met", "Uniqueness not met")) + xlab(NULL) + ylab(NULL)

#Comparing zoomed and non-zoomed singulars in English
eng_zoom <- read.csv("English_Zooming_preprocessed.csv")
model_zoom_eng <- lmer(rate ~ zooming + (1|participant), data = eng_zoom)
plot_eng_zoom_confidence <- eng_zoom %>% ggplot(aes(x=zooming, y=rate)) +geom_boxplot() + xlab(NULL) + ylab("Rate") + labs(title = "Acceptability rate of English singular plurals with non-unique individuals", subtitle = "The zooming-in effect") + scale_x_discrete(labels = c("The girl is not centered", "The girl is centered"))
eng_zoom$rate <- as.factor(eng_zoom$rate)
plot_eng_zoom <- eng_zoom %>% ggplot(aes(zooming, fill = forcats::fct_rev(rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "The absence of the zooming-in effect in English", fill = "Rate") + xlab(NULL) + ylab(NULL) + scale_x_discrete(labels = c("The girl is not centered", "The girl is centered"))

#Comparing zoomed and non-zoomed singulars in Russian
rus_zoom <- read.csv("Russian_Zooming_preprocessed.csv")
colnames(rus_zoom) <- c("Participant", "Rate", "Zooming", "Language")
rus_zoom <- rus_zoom[,1:4]
model_rus_zoom <- lmer(Rate ~ Zooming + (1|Participant), data = rus_zoom)
plot_rus_zoom_confidence <- rus_zoom %>% ggplot(aes(x=Zooming, y=Rate)) +geom_boxplot() + xlab(NULL) + labs(title = "Acceptability rates of singular NPs for non-unique induviduals", subtitle = "The zooming in effect") +  scale_x_discrete(labels = c("The girl is not centered", "The girl is centered")) 
rus_zoom$Rate <- as.factor(rus_zoom$Rate)
plot_rus_zoom <- rus_zoom %>% ggplot(aes(Zooming, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") +scale_y_continuous(labels = scales:: percent) + labs(titile = "Russian singular NPs: the zooming in effect", subtitle = "Percentage distribution of the ratings", fill = "Rate") + xlab(NULL) +ylab(NULL) + scale_x_discrete(labels = c("The girl is not centered", "The girl is centered"))

#Modifying the datasets in order to compare languages
eng <- eng %>% mutate(Language = "English")
eng_participant_context_rate_language <- eng[,c(1,2, 8, 9)]
rus <- rus %>% mutate(Language = "Russian")
rus_participant_context_rate_language <- rus[, c(1,2,8,9)]
rus_participant_context_rate_language$Participant <- sub("^", "rus-", rus_participant_context_rate_language$Participant )
colnames(eng_zoom) <- c("Participant", "Rate", "Zooming", "Language")
eng_zoom$Rate <- as.numeric(eng_zoom$Rate)
rus_zoom$Rate <- as.numeric(rus_zoom$Rate)
rus_zoom$Participant <- sub("^", "rus-", rus_zoom$Participant)
eng_rus_zoom <- rbind(eng_zoom, rus_zoom)
languages_compare <- rbind(eng_participant_context_rate_language, rus_participant_context_rate_language)


#Comparing English and Russian: Experiment 1, part 1 (without zooming)
model_languages <- lmer(Rate ~ Language + (1|Participant) + (1|Context), data = languages_compare)
model_zoom_languages <- lmer(Rate ~ Language + (1|Participant) + (1|Zooming), data = eng_rus_zoom)

#Comparing just plurals in both languages
subjects_pl <- languages_compare %>% filter(Context == "exp3" | Context == "exp8")
model_subjects_pl <- lmer(Rate ~ Language + (1|Participant) +(1|Context), data = subjects_pl)

#Comparing just singulars in both languages 
subjects_sg <- languages_compare %>% filter(Context == "exp1" | Context == "exp2")
model_subjects_sg <- lmer(Rate ~ Language + (1|Participant) + (1|Context), data = subjects_sg)