library(tidyverse)
library(lmerTest)
library(report)

setwd("/Users/dariabikina/Documents/Harvard/ExperimentUniqueness/code")
rus_obj <- read.csv("rus_objects.csv")
eng_obj <- read.csv("eng_objects.csv")

#Comparing uniqueness in singular objects in English
eng_obj_sg <- eng_obj %>% filter(Context == "exp1" | Context == "exp3")
plot_confidence_eng_obj_sg <-  eng_obj_sg %>% ggplot(aes(x = Context, y=Rate)) + geom_boxplot() + labs(title = "Singular objects in English: a weaker requirement on uniqueness", subtitle ="Mean rates and 95% confidence intervals") + scale_x_discrete(labels = c("Uniqueness met", "Uniqueness not met")) + xlab(NULL)
model_eng_obj_sg <- lmer(Rate ~ Context + (1|Participant), data = eng_obj_sg)
eng_obj_sg$Rate <- as.factor(eng_obj_sg$Rate)
plot_eng_obj_sg <- eng_obj_sg %>% ggplot(aes(Context, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "Singular objects in English: a weaker requirement on uniqueness", subtitle = "Mean rates and 95% confidence intervals", fill = "Rate") + scale_x_discrete(labels = c("Uniqueness met", "Uniqueness not met")) + ylab(NULL) + xlab(NULL)

#Comparing uniqueness in singular objects in Russian
rus_obj_sg <- rus_obj %>% filter(Context =="exp1" | Context == "exp3")
plot_confidence_rus_obj_sg <- rus_obj_sg %>% ggplot(aes(x=Context, y=Rate)) + geom_boxplot() + labs(title = "Singular objects in Russian: a weaker requirement on uniqueness", subtitle ="Mean rates and 95% confidence intervals") + scale_x_discrete(labels = c("Uniqueness met", "Uniqueness not met")) +xlab(NULL)
model_rus_obj_sg <- lmer(Rate ~ Context + (1|Participant), data = rus_obj_sg)
rus_obj_sg$Rate <- as.factor(rus_obj_sg$Rate)
plot_rus_obj_sg <- rus_obj_sg %>% ggplot(aes(Context, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") + scale_y_continuous(labels = scales::percent) + labs(title = "Singular objects in Russian: a weaker requirement on uniqueness", subtitle = "Percentage distribution of ratings", fill = "Rate") + scale_x_discrete(labels = c("Uniqueness is met", "Uniqueness is not met")) + ylab(NULL)

#Comparing maximality in plural objects in English
eng_obj_pl <- eng_obj %>% filter(Context == "exp8" | Context == "exp6")
eng_obj_pl <- eng_obj_pl %>% mutate(Context_Right_Order = ifelse(Context == "exp8", "Maximality met", "Maximality not met"))
plot_confidence_eng_obj_pl <- eng_obj_pl %>% ggplot(aes(x=Context_Right_Order, y=Rate)) + geom_boxplot() + labs(title = "Plural objects in English: a weaker requirement on maximality", subtitle = "Mean rates and 95% confidence intervals") + xlab(NULL)
model_eng_obj_pl <- lmer(Rate ~ Context + (1|Participant), data = eng_obj_pl)
eng_obj_pl$Rate <- as.factor(eng_obj_pl$Rate) 
plot_eng_obj_pl <- eng_obj_pl %>% ggplot(aes(Context_Right_Order, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") +scale_y_continuous(labels = scales::percent) + labs(title = "Plural objects in English: a weaker requirement on maximality", subtitle = "Percentage distribution of ratings", fill = "Rate") + xlab(NULL) +ylab(NULL)

#Comparing maximality in plural objects in Russian
rus_obj_pl <- rus_obj %>% filter(Context == "exp8" | Context == "exp6")
rus_obj_pl <- rus_obj_pl %>% mutate(Context_Right_Order = ifelse(Context == "exp8", "Maximality met", "Maximality not met"))
plot_confidence_rus_obj_pl <- rus_obj_pl %>% ggplot(aes(x=Context_Right_Order, y=Rate)) + geom_boxplot() + labs(title = "Plural objects in Russian: no requirement on maximality", subtitle = "Mean rates and 95% confidence intervals") + xlab(NULL)
model_rus_obj_pl <- lmer(Rate ~ Context + (1|Participant), data = rus_obj_pl)
rus_obj_pl$Rate <- as.factor(rus_obj_pl$Rate) 
plot_rus_obj_pl <- rus_obj_pl %>% ggplot(aes(Context_Right_Order, fill = forcats::fct_rev(Rate))) + geom_bar(colour = "black", position = "fill") +scale_y_continuous(labels = scales::percent) + labs(title = "Plural objects in Russian: no requirement on maximality", subtitle = "Percentage distribution of ratings", fill = "Rate") + xlab(NULL) +ylab(NULL)

#Pre-processing the datasets before comparing the two languages
rus_obj <- rus_obj %>% mutate(Language = "Russian")
eng_obj <- eng_obj %>% mutate(Language = "English")
rus_eng_obj <- rbind(rus_obj, eng_obj)
rus_eng_obj$Rate <- as.numeric(rus_eng_obj$Rate)
model_objects <- lmer(Rate ~ Language + (1|Participant) + (1|Context), data = rus_eng_obj)

#Comparing just singulars in English and Russian
rus_eng_obj_sg <- rus_eng_obj %>% filter(Context == "exp1" | Context == "exp3")
model_sg_objects <- lmer(Rate ~ Language + (1|Participant) + (1|Context), data = rus_eng_obj_sg)

#Comparing just plurals in English and Russian
rus_eng_obj_pl <- rus_eng_obj %>% filter(Context == "exp8" | Context == "exp6")
model_pl_objects <- lmer(Rate ~ Context +(1|Participant) + (1|Context), data = rus_eng_obj_pl)
