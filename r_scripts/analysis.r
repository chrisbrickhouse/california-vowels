setwd('/home/cj/Desktop/Linguistics/QP2/')
library(tidyverse)
library(vowels)  # Not actually used yet
library(gss)
library(lme4)
source('./r_scripts/analysis_functions.r')


# Load Data
data.sauce.demo = load_sauce('./data/full_data_with_demo.csv')
data.normed = make_norm(data.sauce.demo)
data.clean.sauce.demo = clean_sauce(data.sauce.demo,T)
data.clean.normed.demo = merge_demo(data.normed,data.clean.sauce.demo)

data.formant = load_data(paths)
data.formant$site = as.factor(data.formant$site)
data.formant$segment = as.factor(data.formant$segment)

data.v_angles = vowel_angles(data.clean.normed.demo)
data.cosangles = compute_vspace(data.v_angles)
plot_vangles(data.v_angles)

model.cosangles.data = data.cosangles%>%
  filter(cos_ratio > 0) %>%
  filter(!is.na(birthyear)) %>%
  filter(!site %in% c("RED","MER")) #%>%
  mutate(log_cos_ratio = log(cos_ratio)) %>%
  filter(!is.nan(log_cos_ratio)) %>%
  filter(!is.na(log_cos_ratio)) %>%
  filter(!is.na(gender)) %>%
model.cosangles.data$c_birthyear = as.numeric(model.cosangles.data$birthyear) - mean(as.numeric(model.cosangles.data$birthyear))
model.cosangles.data$c_gender = as.numeric(as.factor(model.cosangles.data$gender)) - mean(as.numeric(as.factor(model.cosangles.data$gender)))
model.cosangles.data$c_site = as.numeric(as.factor(model.cosangles.data$site)) - mean(as.numeric(as.factor(model.cosangles.data$site)))
model.cosangles.data$l_birthyear = log(as.numeric(model.cosangles.data$birthyear))
model.cosangles.data$l_gender = log(as.numeric(as.factor(model.cosangles.data$gender)))
model.cosangles.data$l_site = log(as.numeric(as.factor(model.cosangles.data$site)))
model.lm.cosangles = lm(log_cos_ratio~l_birthyear*site*gender,data=model.cosangles.data)
model.cosangles = lmer(log_cos_ratio~birthyear*site+(1|gender),data=model.cosangles.data)

# See number of vowels per site
table(data.cleannormed%>%select(site,Vowel))

# Smoothing Spline ANOVA models
ssdata = data.cleansauce %>%
  filter(segment %in% c("AO1","AA1")) %>%
  filter(!site %in% c("RED","RDL","MER"))

m = ssmodel(F1~index*segment*site,ssdata)
m2 = ssmodel(F2~index*segment*site,ssdata)
m3 = ssmodel(F3~index*segment*site,ssdata)
