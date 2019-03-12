setwd('/home/cj/Desktop/Linguistics/QP2/')
library(tidyverse)
library(vowels)  # Not actually used yet
library(gss)
source('./r_scripts/analysis_functions.r')


# Load Data
data.sauce = load_sauce('./data/all/full_data.csv')
data.saucedemo = load_sauce('./data/full_data_with_demo.csv')
data.normed = make_norm(data.sauce)
data.cleansauce = clean_sauce(data.sauce)
data.cleansaucedemo = clean_sauce(data.saucedemo,T)
data.cleannormed = clean_normed(data.normed)

data.formant = load_data(paths)
data.formant$site = as.factor(data.formant$site)
data.formant$segment = as.factor(data.formant$segment)

data.v_angles = vowel_angles(data.normed)
data.cosangles = compute_vspace(data.v_angles)
plot_vangles(data.v_angles)

# See number of vowels per site
table(data.cleannormed%>%select(site,Vowel))

# Smoothing Spline ANOVA models
ssdata = data.cleansauce %>%
  filter(segment %in% c("AO1","AA1")) %>%
  filter(!site %in% c("RED","RDL","MER"))

m = ssmodel(F1~index*segment*site,ssdata)
m2 = ssmodel(F2~index*segment*site,ssdata)
m3 = ssmodel(F3~index*segment*site,ssdata)
