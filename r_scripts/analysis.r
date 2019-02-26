setwd('/home/cj/Desktop/Linguistics/QP2/')
library(tidyverse)
library(vowels)  # Not actually used yet
library(gss)
source('./r_scripts/functions.r')

# Load Data
data.sauce = load_sauce('./data/sac_spectral_measures.txt')
data.normed = make_norm(data.sauce)
data.cleansauce = clean_sauce(data.sauce)

data.formant = load_data(paths)
data.formant$site = as.factor(data.formant$site)
data.formant$segment = as.factor(data.formant$segment)

# Vowel space plot
ggplot(data.normed,aes(x=nF2,y=nF1,color=Vowel,label=Vowel)) +
  geom_point() +
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") +
  geom_density_2d()

# Formant Track plot
ggplot(data.formant,aes(x=index,group=token,color=segment)) +
  geom_line(aes(y=f1_bark)) +
  geom_line(aes(y=f2_bark)) +
  geom_line(aes(y=f3_bark)) +
  facet_wrap(~site)

# Smoothing Spline Plot
f1_model = ssmodel(f1_bark ~ index*segment*site*sex,data=data.formant)

site_labels <- c(SAC="Sacramento",SAL="Salinas")
a = plot_spline(f1_model,site_labels,ylab="Predicted F1 (Bark)")
print(a)
