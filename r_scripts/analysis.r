library(tidyverse)
library(vowels)  # Not actually used yet
library(gss)
source('./r_scripts/functions.r')

# Load Data
### Data locations
setwd('/home/cj/Desktop/Linguistics/QP2/')
paths <- c(
  bot = c(
    men = c(
      sac = './data/SAC_bot_men.csv',
      sal = './data/SAL_bot_men.csv'
    ),
    women = c(
      sac = './data/SAC_bot_women.csv',
      sal = './data/SAL_bot_women.csv'
    )
  ),
  bought = c(
    men = c(
      sac = './data/SAC_bought_men.csv',
      sal = './data/SAL_bought_men.csv'
    ),
    women = c(
      sac = './data/SAC_bought_women.csv',
      sal = './data/SAL_bought_women.csv'
    )
  )
)


data = load_data(paths)
data$site = as.factor(data$site)
data$segment = as.factor(data$segment)

f1_model = ssmodel(f1_bark ~ index*segment*site*sex,data=data)

site_labels <- c(SAC="Sacramento",SAL="Salinas")
a = plot_spline(f1_model,site_labels,ylab="Predicted F1 (Bark)")
print(a)

ggplot(data,aes(x=index,group=token,color=segment)) +
  geom_line(aes(y=f1_bark)) +
  geom_line(aes(y=f2_bark)) +
  geom_line(aes(y=f3_bark)) +
  facet_wrap(~site)