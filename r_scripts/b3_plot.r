library(tidyverse)
library(gss)
bot_men = read_csv('/home/cj/Desktop/Linguistics/QP2/data/spectral_measures_cot_men.txt')
#bot_women = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAC_bot_women.csv')
#bot.sac = bind_rows(bot_men,bot_women)
bought_men = read_csv('/home/cj/Desktop/Linguistics/QP2/data/spectral_measures_caught_men.txt')
#bought_women = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAC_bought_women.csv')
#bought.sac = bind_rows(bought_men,bought_women)
#low_back.sac = bind_rows(bot.sac,bought.sac)
bot_men$var7 = as.integer(bot_men$var7)
low_back.sac = bind_rows(bot_men,bought_men)
low_back.sac = low_back.sac %>%
  filter(Label %in% c('AA1','AO1')) %>%
  select(site=var1,lastname=var2,firstname=var3,token=var8,segment=Label,index=t,f0,f1=F1,f2=F2,f3=F3,B3,H1A2c,CPP,seg_Start,seg_End) %>%
  mutate(log_dur = log(seg_End-seg_Start))

ggplot(low_back.sac,aes(x=index,group=token,color=segment)) +
  geom_point(aes(y=B3))# +
  #geom_line(aes(y=f2)) +
  #geom_line(aes(y=f3)) +
  #facet_wrap(~site)

low_back.sac$segment = as.factor(low_back.sac$segment)
  
B3.model = ssanova(B3 ~ index*segment*log_dur,data=low_back.sac)
B3.model.predicted = expand.grid(index=seq(1,50),segment=as.factor(c("AA1","AO1")))
B3.model.predicted = B3.model.predicted %>%
  group_by(segment) %>%
  mutate(log_dur = rnorm(1,-1.74,0.22)) %>%
  ungroup()
B3.model.predicted$B3.Fit <- predict(B3.model,newdata=B3.model.predicted, se = T)$fit
B3.model.predicted$B3.SE <- predict(B3.model, newdata = B3.model.predicted, se = T)$se.fit

ggplot(low_back.sac, aes(x=index)) +
  #geom_point(aes(y=f1_bark,group=token,shape=segment)) +
  geom_line(data = B3.model.predicted, aes(x = index, y = B3.Fit,color=segment)) +
  geom_ribbon(data = B3.model.predicted, aes(ymin = B3.Fit-B3.SE, ymax = B3.Fit+B3.SE, x = index,group=segment), alpha = 0.4)

SAL_bot_men = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAL_bot_men.csv')
SAL_bot_women = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAL_bot_women.csv')
bot.sal = bind_rows(SAL_bot_men,SAL_bot_women)
SAL_bought_men = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAL_bought_men.csv')
SAL_bought_women = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAL_bought_women.csv')
bought.sal = bind_rows(SAL_bought_men,SAL_bought_women)
low_back.sal= bind_rows(bot.sal,bought.sal)
low_back.sal$site = "SAL"

low_back.all = bind_rows(low_back.sal,low_back.sac)
low_back.all$segment=as.factor(low_back.all$segment)
low_back.all$site=as.factor(low_back.all$site)