library(tidyverse)
library(gss)
bot_men = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAC_bot_men.csv')
bot_women = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAC_bot_women.csv')
bot.sac = bind_rows(bot_men,bot_women)
bought_men = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAC_bought_men.csv')
bought_women = read_csv('/home/cj/Desktop/Linguistics/QP2/data/SAC_bought_women.csv')
bought.sac = bind_rows(bought_men,bought_women)
low_back.sac = bind_rows(bot.sac,bought.sac)
low_back.sac$site = "SAC"

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



ggplot(low_back.all,aes(x=index,group=token,color=segment)) +
  geom_line(aes(y=f1_bark)) +
  geom_line(aes(y=f2_bark)) +
  geom_line(aes(y=f3_bark)) +
  facet_wrap(~site)

f1.model = ssanova(f1_bark ~ index*segment*site,data=low_back.all)
f1.model.predicted = expand.grid(index=seq(1,50),segment=as.factor(c("AA1","AO1")),sex=c(1,2),site=c("SAC","SAL"))
f1.model.predicted$F1.Fit <- predict(f1.model,newdata=f1.model.predicted, se = T)$fit
f1.model.predicted$F1.SE <- predict(f1.model, newdata = f1.model.predicted, se = T)$se.fit

f2.model = ssanova(f2_bark ~ index*segment*site,data=low_back.all)
f2.model.predicted = expand.grid(index=seq(1,50),segment=as.factor(c("AA1","AO1")),sex=c(1,2),site=c("SAC","SAL"))
f2.model.predicted$F2.Fit <- predict(f2.model,newdata=f2.model.predicted, se = T)$fit
f2.model.predicted$F2.SE <- predict(f2.model, newdata = f2.model.predicted, se = T)$se.fit

f3.model = ssanova(f3_bark ~ index*segment*site,data=low_back.all)
f3.model.predicted = expand.grid(index=seq(1,50),segment=as.factor(c("AA1","AO1")),sex=c(1,2),site=c("SAC","SAL"))
f3.model.predicted$F3.Fit <- predict(f3.model,newdata=f3.model.predicted, se = T)$fit
f3.model.predicted$F3.SE <- predict(f3.model, newdata = f3.model.predicted, se = T)$se.fit

labels <- c(SAC="Sacramento",SAL="Salinas")
ggplot(low_back.all, aes(x=index)) +
  #geom_point(aes(y=f1_bark,group=token,shape=segment)) +
  geom_line(data = f1.model.predicted, aes(x = index, y = F1.Fit,color=segment,linetype="F1")) +
  geom_ribbon(data = f1.model.predicted, aes(ymin = F1.Fit-F1.SE, ymax = F1.Fit+F1.SE, x = index,group=segment), alpha = 0.4) +
  #geom_point(aes(y=f2_bark,group=token,shape=segment)) +
  geom_line(data = f2.model.predicted, aes(x = index, y = F2.Fit,color=segment,linetype="F2")) +
  geom_ribbon(data = f2.model.predicted, aes(ymin = F2.Fit-F2.SE, ymax = F2.Fit+F2.SE, x = index,group=segment), alpha = 0.4) +
  #geom_point(aes(y=f3_bark,group=token,shape=segment)) +
  geom_line(data = f3.model.predicted, aes(x = index, y = F3.Fit,color=segment,linetype="F3")) +
  geom_ribbon(data = f3.model.predicted, aes(ymin = F3.Fit-F3.SE, ymax = F3.Fit+F3.SE, x = index,group=segment), alpha = 0.4) +
  labs(y="Frequency (Bark)",x="Index",title="Fitted formant trajectories by field site.") +
  facet_wrap(~site,labeller=labeller(site=labels))
