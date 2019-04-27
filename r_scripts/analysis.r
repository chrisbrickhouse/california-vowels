setwd('/home/cj/Desktop/Linguistics/QP2/')
library(tidyverse)
library(vowels)
library(gss)
library(lme4)
library(dtt)
library(gridExtra)
source('./r_scripts/analysis_functions.r')


# Load Data
data.sauce.demo = load_sauce('./data/full_data_with_demo.csv')
data.normed = make_norm(data.sauce.demo)
data.clean.sauce.demo = clean_sauce(data.sauce.demo,T)
data.clean.normed.demo = merge_demo(data.normed,data.clean.sauce.demo)

# F1-F2 analysis
data.f1f2 = data.clean.normed.demo %>%
  ungroup() %>%
  filter(tolower(Context) %in% c("cot","caught")) %>%
  select(Speaker,site,Context,nF1,nF2,gender,birthyear,race,sexual_orientation,education,town_orientation,politics) %>%
  unite(Fs,nF1,nF2,sep="_") %>%
  spread(Context,Fs) %>%
  separate(COT,c("nF1_bot","nF2_bot"),sep="_",convert=TRUE) %>%
  separate(CAUGHT,c("nF1_bought","nF2_bought"),sep="_",convert=TRUE) %>%
  mutate(F1diff=nF1_bought-nF1_bot,F2diff=nF2_bought-nF2_bot)%>%
  drop_na(gender,birthyear)%>%
  filter(!site %in% c("MER","RED","RDL"))%>%
  mutate(ed=sqrt(F1diff**2 + F2diff**2))

data.f1f2$cbirthyear = as.numeric(data.f1f2$birthyear) - mean(as.numeric(data.f1f2$birthyear))
data.f1f2$cgender = as.numeric(as.factor(data.f1f2$gender)) - mean(as.numeric(as.factor(data.f1f2$gender)))
data.f1f2$csite = as.numeric(as.factor(data.f1f2$site)) - mean(as.numeric(as.factor(data.f1f2$site)))

model.t.F1 = t.test(data.f1f2$F1diff)
model.t.F2 = t.test(data.f1f2$F2diff)

model.f1 = lmer(F1diff ~ cgender*cbirthyear+(1|site),data=data.f1f2)
model.f1.simple = lm(F1diff ~ site*gender*birthyear-site,data=data.f1f2)
model.f2 = lmer(F2diff ~ cgender*cbirthyear+(1|site),data=data.f1f2)
model.f2.simple = lm(F2diff ~ site*gender*birthyear-site,data=data.f1f2)

model.ed = lmer(-log(ed)~cgender*cbirthyear+(1|site),data=data.f1f2)

ggplot(data.f1f2,aes(y=ed,x=birthyear))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(y="Euclidean distance",x="Birth year")

###
# DCT analysis
###
# F1 Plots
######
errlist = data.frame(index=NA,correlation=NA)
for (n in 1:10) {
  x = dct_predictions(data.clean.sauce.demo,"F1",n)%>%mutate(diff=pF1-F1)%>%select(diff)%>%drop_na()
  sse = sum(x**2)
  errlist = rbind(errlist,c(n,sse))
}
errlist = errlist%>%drop_na()
p1 = ggplot(errlist,aes(x=index,y=correlation))+
  geom_point()+
  geom_line()+
  labs(x="",y="Sum of Squared Errors",title="F1")
plot(errlist);lines(errlist)

secondlist = data.frame(index=NA,correlation=NA)
for (i in 2:9) {
  x = errlist$correlation[i+1] + errlist$correlation[i-1] - 2 * errlist$correlation[i]
  secondlist = rbind(secondlist,c(i,x))
}
p2 = ggplot(secondlist,aes(x=index,y=abs(correlation)))+
  geom_point()+
  geom_line()+
  labs(x="Number of DCT coefficients",y="Estimated Second Derivative")+
  xlim(c(1,10))
grid.arrange(p1,p2)

# F2 Plots
#######
errlist = data.frame(index=NA,correlation=NA)
for (n in 1:10) {
  x = dct_predictions(data.clean.sauce.demo,"F2",n)%>%mutate(diff=pF1-F1)%>%select(diff)%>%drop_na()
  sse = sum(x**2)
  errlist = rbind(errlist,c(n,sse))
}
errlist = errlist%>%drop_na()
p1 = ggplot(errlist,aes(x=index,y=correlation))+
  geom_point()+
  geom_line()+
  labs(x="",y="Sum of Squared Errors",title="F2")
plot(errlist);lines(errlist)

secondlist = data.frame(index=NA,correlation=NA)
for (i in 2:9) {
  x = errlist$correlation[i+1] + errlist$correlation[i-1] - 2 * errlist$correlation[i]
  secondlist = rbind(secondlist,c(i,x))
}
p2 = ggplot(secondlist,aes(x=index,y=abs(correlation)))+
  geom_point()+
  geom_line()+
  labs(x="Number of DCT coefficients",y="Estimated Second Derivative")+
  xlim(c(1,10))
grid.arrange(p1,p2)

# DCT F1 Analysis
###
dct_coeffs.f1 = get_dct(data.clean.sauce.demo,"F1")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site,gender)%>%
  unite(id,site,last,first,remove = FALSE)
dct_coeffs.f1$token = as.numeric(as.factor(dct_coeffs.f1$token))-1
dct_coeffs.f1$cDCT1 = center(dct_coeffs.f1$DCT1)
dct_coeffs.f1$cDCT2 = center(dct_coeffs.f1$DCT2)
dct_coeffs.f1$cDCT3 = center(dct_coeffs.f1$DCT3)
dct_coeffs.f1$cDCT4 = center(dct_coeffs.f1$DCT4)
dct_coeffs.f1$cbirthyear = center(dct_coeffs.f1$birthyear)
dct_coeffs.f1$csite = as.numeric(as.factor(dct_coeffs.f1$site)) - mean(as.numeric(as.factor(dct_coeffs.f1$site)))
dct_coeffs.f1$cgender = as.numeric(as.factor(dct_coeffs.f1$gender)) - mean(as.numeric(as.factor(dct_coeffs.f1$gender)))
model.dct.F1 = glm(token~cDCT1*cDCT2*cDCT3*cDCT4,
                   data=dct_coeffs.f1,
                   family="binomial")
summary(model.dct.F1)

# DCT F2 Analysis
###
dct_coeffs.f2 = get_dct(data.clean.sauce.demo,"F2")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site,gender)%>%
  unite(id,site,last,first,remove = FALSE)
dct_coeffs.f2.plot=dct_coeffs.f2
dct_coeffs.f2$token = as.numeric(as.factor(dct_coeffs.f2$token))-1
dct_coeffs.f2$cDCT1 = center(dct_coeffs.f2$DCT1)
dct_coeffs.f2$cDCT2 = center(dct_coeffs.f2$DCT2)
dct_coeffs.f2$cDCT3 = center(dct_coeffs.f2$DCT3)
dct_coeffs.f2$cDCT4 = center(dct_coeffs.f2$DCT4)
dct_coeffs.f2$cbirthyear = center(dct_coeffs.f2$birthyear)
dct_coeffs.f2$csite = as.numeric(as.factor(dct_coeffs.f2$site)) - mean(as.numeric(as.factor(dct_coeffs.f2$site)))
dct_coeffs.f2$cgender = as.numeric(as.factor(dct_coeffs.f2$gender)) - mean(as.numeric(as.factor(dct_coeffs.f2$gender)))
dct_coeffs.f2$rDCT1 = sample(dct_coeffs.f2$DCT1)
dct_coeffs.f2$rDCT2 = sample(dct_coeffs.f2$DCT2)
dct_coeffs.f2$rDCT3 = sample(dct_coeffs.f2$DCT3)
dct_coeffs.f2$rDCT4 = sample(dct_coeffs.f2$DCT4)
dct_coeffs.f2$rbirthyear = sample(dct_coeffs.f2$cbirthyear)
dct_coeffs.f2$rsite = sample(dct_coeffs.f2$csite)
dct_coeffs.f2$rgender = sample(dct_coeffs.f2$cgender)
model.dct.F2 = glm(token~(cDCT1*cDCT2*cDCT3)*(cbirthyear+cgender),
                   data=dct_coeffs.f2%>%filter(abs(DCT2)<2500)%>%filter(DCT1<17500),
                   family="binomial")
summary(model.dct.F2)

# Example graphs
####
dct_example_vec = c(rnorm(4,0,5),0,0,0,0,0,0)
dct_example_vec[4] = 1
base_track = dct(dct_example_vec,inverted = T)
DCT1_plot_data = data.frame(x=1:10,base=base_track)
DCT1_example = dct_example_vec
DCT1_example[1] = -3
DCT1_plot_data$a = dct(DCT1_example,inverted = T)
DCT1_example[1] = 0
DCT1_plot_data$b = dct(DCT1_example,inverted = T)
DCT1_example[1] = 2
DCT1_plot_data$c = dct(DCT1_example,inverted = T)
DCT1_example[1] = 10
DCT1_plot_data$d = dct(DCT1_example,inverted = T)
p1 = ggplot(DCT1_plot_data,aes(x=x))+
  geom_smooth(se=F,aes(y=base_track,color="Base"))+
  geom_smooth(se=F,aes(y=a,color="-3"))+
  geom_smooth(se=F,aes(y=b,color="0"))+
  geom_smooth(se=F,aes(y=c,color="2"))+
  geom_smooth(se=F,aes(y=d,color="10"))+
  labs(y="",x="Index",color="DCT1")+
  scale_y_continuous(limits = c(-4, 4))

base_track = dct(dct_example_vec,inverted = T)
DCT2_plot_data = data.frame(x=1:10,base=base_track)
DCT2_example = dct_example_vec
DCT2_example[2] = -3
DCT2_plot_data$a = dct(DCT2_example,inverted = T)
DCT2_example[2] = 0
DCT2_plot_data$b = dct(DCT2_example,inverted = T)
DCT2_example[2] = 2
DCT2_plot_data$c = dct(DCT2_example,inverted = T)
DCT2_example[2] = 10
DCT2_plot_data$d = dct(DCT2_example,inverted = T)
p2 = ggplot(DCT2_plot_data,aes(x=x))+
  geom_smooth(se=F,aes(y=base_track,color="Base"))+
  geom_smooth(se=F,aes(y=a,color="-3"))+
  geom_smooth(se=F,aes(y=b,color="0"))+
  geom_smooth(se=F,aes(y=c,color="2"))+
  geom_smooth(se=F,aes(y=d,color="10"))+
  labs(y="",x="Index",color="DCT2")+
  scale_y_continuous(limits = c(-4, 4))

base_track = dct(dct_example_vec,inverted = T)
DCT3_plot_data = data.frame(x=1:10,base=base_track)
DCT3_example = dct_example_vec
DCT3_example[3] = -3
DCT3_plot_data$a = dct(DCT3_example,inverted = T)
DCT3_example[3] = 0
DCT3_plot_data$b = dct(DCT3_example,inverted = T)
DCT3_example[3] = 2
DCT3_plot_data$c = dct(DCT3_example,inverted = T)
DCT3_example[3] = 10
DCT3_plot_data$d = dct(DCT3_example,inverted = T)
p3 = ggplot(DCT3_plot_data,aes(x=x))+
  geom_smooth(se=F,aes(y=base_track,color="Base"))+
  geom_smooth(se=F,aes(y=a,color="-3"))+
  geom_smooth(se=F,aes(y=b,color="0"))+
  geom_smooth(se=F,aes(y=c,color="2"))+
  geom_smooth(se=F,aes(y=d,color="10"))+
  labs(y="",x="Index",color="DCT3") +
  scale_y_continuous(limits = c(-4, 4))

DCT4_plot_data = data.frame(x=1:10,base=base_track)
DCT4_example = dct_example_vec
DCT4_example[4] = -3
DCT4_plot_data$a = dct(DCT4_example,inverted = T)
DCT4_example[4] = 0
DCT4_plot_data$b = dct(DCT4_example,inverted = T)
DCT4_example[4] = 2
DCT4_plot_data$c = dct(DCT4_example,inverted = T)
DCT4_example[4] = 10
DCT4_plot_data$d = dct(DCT4_example,inverted = T)
p4 = ggplot(DCT4_plot_data,aes(x=x,xmin))+
  geom_smooth(se=F,aes(y=base_track,color="Base"))+
  geom_smooth(se=F,aes(y=a,color="-3"))+
  geom_smooth(se=F,aes(y=b,color="0"))+
  geom_smooth(se=F,aes(y=c,color="2"))+
  geom_smooth(se=F,aes(y=d,color="10"))+
  labs(y="",x="Index",color="DCT4")+
  scale_y_continuous(limits = c(-4, 4))

grid.arrange(p1,p2,p3,p4)

###
# Length
###
data.dur = data.clean.sauce.demo %>%
  unite(id,site,last,first,sep="_",remove=FALSE) %>%
  filter(token %in% c("COT","CAUGHT","cot","caught")) %>%
  filter(index == 1) %>%
  filter(segment %in% c("AA1","AO1")) %>%
  filter(!site %in% c("RED","MER","RDL"))
data.dur$gender=recode(data.dur$gender,
                       male="male",
                       Male="male",
                       female="female",
                       Female="female",
                       .default=NA_character_)
data.dur = data.dur%>%drop_na(gender,birthyear)

data.dur$cseg = as.numeric(as.factor(data.dur$segment)) - mean(as.numeric(as.factor(data.dur$segment)))
data.dur$csite = as.numeric(as.factor(data.dur$site)) - mean(as.numeric(as.factor(data.dur$site)))
data.dur$cbirthyear = as.numeric(data.dur$birthyear) - mean(as.numeric(data.dur$birthyear))
data.dur$cgender = as.numeric(as.factor(data.dur$gender)) - mean(as.numeric(as.factor(data.dur$gender)))
data.dur$segment = as.factor(data.dur$segment)
contrasts(data.dur$segment) = rbind(-.5, .5)
data.dur$gender = as.factor(data.dur$gender)
contrasts(data.dur$gender) = rbind(-.5, .5)
data.dur$rsite = sample(data.dur$csite)
model.lmer.logdur = lmer(logdur~cseg*cgender*cbirthyear+(1|csite)+(1|id),data=data.dur)
model.lmer.logdur.bak = lmer(logdur~cseg*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="BAK"))
model.lmer.logdur.hum = lmer(logdur~cseg*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="HUM"))
model.lmer.logdur.sac = lmer(logdur~cseg*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="SAC"))
model.lmer.logdur.sal = lmer(logdur~cseg*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="SAL"))
summary(model.lmer.logdur.bak)

p1 = ggplot(data.dur,aes(x=birthyear,y=exp(logdur)*1000,color=segment))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="",y="Duration (ms)")
p2 = ggplot(data.dur,aes(x=birthyear,y=exp(logdur)*1000,color=segment))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Birth year",y="Duration (ms)")+
  facet_wrap(~site)
grid.arrange(p1,p2)
