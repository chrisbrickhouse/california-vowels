setwd('/home/cj/Desktop/Linguistics/QP2/')
library(tidyverse)
library(vowels)
library(gss)
library(lme4)
library(lmerTest)
library(dtt)
library(gridExtra)
source('./r_scripts/analysis_functions.r')


# Load Data
data.sauce.demo = load_sauce('./data/full_data_with_demo.csv')
data.sauce.demo$var6 = tolower(data.sauce.demo$var6)
data.normed = make_norm(data.sauce.demo)
data.clean.sauce.demo = clean_sauce(data.sauce.demo,T)
data.clean.normed.demo = merge_demo(data.normed,data.clean.sauce.demo)
excluded.always = read_csv("./data/exclusions.csv")$id

# F1-F2 analysis
data.f1f2 = data.clean.normed.demo %>%
  ungroup() %>%
  filter(tolower(Context) %in% c("cot","caught")) %>%
  select(Speaker,site,Context,nF1,nF2,gender,birthyear,race,sexual_orientation,education,town_orientation,politics) %>%
  unite(Fs,nF1,nF2,sep="_") %>%
  spread(Context,Fs) %>%
  separate(cot,c("nF1_bot","nF2_bot"),sep="_",convert=TRUE) %>%
  separate(caught,c("nF1_bought","nF2_bought"),sep="_",convert=TRUE) %>%
  mutate(F1diff=nF1_bought-nF1_bot,F2diff=nF2_bought-nF2_bot)%>%
  drop_na(gender,birthyear)%>%
  filter(!site %in% c("MER","RED"))%>%
  mutate(ed=sqrt(F1diff**2 + F2diff**2))

data.f1f2$cbirthyear = as.numeric(data.f1f2$birthyear) - mean(as.numeric(data.f1f2$birthyear))
data.f1f2$cgender = as.numeric(as.factor(data.f1f2$gender)) - mean(as.numeric(as.factor(data.f1f2$gender)))

model.t.F1 = t.test(data.f1f2$F1diff)
model.t.F2 = t.test(data.f1f2$F2diff)


data.f1f2 = data.clean.normed.demo%>%
  ungroup()%>%
  filter(!Speaker %in% excluded.always)%>%
  group_by(Vowel)%>%
  mutate(mF1=mean(nF1,na.rm=T),mF2=mean(nF2,na.rm=T),sdF1=sd(nF1,na.rm=T),sdF2=sd(nF2,na.rm=T))%>%
  mutate(zF1=(nF1-mF1)/sdF1,zF2=(nF2-mF2)/sdF2)%>%
  mutate(oF1=abs(zF1)>2,oF2=abs(zF2)>2)%>%
  mutate(outlier=oF1&oF2)%>%
  ungroup() %>%
  filter(tolower(Context) %in% c("cot","caught"))

data.f1f2$gender=recode(data.f1f2$gender,
               male="male",
               Male="male",
               female="female",
               Female="female",
               .default=NA_character_)

excluded.f1f2 = data.f1f2%>%
  filter(outlier==T)

data.f1f2 = data.f1f2%>%
  filter(outlier==F)%>%
  select(Speaker,site,Context,nF1,nF2,gender,birthyear,race,sexual_orientation,education,town_orientation,politics) %>%
  unite(Fs,nF1,nF2,sep="_") %>%
  spread(Context,Fs) %>%
  separate(cot,c("nF1_bot","nF2_bot"),sep="_",convert=TRUE) %>%
  separate(caught,c("nF1_bought","nF2_bought"),sep="_",convert=TRUE) %>%
  mutate(F1diff=nF1_bought-nF1_bot,F2diff=nF2_bought-nF2_bot)%>%
  drop_na(gender,birthyear)%>%
  filter(!site %in% c("MER","RED"))%>%
  mutate(ed=sqrt(F1diff**2 + F2diff**2))
  

data.f1f2$cbirthyear = as.numeric(data.f1f2$birthyear) - mean(as.numeric(data.f1f2$birthyear))
data.f1f2$cgender = as.numeric(as.factor(data.f1f2$gender)) - mean(as.numeric(as.factor(data.f1f2$gender)))

model.t.F1 = t.test(data.f1f2$F1diff)
model.t.F1
model.t.F2 = t.test(data.f1f2$F2diff)
model.t.F2

model.ed = lmer(log(ed)~cgender*cbirthyear+(1|site),data=data.f1f2)
model.ed.rdl = lm(log(ed)~cgender*cbirthyear,data=data.f1f2%>%filter(site=="RDL"))
summary(model.ed)
summary(model.ed.rdl)
table(data.f1f2$site)
nrow(data.f1f2)

#ggplot(data.f1f2,aes(x=-F2diff,y=-F1diff))+
#  geom_point()+
#  geom_density2d()+
#  facet_wrap(~site)

ggplot(data.f1f2,aes(y=ed,x=birthyear,color=gender))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(y="Euclidean distance",x="Birth year")+
  facet_wrap(~site)

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
  x = dct_predictions(data.clean.sauce.demo,"F2",n)%>%mutate(diff=pF2-F2)%>%select(diff)%>%drop_na()
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
# DCT Euclidean Distance
###
F1_dct_coeffs = get_dct(data.clean.sauce.demo,"F1")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site)%>%
  unite(id,site,last,first,remove = FALSE)
F2_dct_coeffs = get_dct(data.clean.sauce.demo,"F2")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site)%>%
  unite(id,site,last,first,remove = FALSE)

data.dct_dist.temp1 = F1_dct_coeffs%>%
  select(-DCT5,-DCT6,-DCT7,-DCT8,-DCT9,-DCT10)%>%
  ungroup()%>%
  mutate(m2=mean(DCT2,na.rm=T),m3=mean(DCT3,na.rm=T),m4=mean(DCT4,na.rm=T))%>%
  mutate(s2=sd(DCT2,na.rm=T),s3=sd(DCT3,na.rm=T),s4=sd(DCT4,na.rm=T))%>%
  mutate(z2=(DCT2-m2)/s2,z3=(DCT3-m3)/s3,z4=(DCT4-m4)/s4)%>%
  mutate(o2=abs(z2)>2,o3=abs(z3)>2,o4=abs(z4)>2)%>%
  mutate(outlier=o2|o3|o4)%>%
  select(-m2,-m3,-m4,-s2,-s3,-s4,-z2,-z3,-z4,-o2,-o3,-o4)%>%
  unite(temp,DCT1,DCT2,DCT3,DCT4,outlier)%>%
  spread(token,temp)%>%
  separate(caught,c("f1.caught.DCT1","f1.caught.DCT2","f1.caught.DCT3","f1.caught.DCT4","f1.caught.outlier"),sep="_",convert=T)%>%
  separate(cot,c("f1.cot.DCT1","f1.cot.DCT2","f1.cot.DCT3","f1.cot.DCT4","f1.cot.outlier"),sep="_",convert=T)
data.dct_dist.temp2 = F2_dct_coeffs%>%
  select(-DCT5,-DCT6,-DCT7,-DCT8,-DCT9,-DCT10)%>%
  ungroup()%>%
  mutate(m2=mean(DCT2,na.rm=T),m3=mean(DCT3,na.rm=T),m4=mean(DCT4,na.rm=T))%>%
  mutate(s2=sd(DCT2,na.rm=T),s3=sd(DCT3,na.rm=T),s4=sd(DCT4,na.rm=T))%>%
  mutate(z2=(DCT2-m2)/s2,z3=(DCT3-m3)/s3,z4=(DCT4-m4)/s4)%>%
  mutate(o2=abs(z2)>2,o3=abs(z3)>2,o4=abs(z4)>2)%>%
  mutate(outlier=o2|o3)%>%
  select(-m2,-m3,-m4,-s2,-s3,-s4,-z2,-z3,-z4,-o2,-o3,-o4)%>%
  unite(temp,DCT1,DCT2,DCT3,DCT4,outlier)%>%
  spread(token,temp)%>%
  separate(caught,c("f2.caught.DCT1","f2.caught.DCT2","f2.caught.DCT3","f2.caught.DCT4","f2.caught.outlier"),sep="_",convert=T)%>%
  separate(cot,c("f2.cot.DCT1","f2.cot.DCT2","f2.cot.DCT3","f2.cot.DCT4","f2.cot.outlier"),sep="_",convert=T)

data.dct_dist = full_join(data.dct_dist.temp1,data.dct_dist.temp2)%>%
  mutate(caught.outlier=f1.caught.outlier&f2.caught.outlier,cot.outlier=f1.cot.outlier&f2.cot.outlier)%>%
  mutate(outlier=caught.outlier|cot.outlier)%>%
  mutate(dct_dist = sqrt(((f1.caught.DCT2-f1.cot.DCT2)**2 + 
                            (f1.caught.DCT3-f1.cot.DCT3)**2 + 
                            (f1.caught.DCT4-f1.cot.DCT4)**2 +
                            (f2.caught.DCT2-f2.cot.DCT2)**2 +
                            (f2.caught.DCT3-f2.cot.DCT3)**2)))%>%
  filter(!id %in% excluded.always)

excluded.dct_dist = data.dct_dist %>%
  filter(outlier==T)

data.dct_dist = data.dct_dist%>%
  ungroup()%>%
  group_by(site)%>%
  mutate(m=mean(log(dct_dist),na.rm=T))%>%
  ungroup()%>%
  mutate(s=sd(log(dct_dist),na.rm=T))%>%
  mutate(z=(log(dct_dist)-m)/s)%>%
  filter(abs(z) < 2)%>%
  drop_na(gender,birthyear,dct_dist)
table(data.dct_dist$site)
nrow(data.dct_dist)

ggplot(data.dct_dist,aes(x=birthyear,y=dct_dist,color=gender))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~site) +
  labs(x="Birth year",y="Distance in DCT space")

data.dct_dist$cgender = as.numeric(as.factor(data.dct_dist$gender)) - mean(as.numeric(as.factor(data.dct_dist$gender)))
data.dct_dist$cbirthyear = as.numeric(data.dct_dist$birthyear) - mean(as.numeric(data.dct_dist$birthyear))
model.dct_dist = glm(dct_dist~cgender*cbirthyear,data=data.dct_dist,family=gaussian(link=log))
summary(model.dct_dist)

f1_dct_preds = dct_predictions(data.clean.sauce.demo,"F1",4)%>%mutate(age_bin = ntile(birthyear,3))%>%drop_na(age_bin)%>%group_by(age_bin)%>%mutate(cF1=F1-mean(F1,na.rm=T),cpF1=pF1-mean(F1,na.rm=T))
f2_dct_preds = dct_predictions(data.clean.sauce.demo,"F2",3)%>%mutate(age_bin = ntile(birthyear,3))%>%drop_na(age_bin)%>%group_by(age_bin)%>%mutate(cF2=F2-mean(F2,na.rm=T),cpF2=pF2-mean(F2,na.rm=T))

p1=ggplot(f1_dct_preds%>%filter(!site %in% c("RED","MER"))) +
  geom_smooth(aes(x=index,y=cF1,color=token,linetype="Observed"),method = 'loess')+
  geom_smooth(aes(x=index,y=cpF1,color=token,linetype="Predicted"),method = 'loess',se=F)+
  labs(x="Index", y="F1 (Hz)") +
  facet_wrap(~age_bin)
p2=ggplot(f2_dct_preds%>%filter(!site %in% c("RED","MER"))) +
  geom_smooth(aes(x=index,y=cF2,color=token,linetype="Observed"),method = 'loess')+
  geom_smooth(aes(x=index,y=cpF2,color=token,linetype="Predicted"),method = 'loess',se=F)+
  labs(x="Index", y="F2 (Hz)") +
  facet_wrap(~age_bin)
grid.arrange(p1,p2)
>>>>>>> 9d63f1d

###
# Length
###
data.dur = data.clean.sauce.demo %>%
  unite(id,site,last,first,sep="_",remove=FALSE) %>%
  filter(token %in% c("COT","CAUGHT","cot","caught")) %>%
  filter(index == 1) %>%
  filter(segment%in%c("AA1","AO1"))%>%
  filter(!site %in% c("RED","MER"))

data.dur$gender=recode(data.dur$gender,
                       male="male",
                       Male="male",
                       female="female",
                       Female="female",
                       .default=NA_character_)

data.dur = data.dur%>%drop_na(gender,birthyear)%>%
  ungroup()%>%
  group_by(site)%>%
  ungroup()%>%
  mutate(m=mean(logdur,na.rm=T))%>%
  mutate(s=sd(logdur),na.rm=T)%>%
  mutate(z=(logdur-m)/s)%>%
  mutate(outlier=abs(z)>2)%>%
  mutate(other_outlier=id%in%excluded.f1f2$Speaker | id%in%excluded.dct_dist$id)%>%
  mutate(total_outlier=other_outlier&outlier)

excluded.dur = data.dur %>%
  filter(outlier==T)%>%
  arrange(id)

excluded.dur$corrected = c(
  0.155387,
  NA,
  0.078510,
  0.257185,
  0.124509,
  0.073045,
  0.104034,
  NA,
  NA,
  NA,
  0.208089,
  0.186616,
  0.174209,
  0.120000,
  0.145095,
  0.201231,
  0.314270,
  0.205322,
  0.095498,
  0.149909,
  NA,
  NA,
  0.173685,
  NA,
  NA,
  NA,
  0.195119,
  NA,
  0.080654,
  0.100486,
  0.163688,
  0.111809,
  NA,
  0.254704,
  0.355992,
  0.175143,
  0.064890
)

data.dur = excluded.dur%>%
  drop_na(corrected)%>%
  mutate(dur=corrected,logdur=log(corrected))%>%
  select(-corrected)%>%
  bind_rows(data.dur%>%filter(outlier==F))

excluded.dur= excluded.dur%>%filter(is.na(corrected))

table(distinct(data.dur,id,.keep_all=T)$site)
nrow(distinct(data.dur,id,.keep_all=T))
table(table(data.dur$id)!=2)

data.dur$ctok = as.numeric(as.factor(data.dur$token)) - mean(as.numeric(as.factor(data.dur$token)))
data.dur$cseg = as.numeric(as.factor(data.dur$segment)) - mean(as.numeric(as.factor(data.dur$segment)))
data.dur$csite = as.numeric(as.factor(data.dur$site)) - mean(as.numeric(as.factor(data.dur$site)))
data.dur$cbirthyear = as.numeric(data.dur$birthyear) - mean(as.numeric(data.dur$birthyear))
model.lmer.logdur = lmer(logdur~ctok*cgender*cbirthyear+(1|csite)+(1|id),data=data.dur)
model.lmer.logdur.noRDL = lmer(logdur~ctok*cgender*cbirthyear+(1|csite)+(1|id),data=data.dur%>%filter(site!="RDL"))
model.lmer.logdur.bak = lmer(logdur~ctok*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="BAK"))
model.lmer.logdur.hum = lmer(logdur~ctok*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="HUM"))
model.lmer.logdur.sac = lmer(logdur~ctok*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="SAC"))
model.lmer.logdur.sal = lmer(logdur~ctok*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="SAL"))
model.lmer.logdur.rdl = lmer(logdur~ctok*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="RDL"))
summary(model.lmer.logdur)
summary(model.lmer.logdur.noRDL)
summary(model.lmer.logdur.rdl)

p1 = ggplot(data.dur,aes(x=birthyear,y=exp(logdur)*1000,color=token))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="",y="Duration (ms)")
p2 = ggplot(data.dur,aes(x=birthyear,y=exp(logdur)*1000,color=token))+
  geom_point()+
  geom_smooth(method="lm")+
  labs(x="Birth year",y="Duration (ms)")+
  facet_wrap(~site)
grid.arrange(p1,p2)

#ggplot(data.dur%>%filter(site=="SAC"),aes(x=birthyear,y=exp(logdur)*1000,color=gender))+
#  geom_point()+
#  geom_smooth(method="lm")+
#  labs(x="",y="Duration (ms)")+
#  facet_wrap(~segment)
