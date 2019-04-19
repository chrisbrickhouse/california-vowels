errlist = data.frame(index=NA,correlation=NA)
n=10
x = dct_predictions(data.clean.sauce.demo,"F1",n)%>%mutate(diff=pF1-F1)%>%select(diff)%>%drop_na()
sse = sum(x**2)
errlist = rbind(errlist,c(n,sse))
errlist = errlist%>%drop_na()
p1 = ggplot(errlist,aes(x=index,y=correlation))+
  geom_point()+
  geom_line()+
  labs(x="",y="Sum of Squared Errors")
plot(errlist);lines(errlist)

secondlist = data.frame(index=NA,correlation=NA)
i=9
x = errlist$correlation[i+1] + errlist$correlation[i-1] - 2 * errlist$correlation[i]
secondlist = rbind(secondlist,c(i,x))
p2 = ggplot(secondlist,aes(x=index,y=abs(correlation)))+
  geom_point()+
  geom_line()+
  labs(x="Number of DCT coefficients",y="Estimated Second Derivative")+
  xlim(c(1,10))
library(gridExtra)
grid.arrange(p1,p2)
#F2 n = 3

center <- function(x) {
  x = as.numeric(x)
  m = mean(x)
  o = sd(x)
  x = (x-m)/o
  return(x)
}

dct_coeffs = get_dct(data.clean.sauce.demo,"F1")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site,gender)%>%
  unite(id,site,last,first,remove = FALSE)
dct_coeffs$token = as.numeric(as.factor(dct_coeffs$token))-1
dct_coeffs$cDCT1 = center(dct_coeffs$DCT1)
dct_coeffs$cDCT2 = center(dct_coeffs$DCT2)
dct_coeffs$cDCT3 = center(dct_coeffs$DCT3)
dct_coeffs$cDCT4 = center(dct_coeffs$DCT4)
dct_coeffs$cbirthyear = center(dct_coeffs$birthyear)
dct_coeffs$csite = as.numeric(as.factor(dct_coeffs$site)) - mean(as.numeric(as.factor(dct_coeffs$site)))
dct_coeffs$cgender = as.numeric(as.factor(dct_coeffs$gender)) - mean(as.numeric(as.factor(dct_coeffs$gender)))
model.dct.F1 = glm(token~cDCT1*cDCT2*cDCT3*cDCT4,
                   data=dct_coeffs,
                   family="binomial")
summary(model.dct.F1)
model.dct.F1.simple = glm(token~(DCT1+DCT2+DCT3+DCT4)*(cbirthyear*gender*site)-gender-site,
                   data=dct_coeffs,
                   family="binomial")
summary(model.dct.F1.simple)

dct_coeffs = get_dct(data.clean.sauce.demo,"F2")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site,gender)%>%
  unite(id,site,last,first,remove = FALSE)
dct_coeffs.plot=dct_coeffs
dct_coeffs$token = as.numeric(as.factor(dct_coeffs$token))-1
dct_coeffs$cDCT1 = center(dct_coeffs$DCT1)
dct_coeffs$cDCT2 = center(dct_coeffs$DCT2)
dct_coeffs$cDCT3 = center(dct_coeffs$DCT3)
dct_coeffs$cDCT4 = center(dct_coeffs$DCT4)
dct_coeffs$cbirthyear = center(dct_coeffs$birthyear)
dct_coeffs$csite = as.numeric(as.factor(dct_coeffs$site)) - mean(as.numeric(as.factor(dct_coeffs$site)))
dct_coeffs$cgender = as.numeric(as.factor(dct_coeffs$gender)) - mean(as.numeric(as.factor(dct_coeffs$gender)))
dct_coeffs$rDCT1 = sample(dct_coeffs$DCT1)
dct_coeffs$rDCT2 = sample(dct_coeffs$DCT2)
dct_coeffs$rDCT3 = sample(dct_coeffs$DCT3)
dct_coeffs$rDCT4 = sample(dct_coeffs$DCT4)
dct_coeffs$rbirthyear = sample(dct_coeffs$cbirthyear)
dct_coeffs$rsite = sample(dct_coeffs$csite)
dct_coeffs$rgender = sample(dct_coeffs$cgender)
model.dct.F2 = glm(token~(DCT1+DCT2+DCT3)*(cbirthyear*cgender*csite),
                   data=dct_coeffs,
                   family="binomial")
summary(model.dct.F2)

ggplot(dct_coeffs.plot,aes(y=DCT2,x=birthyear,color=gender))+geom_point()+geom_smooth(method="lm")+facet_wrap(~token)

model.dct.F2.simple = glm(token~(DCT1+DCT2+DCT3)*(cbirthyear*gender*site)-gender-site,
                          data=dct_coeffs,
                          family="binomial")
summary(model.dct.F2.simple)

dct_coeffs = get_dct(data.clean.sauce.demo,"B3")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site)%>%
  unite(id,site,last,first,remove = FALSE)
dct_coeffs$token = as.numeric(as.factor(dct_coeffs$token))-1
dct_coeffs$DCT1 = center(dct_coeffs$DCT1)
dct_coeffs$DCT2 = center(dct_coeffs$DCT2)
dct_coeffs$DCT3 = center(dct_coeffs$DCT3)
dct_coeffs$DCT4 = center(dct_coeffs$DCT4)
dct_coeffs$birthyear = center(dct_coeffs$birthyear)
model.dct.B3 = glm(token~(DCT1+DCT2+DCT3+DCT4)*(birthyear*gender*site)-(birthyear*gender*site),
                   data=dct_coeffs,
                   family="binomial")
summary(model.dct.B3)

ggplot(dct_coeffs%>%drop_na(gender),aes(y=DCT3,x=site,fill=gender))+
  geom_boxplot()+
  facet_wrap(~token)

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

ggplot(data.dur%>%filter(site=="SAL"),aes(color=gender,y=exp(logdur),x=birthyear))+
  geom_point()+
  geom_smooth(method="lm")
  facet_wrap(~site)

data.dur$site = recode(data.dur$site,HUM="aHUM")
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
model.lmer.logdur.bak = lmer(logdur~cseg*cgender*cbirthyear+(1|id),data=data.dur%>%filter(site=="SAL"))
summary(model.lmer.logdur.bak)
model.lm.logdur = lm(logdur~segment*gender*cbirthyear,data=data.dur)
summary(model.lm.logdur)
s = lm(logdur~segment*gender*csite*cbirthyear,data=sdd)

sdd = data.dur
sdd$site = recode(data.dur$site,HUM="aHUM")
sdd$csite = as.numeric(as.factor(sdd$site)) - mean(as.numeric(as.factor(sdd$site)))
ggplot(sdd,aes(x=site,y=exp(logdur)*1000))+
  geom_boxplot()

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
