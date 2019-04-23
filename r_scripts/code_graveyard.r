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
grandF1mean = mean(data.clean.sauce.demo$F1)
grandF2mean = mean(data.clean.sauce.demo$F2)
data.clean.normed.demo = data.clean.normed.demo %>%
  mutate(hF1=nF1*grandF1mean,hF2=nF2*grandF2m)

ssdata = data.clean.sauce.demo %>%
  filter(segment %in% c("AO1","AA1")) %>%
  filter(!site %in% c("RED","MER"))

m = ssmodel(F1~index*segment*site,ssdata)
m2 = ssmodel(F2~index*segment*site,ssdata)
m3 = ssmodel(F3~index*segment*site,ssdata)

x = left_join(m$fit,m2$fit)
ggplot(x) +
  geom_pointrange(aes(x=Fit2,y=Fit,ymin=Fit-SE,ymax=Fit+SE,color=segment)) +
  geom_errorbarh(aes(x=Fit2,y=Fit,xmin=Fit2-SE2,xmax=Fit2+SE2,color=segment)) +
  facet_wrap(~site)

i = seq(1,10)
vowels = unique(ssdata$segment)
sites = unique(ssdata$site)
genders = unique(ssdata$gender)
v = as.factor(vowels)
loc = as.factor(sites)
gen = as.factor(genders)
ssdata$segment=as.factor(ssdata$segment)
ssdata$site=as.factor(ssdata$site)
ssdata$index=as.numeric(ssdata$index)
ssdata$gender=as.factor(ssdata$gender)
model = ssanova(F1~index*segment*site*gender,data=ssdata)
model.predicted = expand.grid(index=i,segment=v,site=loc,gender=gen)
model.predicted$Fit <- predict(model,newdata=model.predicted, se = T)$fit
model.predicted$SE <- predict(model, newdata = model.predicted, se = T)$se.fit