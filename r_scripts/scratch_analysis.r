model.dct.F1.simple = glm(token~(DCT1+DCT2+DCT3+DCT4)*(cbirthyear*gender*site)-gender-site,
                   data=dct_coeffs,
                   family="binomial")
summary(model.dct.F1.simple)

ggplot(dct_coeffs.plot%>%filter(abs(DCT2)<2500)%>%filter(DCT1<17500)%>%mutate(bgroup=birthyear>1968),
       aes(x=token,y=DCT1,color=gender))+
  geom_boxplot()
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~bgroup)

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

F1_dct_coeffs = get_dct(data.clean.sauce.demo,"F1")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site)%>%
  unite(id,site,last,first,remove = FALSE)
F2_dct_coeffs = get_dct(data.clean.sauce.demo,"F2")%>%
  filter(!site %in% c("MER","RED"))%>%
  drop_na(DCT1,DCT2,DCT3,DCT4,birthyear,site)%>%
  unite(id,site,last,first,remove = FALSE)
f1_dct_preds = dct_predictions(data.clean.sauce.demo,"F1",4)%>%mutate(age_bin = ntile(birthyear,3))%>%drop_na(age_bin)%>%group_by(age_bin)%>%mutate(cF1=F1-mean(F1,na.rm=T),cpF1=pF1-mean(F1,na.rm=T))
f2_dct_preds = dct_predictions(data.clean.sauce.demo,"F2",3)%>%mutate(age_bin = ntile(birthyear,3))%>%drop_na(age_bin)%>%group_by(age_bin)%>%mutate(cF2=F2-mean(F2,na.rm=T),cpF2=pF2-mean(F2,na.rm=T))

ggplot(f1_dct_preds%>%filter(!site %in% c("RED","MER"))) +
  geom_smooth(aes(x=index,y=cF1,color=token,linetype="Observed"),method = 'loess')+
  geom_smooth(aes(x=index,y=cpF1,color=token,linetype="Predicted"),method = 'loess',se=F)+
  labs(x="Index", y="F1 (Hz)") +
  facet_wrap(~age_bin)
ggplot(f2_dct_preds%>%filter(!site %in% c("RED","MER"))) +
  geom_smooth(aes(x=index,y=cF2,color=token,linetype="Observed"),method = 'loess')+
  geom_smooth(aes(x=index,y=cpF2,color=token,linetype="Predicted"),method = 'loess',se=F)+
  labs(x="Index", y="F2 (Hz)") +
  facet_wrap(~age_bin)

data.dct_dist = F1_dct_coeffs%>%
       select(-DCT5,-DCT6,-DCT7,-DCT8,-DCT9,-DCT10)%>%
       unite(temp,DCT1,DCT2,DCT3,DCT4)%>%
       spread(token,temp)%>%
       separate(caught,c("f1.caught.DCT1","f1.caught.DCT2","f1.caught.DCT3","f1.caught.DCT4"),sep="_",convert=T)%>%
       separate(cot,c("f1.cot.DCT1","f1.cot.DCT2","f1.cot.DCT3","f1.cot.DCT4"),sep="_",convert=T)#%>%
       #mutate(dct_dist = sqrt(((f1.caught.DCT2-f1.cot.DCT2)**2 + (f1.caught.DCT3-f1.cot.DCT3)**2 + (f1.caught.DCT4-f1.cot.DCT4)**2)))
data.dct_dist.2 = F2_dct_coeffs%>%
  select(-DCT5,-DCT6,-DCT7,-DCT8,-DCT9,-DCT10)%>%
  unite(temp,DCT1,DCT2,DCT3,DCT4)%>%
  spread(token,temp)%>%
  separate(caught,c("f2.caught.DCT1","f2.caught.DCT2","f2.caught.DCT3","f2.caught.DCT4"),sep="_",convert=T)%>%
  separate(cot,c("f2.cot.DCT1","f2.cot.DCT2","f2.cot.DCT3","f2.cot.DCT4"),sep="_",convert=T)#%>%
  #mutate(dct_dist = sqrt(((f1.caught.DCT2-f1.cot.DCT2)**2 + (f1.caught.DCT3-f1.cot.DCT3)**2 + (f1.caught.DCT4-f1.cot.DCT4)**2)))
  
data.dct_dist = full_join(data.dct_dist,data.dct_dist.2)%>%
  mutate(dct_dist = sqrt(((f1.caught.DCT2-f1.cot.DCT2)**2 + 
                            (f1.caught.DCT3-f1.cot.DCT3)**2 + 
                            (f1.caught.DCT4-f1.cot.DCT4)**2 +
                            (f2.caught.DCT2-f2.cot.DCT2)**2 +
                            (f2.caught.DCT3-f2.cot.DCT3)**2)))

data.dct_dist = data.dct_dist%>%
       filter(dct_dist < mean(data.dct_dist$dct_dist,na.rm=T)+sd(data.dct_dist$dct_dist,na.rm=T)*2)%>%
       drop_na(gender,birthyear,dct_dist)
     
ggplot(data.dct_dist,aes(x=cbirthyear,y=dct_dist,color=gender))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~site)

data.dct_dist$cgender = as.numeric(as.factor(data.dct_dist$gender)) - mean(as.numeric(as.factor(data.dct_dist$gender)))
data.dct_dist$cbirthyear = as.numeric(data.dct_dist$birthyear) - mean(as.numeric(data.dct_dist$birthyear))
foo = lm(dct_dist~gender*cbirthyear-gender,data=data.dct_dist%>%filter(site=="RDL"))
summary(foo)

F1_dct_coeffs=F1_dct_coeffs%>%
  mutate(dct_dist = sqrt())
ggplot(F1_dct_coeffs%>%filter(site=="SAL"),aes(x=birthyear))+
  #geom_point(aes(y=DCT1,color="DCT1"))+
  geom_point(aes(y=DCT2,color="DCT2"))+
  geom_smooth(aes(y=DCT2,color="DCT2"),method="lm")+
  geom_point(aes(y=DCT3,color="DCT3"))+
  geom_smooth(aes(y=DCT3,color="DCT3"),method="lm")+
  geom_point(aes(y=DCT4,color="DCT4"))+
  geom_smooth(aes(y=DCT4,color="DCT4"),method="lm")+
  #geom_point(aes(y=DCT3,color="DCT3"))+
  #geom_point(aes(y=DCT4,color="DCT4"))+
  facet_wrap(~token)

model.ed.bak = lm(ed~cgender*cby,data=data.f1f2%>%filter(site=="RDL")%>%mutate(cby=cbirthyear-max(cbirthyear)))
summary(model.ed.bak)

bar=data.sauce.demo%>%filter(var1=="RDL")%>%
  group_by(var2,var3,Label) %>%
  mutate(
    speaker_id=paste(var1,var2,var3,sep="_"),
    segment=.get_class(Label,tolower(var6)))%>%
  filter(tolower(var6)%in%c("caught"))%>%
  select(segment)
  
  ungroup()%>%
           group_by(speaker_id,segment)%>%
           mutate(context=var6,
                  F1=mean(F1,na.rm=T),
                  F2=mean(F2,na.rm=T),
                  F3=mean(F3),
                  F1_glide=as.numeric(NA),
                  F2_glide=as.numeric(NA),
                  F3_glide=as.numeric(NA))%>%
  ungroup()%>%
  select(speaker_id,segment,context,F1,F2,F3,F1_glide,F2_glide,F3_glide) %>%
  distinct()
dt_frame = as.data.frame(bar)
nrm = norm.nearey(dt_frame) %>% rename(nF1="F*1",nF2="F*2")%>%separate(Speaker,c("site","foo","bar"),sep="_")
table(nrm$site)

ggplot(data.clean.normed.demo%>%
         ungroup()%>%
         group_by(Vowel)%>%
         mutate(mF1=mean(nF1,na.rm=T),mF2=mean(nF2,na.rm=T),sdF1=sd(nF1,na.rm=T),sdF2=sd(nF2,na.rm=T))%>%
         mutate(zF1=(nF1-mF1)/sdF1,zF2=(nF2-mF2)/sdF2)%>%
         mutate(oF1=abs(zF1)>2,oF2=abs(zF2)>2)%>%
         mutate(outlier=oF1&oF2),aes(x=-nF2,y=-nF1,color=outlier))+
  geom_point()+
  facet_wrap(~Vowel)

ggplot(data.f1f2,aes(x=birthyear,y=(ed),color=gender))+
  geom_point()+
  geom_smooth(method="lm")+
  facet_wrap(~site)

ggplot(data.dur,aes(x=site,y=exp(logdur)*1000,color=outlier))+
  geom_point(position="jitter")

ggplot(data.dct_dist,aes(color=outlier))+
  geom_point(position="jitter",aes(x=site,y=dct_dist))

ggplot(data.clean.normed.demo%>%
         ungroup()%>%
         group_by(Vowel)%>%
         summarize(mF1=mean(nF1,na.rm=T),mF2=mean(nF2,na.rm=T)),aes(x=mF2,y=mF1,label=Vowel))+
  #geom_point()+
  geom_text()+
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") 
