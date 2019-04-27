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
