table(data.dur$race)

data.dur$race_recode=recode(data.dur$race,
                        "African-American"="black",
                        "African American"="black",
                        "Black"="black",
                        "White"="white",
                        "Caucasian"="white",
                        "Croation"="white",
                        "Yugoslavian"="white",
                        .default=NA_character_)
ggplot(data.dur%>%drop_na(race_recode),aes(x=birthyear,y=logdur,color=race_recode,linetype=token))+
  geom_point()+
  geom_smooth(method="lm")

data.f1f2.bw = data.f1f2%>%drop_na(race_recode)
data.f1f2.bw$crace = as.numeric(as.factor(data.f1f2.bw$race_recode)) - mean(as.numeric(as.factor(data.f1f2.bw$race_recode)))
model.ed.race = lm(log(ed)~cgender*cbirthyear*crace,data=data.f1f2.bw)
