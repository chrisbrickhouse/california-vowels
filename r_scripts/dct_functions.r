dct_predictions <- function(sauce, stat, n=5,filter_val=0.0) {
  dct_predicted = sauce %>%
    filter(F1>0)%>%
    filter(F2>0)%>%
    select(site,first,last,token,index,`stat`)%>%
    mutate(token=tolower(token))%>%
    filter(token %in% c("cot","caught"))%>%
    spread(index,`stat`) %>%
    rowwise()%>%
    mutate(f = .dct_fit(c(`1`,`2`,`3`,`4`,`5`,`6`,`7`,`8`,`9`,`10`),n=n))%>%
    separate(f,c("fit1","fit2","fit3","fit4","fit5","fit6","fit7","fit8","fit9","fit10"),sep="_") %>%
    mutate(
      "1"=paste(`1`,fit1,sep="_"),
      "2"=paste(`2`,fit2,sep="_"),
      "3"=paste(`3`,fit3,sep="_"),
      "4"=paste(`4`,fit4,sep="_"),
      "5"=paste(`5`,fit5,sep="_"),
      "6"=paste(`6`,fit6,sep="_"),
      "7"=paste(`7`,fit7,sep="_"),
      "8"=paste(`8`,fit8,sep="_"),
      "9"=paste(`9`,fit9,sep="_"),
      "10"=paste(`10`,fit10,sep="_"),
    )%>%
    gather("index","val",5:14,convert=TRUE) %>%
    select(site,first,last,token,index,val) %>%
    separate(val,c(`stat`,paste(c("p",`stat`),collapse="")),sep="_",convert = TRUE) %>%
    unite(id,site,last,first,sep="_",remove = FALSE)
  demo = .get_demo(sauce)
  dct_data = left_join(dct_predicted,demo)
  return(dct_data)
}

get_dct <- function(sauce) {
  dct_data = sauce %>%
    mutate(token=tolower(token))%>%
    filter(token %in% c("cot","caught")) %>%
    group_by(site,last,first,token) %>%
    summarize(
      DCT1=dct(F1)[1],
      DCT2=dct(F1)[2],
      DCT3=dct(F1)[3],
      DCT4=dct(F1)[4],
      DCT5=dct(F1)[5],
      DCT6=dct(F1)[6],
      DCT7=dct(F1)[7],
      DCT8=dct(F1)[8],
      DCT9=dct(F1)[9],
      DCT10=dct(F1)[10]
    )
  demo = .get_demo(sauce)
  dct_data = left_join(dct_data,demo)
  return(dct_data)
}

.get_demo <- function(sauce) {
  demo = sauce %>%
    select(
      site,
      last,
      first,
      gender,
      birthyear,
      race,
      sexual_orientation,
      education,
      town_orientation,
      politics
    ) %>%
    distinct()
  return(demo)
}

.dct_fit <- function(s,n=5) {
  a = dct(s)
  a = as.numeric(c(a[1:n],rep(0,10-n)))
  y = dct(a,inverted=TRUE)
  return(paste(y, collapse = "_"))
}

#ggplot(f2_dct_preds) +
#  geom_smooth(aes(x=index,y=F2,color=token,linetype="Observed"))+
#  geom_smooth(aes(x=index,y=pF2,color=token,linetype="Predicted"))+
#  facet_wrap(~site)

xx = apply()

