# Create a smoothing spline formant model.
ssmodel <- function (
  formula,
  cleansauce,
  n=10,
  vowels=c("AA1","AO1"),
  sites = c(
    "SAC",
    "SAL"
  )) {
  require(gss)
  i = seq(1,n)
  vowels = unique(cleansauce$segment)
  sites = unique(cleansauce$site)
  v = as.factor(vowels)
  loc = as.factor(sites)
  cleansauce$segment=as.factor(cleansauce$segment)
  cleansauce$site=as.factor(cleansauce$site)
  cleansauce$index=as.numeric(cleansauce$index)
  #sex = c(1,2)
  model = ssanova(formula,data=cleansauce)
  model.predicted = expand.grid(index=i,segment=v,site=loc)#,sex=sex)
  model.predicted$Fit <- predict(model,newdata=model.predicted, se = T)$fit
  model.predicted$SE <- predict(model, newdata = model.predicted, se = T)$se.fit
  return(list("model" = model, "fit" = model.predicted))
}

# Plot a model made using ssmodel.
plot_spline <- function(model,sites,ylab,xlab="Index",a=0.4) {
  require(ggplot2)
  sexes=c("1"="Men","2"="Women")
  segment_labs = c(AA1="LOT",AO1="THOUGHT")
  p = ggplot() +
    geom_line(data=model$fit,
              aes(x=index,y=Fit,color=segment)) +
    geom_ribbon(data=model$fit,
                aes(x=index,ymin = Fit-SE, ymax = Fit+SE,group=segment),alpha=a) +
    labs(y=ylab,x=xlab,col="Vowel")+
    facet_wrap(~sex*site,labeller=labeller(site=sites,sex=sexes,segment=segment_labs))
}

plot_vangles <- function(vangle,n=5) {
  a = vangle%>%
    distinct(Speaker,pos,.keep_all = TRUE)%>%
    group_by(Speaker) %>%
    mutate(cF1=nF1-.recenter(nF1,nF2,pos)[2],cF2=nF2-.recenter(nF1,nF2,pos)[1])
  spk = sample(unique(a$Speaker),n)
  a = a %>%
    filter(Speaker %in% spk)
  ggplot(a%>%arrange(desc(pos)),aes(x=cF2,y=cF1))+
    geom_point()+
    geom_line(aes(color=Speaker)) +
    #geom_text(aes(label=Vowel)) +
    scale_y_reverse(position = "right") + 
    scale_x_reverse(position = "top")
}

.vs_testu <- function(f1vec,f2vec,posvec,i) {
  uv = c(-1,0,'unit')
  return(as.numeric(uv[i]))
}

.vs_test2 <- function(f1vec,f2vec,posvec,i) {
  li = which(posvec=="low")
  hi = which(posvec=="high")
  anchor = c(f2vec[li],f1vec[li])
  anchor2 = c(f2vec[hi],f1vec[hi])
  a2 = anchor2 - anchor
  a2 = c(a2,'a2')
  return(as.numeric(a2[i]))
}

.vs_testt <- function(f1vec,f2vec,posvec,i) {
  li = which(posvec=="low")
  hi = which(posvec=="high")
  ti = setdiff(c(1,2,3),c(li,hi))
  anchor = c(f2vec[li],f1vec[li])
  target = c(f2vec[ti],f1vec[ti])
  t = target - anchor
  t = c(t,'target')
  return(as.numeric(t[i]))
}
