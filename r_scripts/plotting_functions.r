# Create a smoothing spline formant model.
ssmodel <- function (
  formula,
  data,
  n=50,
  vowels=c("AA1","AO1"),
  sites = c(
    "SAC",
    "SAL"
  )) {
  require(gss)
  i = seq(1,n)
  v = as.factor(vowels)
  sex = c(1,2)
  loc = sites
  model = ssanova(formula,data=data)
  model.predicted = expand.grid(index=i,segment=v,sex=sex,site=loc)
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
