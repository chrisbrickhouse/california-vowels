# Load data when given a list of paths and return that data.
#   Calls the recursive bind function to bind them all together.
load_data <- function(l) {
  dt = lapply(l,read_csv)
  dtbind = bind_rec(dt)
  return(dtbind)
}

# Recursively binds datasets together.
bind_rec <- function(var, i=1 , target=0) {
  require(dplyr)
  if (i > length(var)) {
    return(target)
  } else if (target == 0) {
    target = var[1]
    i = 2
  }
  target = bind_rows(target,var[i])
  v = bind_rec(var,i+1,target)
  return(v)
}

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