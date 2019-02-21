library(tidyverse)
library(gss)

# Load Data
### Data locations
setwd('/home/cj/Desktop/Linguistics/QP2/')
paths <- c(
  bot = c(
    men = c(
      sac = './data/SAC_bot_men.csv',
      sal = './data/SAL_bot_men.csv'
    ),
    women = c(
      sac = './data/SAC_bot_women.csv',
      sal = './data/SAL_bot_women.csv'
    )
  ),
  bought = c(
    men = c(
      sac = './data/SAC_bought_men.csv',
      sal = './data/SAL_bought_men.csv'
    ),
    women = c(
      sac = './data/SAC_bought_women.csv',
      sal = './data/SAL_bought_women.csv'
    )
  )
)

### Functions
#### Reads all the locations in paths then calls recursive function to combine
load_data <- function(l) {
  dt = lapply(l,read_csv)
  dtbind = bind_rec(dt)
  return(dtbind)
}

#### Recursively binds datasets together
bind_rec <- function(var, i=1 , target=0) {
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


### Load the data using above function
data = load_data(paths)

# Models
ssmodel <- function (
    formula,
    data,
    n=50,
    vowels=c("AA1","AO1"),
    sites = c(
      "SAC",
      "SAL"
    )
  ) {
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

data$site = as.factor(data$site)
data$segment = as.factor(data$segment)
f1_model = ssmodel(f1_bark ~ index*segment*site*sex,data=data)

# Plot
plot_spline <- function(model,sites,ylab,xlab="Index",a=0.4) {
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
ggplot(data,aes(x=index,group=token,color=segment)) +
  geom_smooth(aes(y=f1_bark)) +
  geom_line(aes(y=f2_bark)) +
  geom_line(aes(y=f3_bark)) +
  facet_wrap(~site)

site_labels <- c(SAC="Sacramento",SAL="Salinas")
a = plot_spline(f1_model,site_labels,ylab="Predicted F1 (Bark)")
print(a)
