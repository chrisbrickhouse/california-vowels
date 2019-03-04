# Data locations
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

load_sauce <- function(fname=FALSE) {
  if (fname != FALSE) {
    sauce = read_csv(fname)
  } else {
    a = read_csv('./data/all/spectral_measures.txt')
    b = read_csv('./data/all/spectral_measures_pt2.txt')
    c = read_csv('./data/all/spectral_measures_pt3_ascii.txt') # Files in this group were in utf-16 and needed rencoding
    d = read_csv('./data/all/spectral_measures_pt4.txt')
    e = read_csv('./data/all/spectral_measures_pt5.txt')
    f = read_csv('./data/all/spectral_measures_pt6.txt')
    g = read_csv('./data/all/spectral_measures_pt7_fixed.txt') # One file in this group didn't follow the naming convention
    sauce = bind_rows(a,bind_rows(b,bind_rows(c,bind_rows(d,bind_rows(e,bind_rows(f,g))))))
  }
  return(sauce)
}

# To Norm Data
make_norm <- function(sauce,rm.na=TRUE) {
  dt = sauce %>%
    group_by(var2,var3,Label) %>%
    mutate(
      speaker_id=paste(var1,var2,var3,sep="_"),
      segment=.get_class(Label,var6)) %>%
    ungroup()%>%
    group_by(speaker_id,segment)%>%
    mutate(context=var6,
      F1=mean(F1),
      F2=mean(F2),
      F3=mean(F3),
      F1_glide=as.numeric(NA),
      F2_glide=as.numeric(NA),
      F3_glide=as.numeric(NA)) %>%
    ungroup()%>%
    select(speaker_id,segment,context,F1,F2,F3,F1_glide,F2_glide,F3_glide) %>%
    distinct()
  dt_frame = as.data.frame(dt)
  nrm = norm.nearey(dt_frame) %>% rename(nF1="F*1",nF2="F*2")
  if (rm.na) {
    nrm = filter(nrm,!Vowel %in% c("",NA)) %>%
      filter(nF1 != Inf) %>%
      filter(nF2 != Inf) %>%
      group_by(Vowel) %>%
      filter(n() > 2)
  }
  return(nrm)
}

.get_class <- function(segment,token,debug="") {
  class_dict = c(
    make = c(
      EY1 = "BAIT",
      SIL = ""
    ),
    sand = c(
      AE1 = "BAN"
    ),
    coop = c(
      UW1 = "BOOT"
    ),
    tin = c(
      IH1 = "PIN"
    ),
    bag = c(
      AE1 = "BAT"
    ),
    bowl = c(
      OW1 = "POLE"
    ),
    pit = c(
      IH1 = "BIT"
    ),
    take = c(
      EY1 = "BAIT"
    ),
    tune = c(
      UW1 = "BOON"
    ),
    greasy = c(
      IY1 = "BEET",
      IY0 = "",
      OW1 = ""
    ),
    apricot = c(
      EY1 = "BAIT",
      AH0 = "",
      AA2 = "BOT",
      IY0 = ""
    ),
    do = c(
      UW1 = "BOOT"
    ),
    fell = c(
      EH1 = "PELL"
    ),
    gem = c(
      EH1 = "PEN"
    ),
    dance = c(
      AE1 = "BAN"
    ),
    colt = c(
      OW1 = "POLE"
    ),
    bang = c(
      AE1 = "BANG"
    ),
    coke = c(
      OW1 = "BOAT"
    ),
    pecan = c( # Don't use vowels from this word
      IY1 = "",
      AA2 = "",
      IH0 = "",
      AE1 = ""
    ),
    all = c(
      AO1 = "PAUL"
    ),
    old = c(
      OW1 = "POLE"
    ),
    feel = c(
      IY1 = "PEEL"
    ),
    ten = c(
      EH1 = "PEN"
    ),
    keep = c(
      IY1 = "BEET"
    ),
    beth = c(
      EH1 = "BEG"
    ),
    coal = c(
      OW1 = "POLE"
    ),
    toe = c(
      OW1 = "TOE"
    ),
    pool = c(
      UW1 = "POOL"
    ),
    tuesday = c(
      UW1 = "BOOT",
      EY2 = "BAIT"
    ),
    cot = c(
      AA1 = "BOT"
    ),
    fill = c(
      IH1 = "PILL"
    ),
    almond = c(
      AA1 = "",
      AH0 = ""
    ),
    gym = c(
      IH1 = "PIN"
    ),
    egg = c(
      EH1 = "BEG"
    ),
    pail = c(
      EY1 = "PAIL"
    ),
    cult = c(
      AH1 = "PULP"
    ),
    pal = c(
      AE1 = "PAL",
      OW1 = ""
    ),
    been = c(
      IH1 = "PIN"
    ),
    both = c(
      OW1 = "BOAT"
    ),
    cut = c(
      AH1 = "BUT"
    ),
    fail = c(
      EY1 = "PAIL"
    ),
    dam = c(
      AE1 = "BAN"
    ),
    bath = c(
      AE1 = "BAT"
    ),
    creek = c(
      IY1 = "BEET",
      IH1 = "BIT"
    ),
    pull = c(
      UH1 = "PULL"
    ),
    caught = c(
      AO1 = "BOUGHT"
    ),
    sat = c(
      AE1 = "BAT"
    ),
    which = c(
      IH1 = "BIT",
      SIL = ""
    )
  )
  token = trimws(tolower(token))
  segment = trimws(toupper(segment))
  class = class_dict[paste(token,segment,sep='.')]
  if (is.na(class)) {
    if (debug != ""){
      print(paste(debug,token,segment))
    }
  }
  return(class)
}

clean_sauce <- function(sauce) {
  dt = sauce %>%
    mutate(dur=seg_End-seg_Start,logdur=log(seg_End-seg_Start)) %>%
    select(
      site=var1,
      last=var2,
      first=var3,
      token=var6,
      segment=Label,
      dur,
      logdur,
      index=t,
      f0,
      F1,
      F2,
      F3,
      B3,
      H1c,
      H2c,
      H1H2c,
      CPP)
  return(dt)
}
