# Vowel Space Angle functions
# For functions related to the analysis of the angle of the low back vowels

vowel_angles <- function(normed) {
  a = bind_rows(high_back_vowel(normed),lowest_vowel(normed))
  b = high_back_vowel(normed)
  c = lowest_vowel(normed)
  vangle = bind_rows(a,normed%>%filter(Vowel=="BOT")%>%mutate(pos="target"))
  return(vangle)
}

compute_vspace <- function(vangle) {
  a = vangle%>%
    distinct(Speaker,pos,.keep_all = TRUE)%>%
    group_by(Speaker) %>%
    summarize(cos_ratio = .vspace_shape(nF1,nF2,pos))
  demo = vangle %>%
    select(Speaker,gender,birthyear,race,sexual_orientation,education,town_orientation,politics) %>%
    distinct()
  a = left_join(a,demo) %>%
    separate(Speaker,c("site","last","first"),sep = "_",remove = FALSE)
  return(a)
}

cosdist <- function(a,b) {
  num = sum(a*b)
  denom = mag(a)*mag(b)
  cd = num/denom
  return(cd)
}

lowest_vowel <- function(normed) {
  # This function finds the highest and backest vowel that is not BOT/BOUGHT
  # It takes normed data
  lv = normed %>%
    group_by(Speaker) %>%
    filter(!Vowel %in% c("BOT","BOUGHT")) %>%
    filter(nF1 == max(nF1)) %>%
    mutate(pos="low")
  return(lv)
}

high_back_vowel <- function(normed) {
  # This function finds the highest and backest vowel that is not BOT/BOUGHT
  # It takes normed data
  hbv = normed %>%
    group_by(Speaker) %>%
    filter(Vowel %in% c("BOAT","POOL")) %>%
    mutate(oDist = nF1^2+nF2^2) %>%
    filter(!oDist > min(oDist)) %>%
    mutate(pos="high")
  return(hbv)
}

mag <- function(a) {
  c = sqrt(sum(a^2))
  return(c)
}

.vspace_shape <- function(f1vec,f2vec,posvec) {
  li = which(posvec=="low")
  hi = which(posvec=="high")
  ti = setdiff(c(1,2,3),c(li,hi))
  anchor = c(f2vec[li],f1vec[li])
  anchor2 = c(f2vec[hi],f1vec[hi])
  target = c(f2vec[ti],f1vec[ti])
  a2 = anchor2 - anchor
  t = target - anchor
  cd = cosdist(a2,t)
  uv = c(-1,0)
  cdo = cosdist(uv,t)
  crat = cd/cdo # a value below 1 indicates more trapezoidal, above 1 indicates more triangular
  return(crat)
}

.recenter <- function(f1vec,f2vec,posvec) {
  li = which(posvec=="low")
  anchor = c(f2vec[li],f1vec[li])
  return(anchor)
}