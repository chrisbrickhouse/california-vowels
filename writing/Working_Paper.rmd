---
title: "Low-Back Merger in California English: Working Paper"
author: "Christian Brickhouse"
date: "March 25, 2019"
output:
  html_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
setwd('../')
library(tidyverse)
library(vowels)  # Not actually used yet
library(gss)
source('./r_scripts/analysis_functions.r')
data.sauce.demo = load_sauce('./data/full_data_with_demo.csv')
data.normed = make_norm(data.sauce.demo)
data.clean.sauce.demo = clean_sauce(data.sauce.demo,T)
data.clean.normed.demo = merge_demo(data.normed,data.clean.sauce.demo)
```

## Caveat
This draft represents a work in progress and therefore **you should not cite this work for any reason**. The analysis is still ongoing and so this document will change substantially and without notice. Further work may demonstrate significant errors in present or past texts, and so readers should be aware that this analysis is subject to change as more work is done. This document is not intended as a scholarly publication but as living documentation of the ongoing analysis.

## Methods
Data were collected as part of the Voices of California project which conducts sociolinguistic interviews with life-long California residents. At the end of the interview, participants are asked to read a wordlist to ensure that vowels of itnerest are captured. This study analyzes that word list data from `r length(unique(data.clean.normed.demo$Speaker))` California English speakers. The selection criteria excluded any person who lived outside of the fieldsite of interest for more than 3(?) years between the ages of 8(?) and 18, or for more than 6(?) years after 18. The field sites, with year of fieldwork, were Merced (2010), Redding (2011), Bakersfield (2012), Sacramento (2014), Salinas (2016), Humboldt Bay (2017), and Redlands (2018).

The wordlists were force aligned using the Penn Forced Aligner (citation), extracted by automated script (see [extract_vowels_cj.praat](../praat_scripts/extract_vowels_cj.praat)), and analyzed using PraatSauce (citation). Vowels were normalized using the Nearey method (citation) before analysis.

## Results
The sample used in this analysis comprises `r length(unique(data.clean.normed.demo$Speaker))` speakers from `r length(unique(data.clean.sauce.demo$site))` field sites. 
```{r fieldsite_table, echo=FALSE}
print("Number of participants per fieldsite:")
table(distinct(data.clean.sauce.demo,site,first,last,.keep_all=TRUE)$site)
```

### Vowel Space Shape
```{r vowel_space_shape_data, include=FALSE}
data.v_angles = vowel_angles(data.clean.normed.demo)
data.cosangles = compute_vspace(data.v_angles)
vec_plot_data = data.v_angles%>%
  distinct(Speaker,pos,.keep_all = TRUE)%>%
  group_by(Speaker) %>%
  summarize( xu = .vs_testu(nF1,nF2,pos,1),
          yu = .vs_testu(nF1,nF2,pos,2),
          x2 = .vs_test2(nF1,nF2,pos,1),
          y2 = .vs_test2(nF1,nF2,pos,2),
          xt = .vs_testt(nF1,nF2,pos,1),
          yt = .vs_testt(nF1,nF2,pos,2))
```
To investigate whether the phonetic movement of the LOT and THOUGHT vowels is causing a shift in the shape of the vowel space, the vowels were treated as vectors and their angles measured. If the vowel space were triangular then the LOT-THOUGHT vowels should lie on the line between the lowest vowel and the highest backest vowel. For each speaker an algorithm identified both the lowest vowel that was not LOT or THOUGHT and the highest backest vowel. The high back vowel was defined as a vowel in the vowel classes of BOAT or POOL that had the lowest Euclidean distance from the origin in Hz space (i.e., closest to the origin).

The high back vowel and the LOT vowel were then converted to vectors from the low front vowel in order to test the hypothesis that LOT lies along the line between the low vowel and high back vowel. If this hypothesis were true then the angle between the two vectors should be zero. The alternative hypothesis---that the vowel space is trapezoidal---would predict that the LOT vector would have an angle of zero with the horizontal (F2) unit vector. As the plot below shows, the LOT vector appears to have minimal overlap with the high back vector, suggesting that there is not a consensus for triangular vowel spaces.

```{r vector_plot,message=FALSE,echo=FALSE}
ggplot(vec_plot_data,aes())+
  geom_segment(aes(x=-x2,y=-y2,xend=0,yend=0,color="High back",alpha=0.1),na.rm=TRUE) +
  geom_segment(aes(x=-xu,y=-yu,xend=0,yend=0,color="Unit vector",alpha=0.1),na.rm=TRUE) +
  geom_segment(aes(x=-xt,y=-yt,xend=0,yend=0,color="BOT",alpha=0.1),na.rm=TRUE) +
  guides(alpha=FALSE) +
  labs(x="Distance of normalized F2 from low front vowel",
       y="Distance of normalized F1 from low front vowel")
```

To more precisely test this hypothesis, a metric for how triangular or trapezoidal a speaker's vowel space is was computed. The triangularity of a vowel space was defined as the angle between the LOT vector and the high back vector. The trapezoidality of a vowel space was defined as the angle between the horizontal unit vector and the LOT vector. The ratio of triangularity to trapezoidality allows for the comparison of which state, triangle or trapezoid, best describes a speaker's vowel space shape. If this ratio has a value of 1 then the LOT vowel lies perfectly between triangular and trapezoidal states. If the value is greater than 1 then the vowel space is more triangular than it is trapezoidal supporting the hypothesis that the LOT-THOUGHT movement is causing a more triangular vowel space. However if the ratio is less than 1 then the vowel space is more trapezoidal supporting the alternative hypothesis that the LOT-THOUGHT movement is not causing a change in the vowel space shape. The plot below corroborates the intuitions from the previous figure as for each field site (Merced and Redding were not included due to small sample size) the bulk of speakers have a more trapezoidal than triangular space, though a notable few have more triangular vowel space.

```{r ratio_plot, message=FALSE,echo=FALSE}
ratio_data = data.cosangles %>%
  separate(Speaker,c('site','last','first'),'_') %>%
  filter(!site %in% c("MER","RED"))

ggplot(ratio_data,aes(x=cos_ratio)) +
  geom_histogram(binwidth=.025) +
  geom_vline(aes(xintercept=1,color="even")) +
  facet_wrap(~site) +
  xlim(0,3) +
  guides(color=FALSE) +
  labs(x="Ratio of triangle to trapezoid scores",
       y="Count")
```

However because the California Vowel Shift seems to be a change in progress, this analysis should consider how the vowel space has been changing over time. The figure below shows the triangle to trapezoid ratio for a participant by their birth year, and there appears to be a trend in apparent time whereby younger participants have a more triangular vowel space. For this analysis the data was further subsetted to remove those whose birthyear was unknown and any participant with negative ratio values as they were few and (from the first figure) likely to be measurement errors. The pattern seems strongest in Bakersfield and Sacramento. Redlands looks to have an increasing trend however younger speakers there seem to have a wider envelope of variation which may indicate an interesting social patterning of the low back vowels.

```{r ratio_by_birthyear, echo=FALSE}
site_labels = c(BAK="Bakersfield",
    HUM="Humboldt",
    RDL="Redlands",
    SAC="Sacramento",
    SAL="Salinas")
model.cosangles.data = data.cosangles%>%
  filter(cos_ratio > 0) %>%
  filter(!is.na(birthyear)) %>%
  filter(!site %in% c("RED","MER"))
ggplot(model.cosangles.data,aes(x=birthyear,y=cos_ratio))+
  geom_point()+
  geom_smooth(method = "lm")+
  labs(x="Birth year",y="Triangle to trapezoid ratio (higher = more triangular)")+
  facet_wrap(~site,labeller=labeller(site=site_labels))
```
