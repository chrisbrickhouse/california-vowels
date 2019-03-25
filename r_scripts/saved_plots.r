# This script contains the code for plots that have been published or distributed
#   in some meaningful sense. It serves largely as an archive of how particular
#   plots were made while also making modifications and reproductions easy.

source('./r_scripts/analysis_functions.r')

vec_plot_data = data.v_angles%>%
  distinct(Speaker,pos,.keep_all = TRUE)%>%
  group_by(Speaker) %>%
  summarize( xu = .vs_testu(nF1,nF2,pos,1),
          yu = .vs_testu(nF1,nF2,pos,2),
          x2 = .vs_test2(nF1,nF2,pos,1),
          y2 = .vs_test2(nF1,nF2,pos,2),
          xt = .vs_testt(nF1,nF2,pos,1),
          yt = .vs_testt(nF1,nF2,pos,2))

vec_plot = ggplot(vec_plot_data,aes())+
  geom_segment(aes(x=-x2,y=-y2,xend=0,yend=0,color="High back",alpha=0.1)) +
  geom_segment(aes(x=-xu,y=-yu,xend=0,yend=0,color="Unit vector",alpha=0.1)) +
  geom_segment(aes(x=-xt,y=-yt,xend=0,yend=0,color="BOT",alpha=0.1)) +
  guides(alpha=FALSE) +
  labs(x="Distance of normalized F2 from low front vowel",
       y="Distance of normalized F1 from low front vowel",
       title="4 Mar 2019")
  
ggsave('vowel_vectors.png',vec_plot,device=png(),'./plots')

ratio_data = data.cosangles %>%
  separate(Speaker,c('site','last','first'),'_') %>%
  filter(!site %in% c("MER","RED"))

ratio_plot = ggplot(ratio_data,aes(x=cos_ratio)) +
  geom_histogram(binwidth=.025) +
  geom_vline(aes(xintercept=1,color="even")) +
  facet_wrap(~site) +
  xlim(0,3) +
  guides(color=FALSE) +
  labs(x="Ratio of triangle to trapezoid scores",
       y="Count",
       title="4 Mar 2019")

ggsave('angle_ratios.png',ratio_plot,device=png(),'./plots')

v_plot_1 = ggplot(data.cleannormed%>%
         filter(Vowel%in%c("BEET","BAT","POOL","BOT","BOUGHT"))%>%
         filter(!site %in% c("RED","MER","RDL")),aes(x=nF2,y=nF1,color=Vowel,label=Vowel)) +
  #geom_text() + # uncomment to have vowels labelled with words
  geom_point(alpha=0.2) +
  scale_y_reverse(position = "right") + 
  scale_x_reverse(position = "top") +
  geom_density_2d() +
  facet_wrap(~site) +
  labs(x="Normalized F2",y="Normalized F1",title="8 Mar 2019")
ggsave('Vowel_Space_Corners.png',v_plot_1,device=png(),'./plots')

spline_plot = ggplot() +
  geom_ribbon(data=m$fit,alpha=0.2,aes(x=index,ymin=Fit-SE,ymax=Fit+SE,group=segment)) +
  geom_ribbon(data=m2$fit,alpha=0.2,aes(x=index,ymin=Fit-SE,ymax=Fit+SE,group=segment)) +
  geom_ribbon(data=m3$fit,alpha=0.2,aes(x=index,ymin=Fit-SE,ymax=Fit+SE,group=segment)) +
  geom_line(data=m$fit,aes(x=index,y=Fit,linetype=segment,color="F1")) +
  geom_line(data=m2$fit,aes(x=index,y=Fit,linetype=segment,color="F2")) +
  geom_line(data=m3$fit,aes(x=index,y=Fit,linetype=segment,color="F3")) +
  facet_wrap(~site) +
  labs(y="Predicted Formant (Hz)",x="Time normalized index",title="8 Mar 2019")
ggsave('SmoothingSpline.png',spline_plot,device=png(),'./plots')



