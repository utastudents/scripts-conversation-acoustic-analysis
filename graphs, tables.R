library(ggplot2)
library(dplyr)
library(sqldf)
#library(gamlss)
#library(gamlss.dist)
#library(gamlss.add)

ggplot(data=trans[trans$params=='_30_200_1.25',],aes(y=gap, x= LangCd, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_y_continuous(name="Gap length") +   
  scale_x_discrete(name=element_blank())  +
  coord_cartesian(ylim=c(-1,1)) 

ggplot(data=trans,aes(x=gap,linetype=Parameters)) +
 geom_freqpoly(bins=48) +
 theme_bw() +
 scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
 xlim(-2,2.5) +
 ylab("") +
 xlab("Gap length") +
 theme(legend.key.size = unit(2, 'lines'))
#ggsave(filename="~/synced/dissertation/Corpus/images/trans freqpoly params.jpg" ,device="jpg",width = 6, height = 5,dpi = 300)

ggplot(data=trans[trans$params=='_30_200_1.25',],aes(x=gap)) +
 geom_histogram(bins=64, fill="grey",closed="right", boundary=0) +
 theme_bw() +
 scale_y_continuous(labels = function(x) format(x, scientific = FALSE,big.mark=",")) +
 xlim(-2,2.5) +
 coord_cartesian(xlim=c(-1,1)) +
 ylab("Count") +
 xlab("Gap length") +
 facet_wrap(. ~ LangCd, nrow=2,scales="free_y")
#ggsave(filename="~/synced/dissertation/Corpus/images/trans hist 30_200 by lang.jpg" ,device="jpg",width = 7, height = 4,dpi = 300)

#cumulative dist
#too close together to be useful (i.e., the languages are similar)
#ggplot(data=trans[trans$params=='_30_200_1.25',],aes(x=gap,color=LangCd)) +
# stat_ecdf() +
# theme_bw() +
# xlim(-3,3) +
# ylab("") +
# xlab("Gap length")

##summaries
#count
ggplot(conv, aes(x=LangCd,fill=LangCd)) +
  geom_bar() + 
  theme_bw() +
  theme(legend.position = "none") + 
  scale_x_discrete(name=element_blank()) +
  ylab("Number of conversations")

ggplot(conv, aes(x=LangCd,y=conv_length/3600,fill=LangCd)) +
  geom_bar(stat="identity") + 
  theme_bw() +
  theme(legend.position = "none") + 
  scale_x_discrete(name=element_blank()) +
  ylab("Total duration of conversations, in hours")  
#ggsave(filename="~/synced/dissertation/Corpus/images/conv dur by lang.jpg" ,device="jpg",width = 5, height = 4,dpi = 300)

##trans_rate
ggplot(data=conv,aes(x=trans_rate,color=LangCd)) +
 geom_freqpoly(bins=12,linewidth=1.5) +
 theme_bw()

ggplot(data=conv,aes(x=trans_rate * 60,y = after_stat(density))) +
 geom_histogram(bins=12, fill="grey",closed="right", boundary=0) +
 theme_bw() +
 geom_density(color="red")  +
 ylab("Density") +
 xlab("Transitions per minute") + 
 facet_wrap(. ~ LangCd, nrow=2)
##ggsave(filename="~/synced/dissertation/Corpus/images/trans rate hist,density by lang.jpg" ,device="jpg",width = 5, height = 4,dpi = 300)

#reorder languages by trans rate
df<-sqldf("
 select LangCd 
   from conv
   group by LangCd
  order by avg(trans_rate) desc
")
conv$LangCd2 <-factor(conv$LangCd,levels=df$LangCd,ordered=TRUE )

ggplot(data=conv,aes(y=trans_rate*60, x= LangCd2, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_y_continuous(name="Transitions per minute") +   
  scale_x_discrete(name=element_blank())  
#ggsave(filename="~/synced/dissertation/Corpus/images/trans rate boxplot reorder.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

ggplot(data=conv,aes(y=trans_rate*60, x= LangCd2, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_y_continuous(name="Transitions per minute") +   
  scale_x_discrete(name=element_blank()) +
  stat_summary(fun=mean, geom="point", size=1, shape=1,color="white") + 
  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2,color="white")  
#ggsave(filename="~/synced/dissertation/Corpus/images/trans rate boxplot reorder, w mean.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

ggplot(data=conv,aes(y=trans_rate * 60, x= LangCd)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=2, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("") +
  ylab("Transitions per minute") +
  theme(legend.position = "none") 

#pretty close to normal each time
ggplot(conv, aes(sample = trans_rate)) +
  stat_qq() +
  stat_qq_line() +
  theme_bw() + 
  facet_wrap(. ~ LangCd, nrow = 2)

##Overlap_trans_ratio
df<-sqldf("
 select LangCd 
   from conv
   group by LangCd
  order by median(overlap_trans_ratio) desc
")
conv$LangCd3 <-factor(conv$LangCd,levels=df$LangCd,ordered=TRUE )
#reorder for kw 
conv$LangCd3a<-factor(conv$LangCd,levels=c('deu','spa','ara','zho','fra','eng','jpn'),ordered=TRUE)

ggplot(data=conv,aes(y=overlap_trans_ratio, x= LangCd3, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none")+    
  #scale_y_continuous(name="Overlaps per transition") + 
  scale_y_continuous(name="Overlapped transitions",labels = scales::percent) + 
  scale_x_discrete(name=element_blank())
#ggsave(filename="~/synced/dissertation/Corpus/images/overlap_trans_ratio boxplot reorder.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

ggplot(data=conv,aes(y=overlap_trans_ratio, x= LangCd3a, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none")+    
  #scale_y_continuous(name="Overlaps per transition") + 
  scale_y_continuous(name="Overlapped transitions",labels = scales::percent) + 
  scale_x_discrete(name=element_blank())
#ggsave(filename="~/synced/dissertation/Corpus/images/overlap_trans_ratio boxplot reorder 2.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

#no need to show mean if only using kruskal, since it's based on the median
#ggplot(data=conv,aes(y=overlap_trans_ratio, x= LangCd3, fill=LangCd)) +
#  geom_boxplot(notch=FALSE) +
#  theme_bw() +
#  theme(legend.position = "none")+    
#  scale_y_continuous(name="Overlaps per transition") + 
#  scale_x_discrete(name=element_blank()) +
#  stat_summary(fun=mean, geom="point", size=1, shape=1,color="white") + 
#  stat_summary(fun.data = mean_se, geom = "errorbar", width=0.2,color="white") 

#ggplot(data=conv,aes(x=overlap_trans_ratio,y = after_stat(density))) +
# geom_histogram(bins=12, fill="grey",closed="right", boundary=0) +
# theme_bw() +
# geom_density(color="red")  +
# ylab("Density") +
# xlab("Overlaps per transition") + 
# facet_wrap(. ~ LangCd, nrow=2)
##ggsave(filename="~/synced/dissertation/Corpus/images/overlap_trans_ratio hist,density.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)


#parm=list(shape1=7,shape2=5.5)
#parm=list(shape1=9,shape2=5)
#ggplot(conv, aes(sample = overlap_trans_ratio)) +
#  stat_qq(distribution = stats::qbeta, dparams = parm) +
#  stat_qq_line(distribution = stats::qbeta, dparams = parm) +
#  theme_bw()  +
#  facet_wrap(LangCd ~ .) 

##overlap dur
df<-sqldf("
 select LangCd 
   from conv
   group by LangCd
  order by avg(overlap_dur_ratio) desc
")
conv$LangCd5 <-factor(conv$LangCd,levels=df$LangCd,ordered=TRUE )

ggplot(data=conv,aes(y=overlap_dur_ratio, x= LangCd5, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none")+    
  scale_y_continuous(name="Percentage of conversation overlapped",labels = scales::percent) + 
  scale_x_discrete(name=element_blank())
#ggsave(filename="~/synced/dissertation/Corpus/images/overlap_dur_ratio boxplot reorder.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)


#parm=list(shape1=1.5,shape2=6)
#parm=list(shape1=1.5,shape2=9)
#ggplot(conv, aes(sample = overlap_dur_ratio)) +
#  stat_qq(distribution = stats::qbeta, dparams = parm) +
#  stat_qq_line(distribution = stats::qbeta, dparams = parm) +
#  theme_bw() 

##
#gap duration
df<-sqldf("
 select LangCd 
   from conv
   group by LangCd
  order by avg(gap_dur_ratio) desc
")
conv$LangCd4 <-factor(conv$LangCd,levels=df$LangCd,ordered=TRUE )

ggplot(data=conv,aes(y=gap_dur_ratio, x= LangCd4, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none")+    
  scale_y_continuous(name="Percentage of conversation\nin silence between speakers",labels = scales::percent) + 
  scale_x_discrete(name=element_blank()) 
#ggsave(filename="~/synced/dissertation/Corpus/images/gap_dur_ratio boxplot reorder.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

## gap & overlap dur together
ggplot(data=conv_long[conv_long$Type=='Gap' | conv_long$Type=='Overlap',],aes(y=Duration * 60/conv_length, x= LangCd, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=2, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("") +
  ylab("Seconds per minute") +
  theme(legend.position = "none") 
#ggsave(filename="~/synced/dissertation/Corpus/images/overlap, gap dur by lang.jpg" ,device="jpg",width = 4, height = 3,dpi = 300)

ggplot(data=conv_long[conv_long$Type=='Gap' | conv_long$Type=='Overlap',],aes(y=Duration * 60/conv_length, x= LangCd, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=2, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("") +
  ylab("Seconds per minute")  
#ggsave(filename="~/synced/dissertation/Corpus/images/overlap, gap dur by lang w legend.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

#mean individual gap/overlap length
#ggplot(data=conv_long[conv_long$Type=='Gap' | conv_long$Type=='Overlap',],aes(y=Duration * 1000/Count, x= LangCd, color=Type)) +
#  theme_bw() +
#  stat_summary(fun=mean, geom="point", size=2, shape=1) + 
#  stat_summary(fun.data = mean_se, geom = "errorbar")  +
#  xlab("") +
#  ylab("Mean gap/overlap duration (ms)") +
#  theme(legend.position = "none") 
##ggsave(filename="~/synced/dissertation/Corpus/images/mean overlap, gap dur by lang.jpg" ,device="jpg",width = 2.5, height = 2,dpi = 300)

#mean number per min.
ggplot(data=conv_long2,aes(y=Count/(conv_length/60), x= LangCd, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=2, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("") +
  ylab("Number per minute")+
  theme(legend.position = "none") 
#ggsave(filename="~/synced/dissertation/Corpus/images/overlap, gap count by lang.jpg" ,device="jpg",width = 4, height = 3,dpi = 300)

#trans level
#ggplot() + 
#  theme_bw() +
#  stat_summary(data=trans_gap_type,aes(y=gap, x=LangCd, color=Type),fun=mean, geom="point", size=2, shape=1) + 
#  stat_summary(data=trans_gap_type,aes(y=gap, x=LangCd, color=Type),fun.data = mean_se, geom = "errorbar")  +
#  xlab("") +
#  ylab("Mean gap") 

#gap length (transition time)
ggplot(data=trans[trans$params=='_30_200_1.25',],aes( y=gap*1000,x=LangCd)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=2, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  #stat_summary(fun=median, geom="point", size=2, shape=2) + 
  xlab("") +
  ylab("Mean Gap Length")
##ggsave(filename="~/synced/dissertation/Corpus/images/mean gap,trans.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

#stacked bar of overlap/gap/single speaker
ggplot(conv_long, aes(x=LangCd2, y=Duration, fill=Type)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  xlab("") +
  ylab("Duration per conversation")

ggplot(conv_long, aes(x=LangCd2, y=Count, fill=Type)) + 
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  xlab("") 

##by minute
ggplot(data=all_minute,aes(y=trans_cnt, x= minute)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape = 1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ylab("Transitions per minute") +
  xlab("Minute") 

ggplot(data=all_minute,aes(y=trans_cnt, x= minute)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape = 1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ylab("Transitions per minute") +
  xlab("Minute") +
  stat_smooth(formula=y~x, method="loess", span=0.5)
#ggsave(filename="~/synced/dissertation/Corpus/images/trans_cnt by_minute span 0.5.jpg" ,device="jpg",width = 6, height = 4,dpi = 300)

ggplot(data=all_minute,aes(y=trans_cnt, x= minute)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape = 1) + 
  #stat_summary(fun.data = mean_se, geom = "errorbar") +
  ylab("Transitions per minute") +
  xlab("Minute") +
  stat_smooth(formula=y~x, method="loess", span=0.5) +
  facet_wrap(. ~ LangCd,nrow=2)
#ggsave(filename="~/synced/dissertation/Corpus/images/trans_cnt by minute by lang span 0.5.jpg" ,device="jpg",width = 6, height = 4,dpi = 300)

#overlap per trans
ggplot(data=all_minute,aes(y=overlap_trans_ratio, x= minute)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ylab("Overlaps per transition") +
  xlab("Minute") 

ggplot(data=all_minute,aes(y=overlap_trans_ratio, x= minute)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ylab("Overlaps per transition") +
  xlab("Minute") +
  stat_smooth(formula=y~x, method="loess", span=0.5) +
  facet_wrap(. ~ LangCd, nrow=2)

ggplot(data=all_minute[all_minute$minute<=4,],aes(y=overlap_trans_ratio, x= minute)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar") +
  ylab("Overlaps per transition") +
  xlab("Minute") +
  facet_wrap(. ~ LangCd, nrow=2)

## gap & overlap dur together, by minute
ggplot(data=all_minute_long,aes(y=Duration, x= minute, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=1, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("Minute") +
  ylab("Seconds per minute") +
  facet_wrap(. ~ LangCd, nrow = 2) +
  theme(legend.position = "bottom")   
#ggsave(filename="~/synced/dissertation/Corpus/images/gap, overlap dur by minute.jpg" ,device="jpg",width = 6, height = 4,dpi = 300)

ggplot(data=all_minute_long,aes(y=Duration, x= minute, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=1, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("Minute") +
  ylab("Seconds per minute") +
  facet_wrap(. ~ LangCd, nrow = 2)   
##ggsave(filename="~/synced/dissertation/Corpus/images/gap, overlap dur by minute, legend right.jpg" ,device="jpg",width = 7, height = 4,dpi = 300)

ggplot(data=all_minute_long,aes(y=Count, x= minute, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=1, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")  +
  xlab("Minute") +
  facet_wrap(. ~ LangCd, nrow = 4)

ggplot(data=(ranef(lmer.min.24reml)$conv),aes(x=minute)) +
 geom_histogram(bins=40, closed="right", boundary=0) +
 theme_bw() 
#
#some are zero, so this isn't perfect
#ggplot(data=all_minute,aes(y=gap_dur/gap_cnt, x= minute)) +
#  theme_bw() +
#  #stat_summary(fun=mean, geom="point", size=2, shape=2,color="blue") + 
#  stat_summary(fun.data = mean_se, geom = "errorbar",color="blue") +
#  ylab("avg duration") +
#  xlab("Minute") +
#  stat_summary(data=all_minute,aes(y=overlap_dur/overlap_cnt,x=minute),fun.data = mean_se, geom = "errorbar",color="red") 

##CH vs CF
#is there a systematic difference between CF and CH?
#most of the differences are between languages in one and not the other
#ggplot(data=all_minute,aes(y=overlap_dur, x= minute)) +
#  theme_bw() +
#  stat_summary(fun=mean, geom="point", size=2, shape=2) + 
#  stat_summary(fun.data = mean_se, geom = "errorbar") +
#  ylab("Overlap duration per minute") +
#  xlab("Minute") +
#  facet_wrap(LangCd ~ Corpus)

#
#parm=list(scale=1)
#ggplot(trans[trans$params=='_50_50_1.25' & trans$gap<3.2 & trans$gap>-3.2,], aes(sample = gap)) +
#  stat_qq(distribution = qlaplace, dparams = parm) +
#  stat_qq_line(distribution = qlaplace, dparams = parm) +
#  theme_bw() 

#parm=list(scale=1,alpha=0.36)
#ggplot(trans[trans$params=='_50_50_1.25' & trans$gap<3.2 & trans$gap>-3.2,], aes(sample = gap)) +
#  stat_qq(distribution = greybox::qalaplace, dparams = parm) +
#  stat_qq_line(distribution = greybox::qalaplace, dparams = parm) +
#  theme_bw()

#tg3 <- sqldf("
# select * from trans
#  where params = '_50_50_1.25' and gap < 3.3
#")
#parms=c(mu=-0.086,sigma=0.0719, nu= 1.53, tau=0.815)
#ggplot(tg3, aes(sample = gap)) +
#  stat_qq(distribution = qSEP3, dparams = parms) +
#  stat_qq_line(distribution = qSEP3, dparams = parms) +
#  theme_bw() +
#  xlab("Theoretical Quantiles") +
#  ylab("Empirical Quantiles")
##ggsave(filename="~/synced/dissertation/Corpus/images/SEP3 50_50.jpg" ,device="jpg",width = 5, height = 4,dpi = 300)

#parms=c(mu=-0.086,sigma=0.0719, nu= 1.53, tau=0.815)
#ggplot(tg3, aes(sample = gap)) +
#  stat_qq(distribution = qSEP3, dparams = parms) +
#  stat_qq_line(distribution = qSEP3, dparams = parms) +
#  theme_bw() +
#  facet_wrap(. ~ LangCd, nrow=2) +
#  xlab("Theoretical Quantiles") +
#  ylab("Empirical Quantiles")
##ggsave(filename="~/synced/dissertation/Corpus/images/SEP3 50_50 by lang.jpg" ,device="jpg",width = 6, height = 4,dpi = 300)

ggplot(data=sss[sss$tm2<10,],aes(x=tm2)) +
 geom_histogram(bins = 40, fill="grey",closed="right", boundary=0) +
 scale_y_continuous(name="Count") +
 scale_x_continuous(name="Single-speaker segment (s)") +
 theme_bw()

ggplot(data=sss[sss$tm2<4,],aes(x=tm2)) +
 geom_histogram(binwidth = 0.01, fill="grey",closed="right", boundary=0) +
 scale_y_continuous(name="Count") +
 scale_x_continuous(name="Single-speaker segment (s)") +
 theme_bw()

#almost a uniform log distribution between 0.2 and 1.9 (actually bimodal, but fairly even in that range)
ggplot(data=sss[sss$tm2<10 & sss$tm2>0.03,],aes(x=tm2)) +
 geom_histogram(bins = 50, fill="grey",closed="right", boundary=0) +
 scale_y_continuous(name="Count") +
 scale_x_continuous(name="Single-speaker segment (s)",trans='log10',breaks=c(0.03,0.1,0.3,1,3,10)) +
 theme_bw()

ggplot(data=sss,aes(y=tm2, x= LangCd, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_x_discrete(name=element_blank()) +
  scale_y_continuous(trans='log10',breaks=c(0.001,0.01,0.1,0.3,1,2,10,100,1000))

#exclude insubstantial segments (those < 1s) and those over 30s (just for readability)
ggplot(data=sss[sss$tm2>1.0 & sss$tm2<30,],aes(y=tm2, x= LangCd, fill=LangCd)) +
  geom_boxplot(notch=FALSE) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_x_discrete(name=element_blank()) +
  scale_y_continuous(trans='log10')

##data for tables
#data summary
sqldf("select LangCd, count(1), sum(conv_length)
 from conv
 group by LangCd")

#conversations that don't end right at 15 or 30 minutes
sqldf("select LangCd, count(1), sum(conv_length)
 from ended_conv 
 group by LangCd")

#trans level
#table 2 Gap Metrics
 summary(trans$gap[trans$params=='_30_200_1.25'])
 sd(trans$gap[trans$params=='_30_200_1.25'])

 sqldf("
  select LangCd, avg(gap), stdev(gap), median(gap)
   from trans
   where params = '_30_200_1.25'
   group by LangCd
 ")
 #mode
 sqldf("select gap,count(1) from trans where params = '_30_200_1.25' group by gap order by 2")
 #sqldf("select gap,count(1) from trans where params = '_50_50_1.25' group by gap order by 2")
 sqldf("
  select *
  from
  (
  select LangCd, gap, cnt, max(cnt) over (partition by LangCd) max_cnt
  from
  (
  select LangCd, gap,count(1) cnt
   from trans 
   where params = '_30_200_1.25' 
   group by LangCd, gap 
   order by 2
  )
  ) 
  where max_cnt = cnt
 ")

#conv level

summary(conv$gap_dur_ratio)
summary(conv$overlap_dur_ratio)
#durations expressed in seconds per minute
sqldf("
 select LangCd, 
     avg(gap_dur_ratio)*60 mn_gap_dur, 
     median(gap_dur_ratio)*60 mdn_gap_dur,
     avg(gap_cnt/(conv_length/60)) mn_gap_num,
     avg(overlap_dur_ratio)*60 mn_overlap_dur,
     median(overlap_dur_ratio)*60 mdn_overlap_dur,
     avg(overlap_cnt/(conv_length/60)) mn_overlap_num
   from conv
  group by LangCd
")


summary(conv$trans_rate*60)
sd(conv$trans_rate*60)
sqldf("
 select LangCd, avg(trans_rate)*60 mean_trans_per_min, stdev(trans_rate)*60 sd, median(trans_rate)*60 median_trans_per_min
   from conv
  group by LangCd
")

summary(conv$overlap_trans_ratio)
sqldf("
 select LangCd, avg(overlap_trans_ratio),-- avg(gap_dur_ratio), avg(overlap_dur_ratio)
     median(overlap_trans_ratio)
   from conv
  group by LangCd
")

sqldf("
 select LangCd, sum(conv_length) length, 
    sum(gap_cnt) gap_cnt, sum(overlap_cnt) overlap_cnt, sum(zero_cnt) zero_cnt, sum(trans_cnt) trans_cnt,
    cast(100 as float) *sum(gap_cnt)/sum(trans_cnt) gap_pct,
    cast(100 as float) *sum(overlap_cnt)/sum(trans_cnt) overlap_pct,
    cast(100 as float) *sum(zero_cnt)/sum(trans_cnt) zero_pct
  from conv
  group by LangCd
")

sqldf("
 select sum(conv_length) length, 
    sum(gap_cnt) gap_cnt, sum(overlap_cnt) overlap_cnt, sum(zero_cnt) zero_cnt, sum(trans_cnt) trans_cnt,
    cast(100 as float) *sum(gap_cnt)/sum(trans_cnt) gap_pct,
    cast(100 as float) *sum(overlap_cnt)/sum(trans_cnt) overlap_pct,
    cast(100 as float) *sum(zero_cnt)/sum(trans_cnt) zero_pct
  from conv
")


#minute
sqldf("
 select minute, avg(trans_cnt) trans_per_min,
     cast(sum(case when LangCd = 'ara' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'ara' then 1 else 0 end) ara_trans,
     cast(sum(case when LangCd = 'deu' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'deu' then 1 else 0 end) deu_trans,
     cast(sum(case when LangCd = 'eng' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'eng' then 1 else 0 end) eng_trans,
     cast(sum(case when LangCd = 'fra' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'fra' then 1 else 0 end) fra_trans,
     cast(sum(case when LangCd = 'jpn' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'jpn' then 1 else 0 end) jpn_trans,
     cast(sum(case when LangCd = 'spa' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'spa' then 1 else 0 end) spa_trans,
     cast(sum(case when LangCd = 'zho' then trans_cnt else 0 end) as float) / sum(case when LangCd = 'zho' then 1 else 0 end) zho_trans
   from all_minute
  group by minute
")

sqldf("
 select slope_cat, count(1)
 from
 (
 select LangCd, case when minute < 0 then 'negative' when minute > 0 then 'positive' else 'zero' end slope_cat
  from minute_slope
 ) a
 group by slope_cat
")

sqldf("
 select LangCd, slope_cat, count(1)
 from
 (
 select LangCd, case when minute < 0 then 'negative' when minute > 0 then 'positive' else 'zero' end slope_cat
  from minute_slope
 ) a
 group by LangCd, slope_cat
")

#count where minute 0 trans cnt > minute 1
sqldf("select count(1)
  from all_minute a
   join all_minute b
     on a.subcorpus = b.subcorpus and a.conv = b.conv and a.minute = 0 and b.minute = 1
   where a.trans_cnt > b.trans_cnt
")

sqldf("select count(1)
  from all_minute a
   join all_minute b
     on a.subcorpus = b.subcorpus and a.conv = b.conv and a.minute = 0 and b.minute = 9
   where a.trans_cnt > b.trans_cnt
")

sqldf("
 select sum(gap), count(1) 
  from trans 
  where params = '_30_200_1.25' and gap > 3
")

sqldf("
 select sum(gap), count(1) 
  from trans 
  where params = '_30_200_1.25' and gap = 3.5
")

sqldf("
 select sum(gap), count(1) 
  from trans 
  where params = '_30_200_1.25' and gap > 0
")

#other
sqldf("select count(distinct conv) from trans where params = '_30_200_1.25' and gap = 3.5")
sqldf("select type, count(1) from all_gaps where gap >= 3.5 group by type")

##examples
ggplot(data=all_minute[all_minute$conv=='1954',],aes(y=trans_cnt, x= minute)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  ylab("Transitions per minute") +
  xlab("Minute") 

#including partial minute at end
ggplot(data=all_minute0[all_minute0$conv=='1954',],aes(y=trans_rate*60, x= minute)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  ylab("Transitions per minute") +
  xlab("Minute") 
#ggsave(filename="~/synced/dissertation/Corpus/images/1954 trans_rate.jpg" ,device="jpg",width =4, height = 4,dpi = 300)
sqldf("select conv, minute, trans_cnt, trans_rate, dur from all_minute0 where conv = '1954'")
sqldf("select conv, min_from_end, trans_cnt, trans_cnt from end_min where conv = '1954'")
#last 60 seconds is 53 transitions vs partial min 17 calculated as 55.7 per min

ggplot(data=end_min[end_min$conv=='1954',],aes(y=trans_cnt, x= min_from_end)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  ylab("Transitions per minute") +
  xlab("Minute from end") 

sqldf("select ch, begin_sound, end_sound, adj_begin, trans_begin, gap
  from trans where conv = '1954' and params = '_30_200_1.25'")

ggplot(data=all_minute[all_minute$conv=='0638'|all_minute$conv=='4093'|all_minute$conv=='4156'|all_minute$conv=='1758'|all_minute$conv=='5567'|all_minute$conv=='6573'|all_minute$conv=='1954'|all_minute$conv=='5882',],aes(y=trans_cnt, x= minute,color =conv)) +
  theme_bw() +
  ylab("Transitions per minute") +
  xlab("Minute") +
  geom_line() +
  geom_point() + 
  facet_wrap(. ~ conv)

ggplot(data=all_minute_long[all_minute_long$conv=='1954',],aes(y=Duration, x= minute, shape=Type, color=Type)) +
  theme_bw() +
  stat_summary(fun=mean, geom="point", size=3) + 
  xlab("Minute") +
  ylab("Seconds per minute") +
  facet_wrap(. ~ LangCd, nrow = 2) +
  theme(legend.position = "bottom")  

##closing
ggplot(data=end_min,aes(y=trans_cnt, x= fnl3way)) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_y_continuous(name="Transitions per minute") +   
  scale_x_discrete(name=element_blank()) +
  stat_summary(fun=mean, geom="point", size=1, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")   +
  facet_wrap(. ~ LangCd, nrow = 2)

ggplot(data=beg_fnl,aes(y=trans_rate*60, x= begfnl)) +
  theme_bw() +
  theme(legend.position = "none") +  
  scale_y_continuous(name="Transitions per minute") +   
  scale_x_discrete(name=element_blank()) +
  stat_summary(fun=mean, geom="point", size=1, shape=1) + 
  stat_summary(fun.data = mean_se, geom = "errorbar")   +
  facet_wrap(. ~ LangCd, nrow = 2) +
  theme(axis.text.x = element_text(angle = 60, vjust = 0.8, hjust=1))
#ggsave(filename="~/synced/dissertation/Corpus/images/ended begfnl.jpg" ,device="jpg",width = 5, height = 3,dpi = 300)

#sdtc
all_min_sdtc<-sqldf("
 select LangCd, conv, stdev(trans_cnt) sdtc, max(trans_cnt) - min(trans_cnt) rngtc, avg(trans_cnt) mntc, median(trans_cnt) mdntc
  from all_minute
  group by LangCd, conv
")

ggplot(data=all_min_sdtc,aes(x=sdtc)) +
 geom_histogram(bins=16, fill="grey",closed="right", boundary=0) +
 theme_bw() +
 xlab("Standard Deviation of Transition Count per Minute") +
 ylab("Count") +
 facet_wrap(. ~ LangCd, nrow=2,scales="free_y")
#ggsave(filename="~/synced/dissertation/Corpus/images/sdtc by lang.jpg" ,device="jpg",width = 5, height = 4,dpi = 300)

sqldf("select LangCd, avg(sdtc), median(sdtc), avg(mntc)
  from all_min_sdtc group by LangCd
")

sqldf("select avg(sdtc), median(sdtc), avg(mntc)
  from all_min_sdtc 
")

sqldf("select count(1) from all_min_sdtc where sdtc < 10")
sqldf("select * from all_min_sdtc where sdtc > 15")

#sdtc mode
for(i in seq_along(levels(all_min_sdtc$LangCd)))
  {
  print(levels(all_min_sdtc$LangCd)[i])
  d<-density(all_min_sdtc$sdtc[all_min_sdtc$LangCd==levels(all_min_sdtc$LangCd)[i]])
  print(d$x[which.max(d$y)])
  }

