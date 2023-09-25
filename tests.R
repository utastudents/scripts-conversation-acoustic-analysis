library(agricolae)
library(dplyr)
library(car)
library(lme4)
library(moments)

opt <- lmerControl(optimizer = "bobyqa", optCtrl=list(maxfun=8e5))
opt2 <- lmerControl(optimizer = "Nelder_Mead", optCtrl=list(maxfun=8e5))
opt3 <- lmerControl(optimizer = "nloptwrap", optCtrl=list(maxeval=8e5))
opt4 <- lmerControl(optimizer = "nlminbwrap", optCtrl=list(maxfun=8e5))
opt5 <- lmerControl(optimizer = "optimx", optCtrl=list(maxit=8e5,method="L-BFGS-B"))

skewness(trans$gap[trans$params=='_30_200_1.25'])
kurtosis(trans$gap[trans$params=='_30_200_1.25'])

skewness(trans$gap[trans$params=='_50_50_1.25'])
kurtosis(trans$gap[trans$params=='_50_50_1.25'])


#transitions by minute

with(all_minute,pairwise.t.test(trans_cnt,minute))

lmer.min.tr.0 <- lmer(trans_cnt ~ (1 | conv) ,   data = all_minute, REML="FALSE")
lm.min.tr.null<- lm(trans_cnt ~ 1, data=all_minute)
lmer.min.tr.1 <- lmer(trans_cnt ~ min1 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.tr.2 <- lmer(trans_cnt ~ min4 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.tr.3 <- lmer(trans_cnt ~ LangCd + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.tr.4 <- lmer(trans_cnt ~ LangCd + min1 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.tr.5 <- lmer(trans_cnt ~ LangCd + min4 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.tr.6 <- lmer(trans_cnt ~ minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.tr.7 <- lmer(trans_cnt ~ LangCd + min4 + (minute | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.tr.8 <- lmer(trans_cnt ~ LangCd + min4 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")

anova(lmer.min.tr.0, lm.min.tr.null, lmer.min.tr.1, lmer.min.tr.2, lmer.min.tr.3, lmer.min.tr.4, lmer.min.tr.5, lmer.min.tr.6, lmer.min.tr.7, lmer.min.tr.8)

lmer.min.tr.5.reml <- update(lmer.min.tr.5, REML="TRUE")
summary(lmer.min.tr.5.reml)
#McFadden pseudo R squared
1-(logLik(lmer.min.tr.5.reml)/logLik(lm.min.tr.null))

hist_with_density(lmer.min.tr.5.reml)
resqq(lmer.min.tr.5.reml)
plot(lmer.min.tr.5.reml)

lmer.min.tr.7.slopes <- coef(lmer.min.tr.7)$conv
ggplot(lmer.min.tr.7.slopes) + 
 geom_density(aes(x=minute)) +
 theme_bw() 

summary(lmer.min.tr.7.slopes$minute)


#all_minute$bL<-paste0(all_minute$beg_cat,all_minute$LangCd)
#Brown-Forsythe Test
#leveneTest(trans_cnt ~ bL, data = all_minute)
leveneTest(trans_cnt ~ LangCd, data = all_minute)
leveneTest(trans_cnt ~ beg_cat, data = all_minute)
leveneTest(trans_cnt ~ min4, data = all_minute)
leveneTest(trans_cnt ~ min4, data = all_minute, center=mean)

##find slope of trans_cnt over time across all convs
multlm  <- lmList(trans_cnt ~ minute | conv, data=all_minute)
#multlm.2  <- update(multlm, pool = FALSE)
minute_slope <- coef(multlm)
summary(minute_slope)
minute_slope$conv <- row.names(minute_slope)
minute_slope <- sqldf("
 select a.*, b.LangCd
  from minute_slope a
   join good_conv b
    on a.conv = b.conv
")

##overlap per trans

lmer.min.otr.0 <- lmer(overlap_trans_ratio ~ (1 | conv) ,   data = all_minute, REML="FALSE")
lm.min.otr.null<- lm(overlap_trans_ratio ~ 1, data=all_minute)
lmer.min.otr.1 <- lmer(overlap_trans_ratio ~ min1 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.otr.2 <- lmer(overlap_trans_ratio ~ min4 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.otr.3 <- lmer(overlap_trans_ratio ~ LangCd + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.otr.4 <- lmer(overlap_trans_ratio ~ LangCd + min1 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.otr.5 <- lmer(overlap_trans_ratio ~ LangCd + min4 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.otr.6 <- lmer(overlap_trans_ratio ~ minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.otr.7 <- lmer(overlap_trans_ratio ~ LangCd + min4 + (minute | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.otr.8 <- lmer(overlap_trans_ratio ~ LangCd + min4 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.otr.9 <- lmer(overlap_trans_ratio ~ LangCd + min1 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.otr.10 <- lmer(overlap_trans_ratio ~ LangCd + min1 + minute + (minute | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.otr.11 <- lmer(overlap_trans_ratio ~ LangCd + min1 + (minute | conv),   data = all_minute, control=opt, REML="FALSE")

anova(lmer.min.otr.0, lm.min.otr.null, lmer.min.otr.1, lmer.min.otr.2, lmer.min.otr.3, lmer.min.otr.4, lmer.min.otr.5, lmer.min.otr.6, lmer.min.otr.7, lmer.min.otr.8, lmer.min.otr.9, lmer.min.otr.10, lmer.min.otr.11)
anova(lmer.min.otr.0, lm.min.otr.null, lmer.min.otr.1, lmer.min.otr.2, lmer.min.otr.4, lmer.min.otr.11)

lmer.min.otr.4.reml <- update(lmer.min.otr.4, REML="TRUE")
summary(lmer.min.otr.4.reml)
#McF
1-(logLik(lmer.min.otr.4.reml)/logLik(lm.min.otr.null))

hist_with_density(lmer.min.otr.4.reml)
resqq(lmer.min.otr.4.reml)

leveneTest(overlap_trans_ratio ~ min1, data = all_minute)
leveneTest(overlap_trans_ratio ~ LangCd, data = all_minute)
with(all_minute,kruskal(overlap_trans_ratio,LangCd,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))
with(all_minute,kruskal(overlap_trans_ratio,min1,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))

##gap duration per minute
lmer.min.gd.0 <- lmer(gap_dur ~ (1 | conv) ,   data = all_minute, REML="FALSE")
lm.min.gd.null<- lm(gap_dur ~ 1, data=all_minute)
lmer.min.gd.1 <- lmer(gap_dur ~ min1 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.gd.2 <- lmer(gap_dur ~ min4 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.gd.3 <- lmer(gap_dur ~ LangCd + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.gd.4 <- lmer(gap_dur ~ LangCd + min1 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.gd.5 <- lmer(gap_dur ~ LangCd + min4 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.gd.6 <- lmer(gap_dur ~ minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.gd.7 <- lmer(gap_dur ~ LangCd + min4 + (minute | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.gd.8 <- lmer(gap_dur ~ LangCd + min4 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.gd.9 <- lmer(gap_dur ~ LangCd + min1 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.gd.10 <- lmer(gap_dur ~ LangCd + min1 + minute + (minute | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.gd.11 <- lmer(gap_dur ~ LangCd + min1 + (minute | conv),   data = all_minute, control=opt, REML="FALSE")

anova(lmer.min.gd.0, lm.min.gd.null, lmer.min.gd.1, lmer.min.gd.2, lmer.min.gd.3, lmer.min.gd.4, lmer.min.gd.5, lmer.min.gd.6, lmer.min.gd.7, lmer.min.gd.8, lmer.min.gd.9, lmer.min.gd.10, lmer.min.gd.11)
anova(lmer.min.gd.0, lm.min.gd.null, lmer.min.gd.1, lmer.min.gd.2, lmer.min.gd.3, lmer.min.gd.4, lmer.min.gd.7, lmer.min.gd.9, lmer.min.gd.11)

lmer.min.gd.4.reml <- update(lmer.min.gd.4,REML="TRUE")
summary(lmer.min.gd.4.reml)

#McF
1-(logLik(lmer.min.gd.4.reml)/logLik(lm.min.gd.null))

hist_with_density(lmer.min.gd.4.reml)
resqq(lmer.min.gd.4.reml)

lmer.min.gd.5.reml <- update(lmer.min.gd.5,REML="TRUE")
summary(lmer.min.gd.5.reml)

lmer.min.gd.reml.5.lt <- lmerTest::lmer(gap_dur ~ LangCd + min4 + (1 | conv),   data = all_minute, REML="TRUE", control=opt)
summary(lmer.min.gd.reml.5.lt)

lmer.min.gd.12 <- lmer(gap_dur ~ LangCd * min1 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
summary(lmer.min.gd.12)

anova(lmer.min.gd.5,lmer.min.gd.12)

leveneTest(gap_dur ~ LangCd, data = all_minute)
leveneTest(gap_dur ~ min4, data = all_minute)
leveneTest(gap_dur ~ minute_f, data = all_minute)

##overlap duration
lmer.min.od.0 <- lmer(overlap_dur ~ (1 | conv) ,   data = all_minute, REML="FALSE")
lm.min.od.null<- lm(overlap_dur ~ 1, data=all_minute)
lmer.min.od.1 <- lmer(overlap_dur ~ min1 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.od.2 <- lmer(overlap_dur ~ min4 + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.od.3 <- lmer(overlap_dur ~ LangCd + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.od.4 <- lmer(overlap_dur ~ LangCd + min1 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.od.5 <- lmer(overlap_dur ~ LangCd + min4 + (1 | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.od.6 <- lmer(overlap_dur ~ minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.od.7 <- lmer(overlap_dur ~ LangCd + min4 + (minute | conv),   data = all_minute, REML="FALSE", control=opt)
lmer.min.od.8 <- lmer(overlap_dur ~ LangCd + min4 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
lmer.min.od.9 <- lmer(overlap_dur ~ LangCd + min1 + minute + (1 | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.od.10 <- lmer(overlap_dur ~ LangCd + min1 + minute + (minute | conv),   data = all_minute, control=opt, REML="FALSE")
#nearly unidentifiable:
lmer.min.od.11 <- lmer(overlap_dur ~ LangCd + min1 + (minute | conv),   data = all_minute, control=opt, REML="FALSE")

anova(lmer.min.od.0, lm.min.od.null, lmer.min.od.1, lmer.min.od.2, lmer.min.od.3, lmer.min.od.4, lmer.min.od.5, lmer.min.od.6, lmer.min.od.7, lmer.min.od.8, lmer.min.od.9, lmer.min.od.10, lmer.min.od.11)
anova(lmer.min.od.0, lm.min.od.null, lmer.min.od.1, lmer.min.od.4, lmer.min.od.11)

lmer.min.od.4.reml <- update(lmer.min.od.4,REML="TRUE")
summary(lmer.min.od.4.reml)

#McF
1-(logLik(lmer.min.od.4.reml)/logLik(lm.min.od.null))

hist_with_density(lmer.min.od.4.reml)
resqq(lmer.min.od.4.reml)

lmer.min.od.5.reml.lt <- lmerTest::lmer(overlap_dur ~ LangCd + min4 + (1 | conv),   data = all_minute, REML="TRUE", control=opt)
summary(lmer.min.od.5.reml.lt)

leveneTest(overlap_dur ~ LangCd, data = all_minute)
leveneTest(overlap_dur ~ min1, data = all_minute)
leveneTest(overlap_dur ~ min4, data = all_minute)
leveneTest(overlap_dur ~ minute_f, data = all_minute)

##conv level
#in general, not much point in mixed models at this level because we don't have a subject or item variable to use as a random intercept (number of convs is the nubmer of elements)
lm.tr.null <- lm(trans_rate ~ 1, data = conv)
lmer.tr.2 <- lmer(trans_rate ~ (1 | LangCd) ,   data = conv, REML="FALSE")
lmer.tr.5 <- lmer(trans_rate ~ (1 | Corpus) + (1 | LangCd), data=conv, control=opt2, REML="FALSE")
lmer.tr.9 <- lmer(trans_rate ~ LangCd  + (1 | Corpus),   data = conv, REML="FALSE")
lm.tr.1 <- lm(trans_rate ~ Corpus, data=conv)
lm.tr.2 <- lm(trans_rate ~ LangCd, data= conv)
lm.tr.3 <- lm(trans_rate ~ subcorpus, data= conv)
lm.tr.4 <- lm(trans_rate ~ RegionLangCd, data = conv)

anova( lmer.tr.2, lm.tr.null, lmer.tr.5, lmer.tr.9, lm.tr.1, lm.tr.2, lm.tr.3, lm.tr.4)
anova(lm.tr.null, lm.tr.1, lm.tr.2, lm.tr.3, lm.tr.4)

#
lm.otr.null <- lm(overlap_trans_ratio ~ 1, data = conv)
lmer.otr.1 <- lmer(overlap_trans_ratio ~ LangCd  + (1 | Corpus),   data = conv, REML="FALSE")
lm.otr.1 <- lm(overlap_trans_ratio ~ LangCd, data= conv)
anova( lmer.otr.1, lm.otr.null, lm.otr.1)


#assumption of equal variances ok for trans_rate, but not the others
leveneTest(trans_rate ~ LangCd , data = conv)
leveneTest(overlap_trans_ratio ~ LangCd , data = conv)
leveneTest(overlap_dur_ratio ~ LangCd , data = conv)
leveneTest(gap_dur_ratio ~ LangCd , data = conv)

leveneTest(trans_rate ~ LangCd , data = conv, center=mean)

#can do anova w/ trans_rate b/c variances are ok
aov.tr <- aov(trans_rate ~ LangCd , data = conv)
summary(aov.tr)
resqq(aov.tr)
scheffe.test(aov.tr,"LangCd",group=TRUE, alpha=0.01,console=TRUE)
#TukeyHSD(aov.tr)

#kruskal
with(conv,kruskal(overlap_trans_ratio,LangCd,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))
with(conv,kruskal(gap_dur_ratio,LangCd,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))
with(conv,kruskal(overlap_dur_ratio,LangCd,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))

#comparison
#aov.otr <- aov(overlap_trans_ratio ~ LangCd , data = conv)
#scheffe.test(aov.otr,"LangCd",group=TRUE, alpha=0.01,console=TRUE)
#resqq(aov.otr)

#aov.gd <- aov(gap_dur_ratio ~ LangCd , data = conv)
#scheffe.test(aov.gd,"LangCd",group=TRUE, alpha=0.01,console=TRUE)
#not normal
#resqq(aov.gd)

#aov.od <- aov(overlap_dur_ratio ~ LangCd , data = conv)
#scheffe.test(aov.od,"LangCd",group=TRUE, alpha=0.01,console=TRUE)
#not normal
#resqq(aov.od)

##gap - variances unequal for languages
leveneTest(gap ~ LangCd , data = trans[trans$params=='_30_200_1.25',])

with(trans[trans$params=='_30_200_1.25',],kruskal(gap,LangCd,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))

##ended convs fnl
leveneTest(trans_cnt ~ LangCd, data = end_min)
leveneTest(trans_cnt ~ fnl2way, data = end_min)
leveneTest(trans_cnt ~ fnl3way, data = end_min)

lmer.end.0 <- lmer(trans_cnt ~ (1 | conv) ,   data = end_min, REML="FALSE")
lm.end.null<- lm(trans_cnt ~ 1, data=end_min)
lm.end.1   <- lm(trans_cnt ~ min_from_end, data=end_min)
lm.end.2   <- lm(trans_cnt ~ fnl3way, data=end_min)
lmer.end.1 <- lmer(trans_cnt ~ min_from_end + (1 | conv),   data = end_min, REML="FALSE")
lmer.end.2 <- lmer(trans_cnt ~ fnl2way + (1 | conv),   data = end_min, control=opt, REML="FALSE")
lmer.end.3 <- lmer(trans_cnt ~ fnl3way + (1 | conv),   data = end_min, control=opt, REML="FALSE")
lmer.end.4 <- lmer(trans_cnt ~ fnl2way + (fnl2way | conv),   data = end_min, control=opt, REML="FALSE")
lmer.end.5 <- lmer(trans_cnt ~ fnl3way + (fnl3way | conv),   data = end_min, control=opt, REML="FALSE")

anova(lmer.end.0, lm.end.null, lm.end.1, lm.end.2, lmer.end.1,lmer.end.2,lmer.end.3,lmer.end.4,lmer.end.5)

summary(lmer.end.3)
resqq(lmer.end.3)
hist_with_density(lmer.end.3)

#beg_fnl isn't weighted evenly, but let's try this anyway
leveneTest(trans_rate ~ LangCd, data = beg_fnl)
leveneTest(trans_rate ~ begfnl2, data = beg_fnl)

lm.bf.2   <- lm(trans_rate *60~ begfnl2, data=beg_fnl)
lm.bf.3   <- lm(trans_rate *60~ begfnl2 + LangCd, data=beg_fnl)
lmer.bf.1 <- lmer(trans_rate *60~ begfnl2 + (1 | conv), data=beg_fnl, REML="FALSE")
lmer.bf.2 <- lmer(trans_rate *60~ LangCd + (1 | conv), data=beg_fnl, REML="FALSE")
lmer.bf.3 <- lmer(trans_rate *60 ~ LangCd + begfnl2 + (1 | conv), data=beg_fnl, REML="FALSE")

anova(lmer.bf.1, lm.bf.2, lm.bf.3, lmer.bf.2, lmer.bf.3)

lmer.bf.3.reml <- update(lmer.bf.3, REML="TRUE")
summary(lmer.bf.3.reml)
resqq(lmer.bf.3.reml)

##region
aov.tr.reg <- aov(trans_rate ~ RegionLangCd , data = conv)
summary(aov.tr.reg)
resqq(aov.tr.reg)
scheffe.test(aov.tr.reg,"RegionLangCd",group=TRUE, alpha=0.01,console=TRUE)

with(conv,kruskal(gap_dur_ratio,RegionLangCd ,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))
with(conv,kruskal(overlap_dur_ratio,RegionLangCd ,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))
with(conv,kruskal(overlap_trans_ratio,RegionLangCd ,group=TRUE, alpha=0.01, p.adj="holm", console=TRUE))

