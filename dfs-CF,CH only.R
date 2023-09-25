library(sqldf)
library(dplyr)

codes<-read.csv("~/synced/dissertation/corpus/codes.csv", header=TRUE)

trans  <-read.csv("~/synced/Dissertation/Corpus/trans.csv", header=TRUE)

same_pitch <- read.csv("~/synced/Dissertation/Corpus/same_pitch.csv", header=TRUE)

sound<-read.csv("~/synced/Dissertation/Corpus/sound_times.csv", header=TRUE)
sst  <-read.csv("~/synced/Dissertation/Corpus/sound_silence_turn.csv", header=TRUE)

#note:
#Across all of CH/CF, there are a small number of conversation numbers in both corpora. 
#I didn't try to make indexes on my dataframes, but I included subcorpus in the logical 
#key of all of them, and therefore I included subcorpus in the join condition in the 
#queries below even though the final data set has unique conv numbers.

all_conv <- sqldf("
  select a.subcorpus, a.params, a.conv, b.LangCd,
     count(1) trans_cnt, 
     min(a.gap) min_gap,
     max(a.gap) max_gap,
     max(a.end_sound) max_end,
     sum(case when a.gap < 0 then 1 else 0 end) overlap_cnt, 
     sum(case when a.gap < 0 then abs(a.gap) else 0 end) overlap_dur,
     sum(case when a.gap > 3.5 then 3.5 when a.gap > 0 then a.gap else 0 end) gap_dur,
     sum(case when a.gap > 0 then 1 else 0 end) gap_cnt,
     avg(case when a.gap > 3.5 then 3.5 else a.gap end) mean_gap,
     sum(case when a.gap = 0 then 1 else 0 end) zero_cnt
   from trans a
    join codes b
      on a.subcorpus = b.Code
   where a.params = '_30_200_1.25' and
    b.Corpus in ('CallHome','CallFriend') and
    --weed out duplicate Spanish conversations
    --the wav files have been deleted from the folder, so this line can be removed if the scripts are rerun from the top
    not (a.subcorpus = 'CFspa-c' and a.conv in ('5319','5357','5445','5539','5622','5640','5658','6896','6959'))
  group by a.subcorpus, a.params, a.conv 
")
all_conv$overlap_trans_ratio<-all_conv$overlap_cnt/all_conv$trans_cnt

#total up the sounding times for each channel, excluding sounds after the last transition
#this accounts for the few files where channel 2 data goes to silence partway through,
#but it also makes sense to exclude the final, unfinished turn when the recording stops
ch_sound<-sqldf("
  select a.subcorpus, a.params, a.conv, a.ch, sum(a.end-a.begin) dur, min(a.begin) min_begin
   from sound a
    join all_conv b 
      on b.subcorpus = a.subcorpus and b.params = a.params and b.conv = a.conv
  where a.end <= b.max_end
  group by a.subcorpus, a.params, a.conv, a.ch
")

#join the two channels
conv_sound<-sqldf("
  select a.subcorpus, a.params, a.conv, a.dur ch1_dur, b.dur ch2_dur,
    case when a.min_begin < b.min_begin then a.min_begin else b.min_begin end min_begin
   from ch_sound a
     join ch_sound b
       on a.subcorpus = b.subcorpus and a.params = b.params and a.conv = b.conv and a.ch = 1 and b.ch = 2
")

all_conv2 <- sqldf("
  select x.*, c.min_begin, x.max_end - c.min_begin conv_length
   from all_conv x
    join conv_sound c on c.subcorpus = x.subcorpus and c.params = x.params and c.conv = x.conv
")
all_conv2$trans_rate<-all_conv2$trans_cnt/all_conv2$conv_length
all_conv2$overlap_rate<-all_conv2$overlap_cnt/all_conv2$conv_length
all_conv2$overlap_dur_ratio<-all_conv2$overlap_dur/all_conv2$conv_length
all_conv2$gap_dur_ratio<-all_conv2$gap_dur/all_conv2$conv_length	

##intraturn gaps
#intraturn gaps can't be directly compared with the interturn gaps unless you exclude those less than 0.2s, which is done in all_gaps
ig <- sqldf("
 select *
 from
 (
 select *, case when begin_turn = lag_beg_turn then begin_sound - lag_end_sound end intraturn_gap
 from
 (
 select *, 
    lag(begin_turn) over (partition by subcorpus, params, conv order by begin_turn) lag_beg_turn,
    lag(end_sound) over (partition by subcorpus, params, conv order by begin_sound) lag_end_sound
  from sst where ch = 0
 )
 where intraturn_gap is not null --and intraturn_gap < 4
 )
 union all
 select *
 from
 (
 select *, case when begin_turn = lag_beg_turn then begin_sound - lag_end_sound end intraturn_gap
 from
 (
 select *, 
    lag(begin_turn) over (partition by subcorpus, params, conv order by begin_turn) lag_beg_turn,
    lag(end_sound) over (partition by subcorpus, params, conv order by begin_sound) lag_end_sound
  from sst where ch = 1
 )
 where intraturn_gap is not null --and intraturn_gap < 4
 )
")

#make this is a separate df to avoid having to nest another table in the next query
igmax <- sqldf("
 select subcorpus, conv, max(intraturn_gap) ig_max
  from ig
  group by subcorpus, conv
")

noisy_conv <- sqldf("
 select *
 from
 (
  select a.subcorpus, a.params, a.conv, a.ch1_dur, a.ch2_dur, 
    c.conv_length, c.overlap_rate, c.trans_rate, c.min_gap, c.max_gap,
    round(c.overlap_dur_ratio,4) overlap_dur_ratio, 
    round(d.ratio,4) same_pitch_ratio, 
    round(a.ch1_dur/c.conv_length,4) ch1_ratio, 
    round(a.ch2_dur/c.conv_length,4) ch2_ratio
   from conv_sound a
    join all_conv2 c
     on a.subcorpus = c.subcorpus and a.params = c.params and a.conv = c.conv
    left join same_pitch d
     on d.subcorpus = c.subcorpus and d.conv = c.conv
  ) x
  where 
    (
     --a lot of matched pitch and overlaps per second indicates probably crosstalk (cross-channel sound capture)
     --using a product here because one or the other could be high without it being a problem, but if they're both high, it's likely bad
     overlap_rate * same_pitch_ratio > 0.015 or
     --an extremely high transition rate is an alternate crosstalk indicator
     round(trans_rate,2) >= 0.9 or
     --if one channel or the other is sounding 80% of the time, it's probably noise in the background
     ch1_ratio >= 0.8 or
     ch2_ratio >= 0.8 or
     --conv was found to be crosstalk even though it didn't meet the criteria above
        (subcorpus='CGNvl-cs' and conv = 'fv701015') or
        (subcorpus='CGNvl-cs' and conv = 'fv701014') or
        (subcorpus='CGNvl-cs' and conv = 'fv901071') or
        (subcorpus='CGNvl-cs' and conv = 'fv701251') or
        (subcorpus='CGNvl-cs' and conv = 'fv701325') or
        (subcorpus='CGNvl-cs' and conv = 'fv901174') or
        (subcorpus='CGNnl-cs' and conv = 'fn008149') or
        (subcorpus='CHara' and conv = '4540') or
        (subcorpus='CGNnl-cs' and conv = 'fn008017') or
        (subcorpus='CGNvl-cs' and conv = 'fv901161') or
        (subcorpus='CGNnl-cs' and conv = 'fn008216') or
        (subcorpus='CGNnl-cs' and conv = 'fn008286') or
        (subcorpus='CGNnl-cs' and conv = 'fn008029') or
        (subcorpus='CHeng' and conv = '5273') or
        (subcorpus='CHjpn' and conv = '2218') or
        (subcorpus='CGNvl-cs' and conv = 'fv701322') or
        (subcorpus='CGNnl-cs' and conv = 'fn008298') or
        (subcorpus='CGNnl-cs' and conv = 'fn008154') or
        (subcorpus='CGNvl-cs' and conv = 'fv901124') or
        (subcorpus='CHara' and conv = '5775') or
        (subcorpus='CGNnl-cs' and conv = 'fn008068') or
        (subcorpus='CFspa-c' and conv = '6945') or
        (subcorpus='CGNnl-cs' and conv = 'fn008019') or
        (subcorpus='CGNvl-cs' and conv = 'fv701271') or
        (subcorpus='CHeng' and conv = '6183') or
        (subcorpus='CHeng' and conv = '6282') or
        (subcorpus='CHjpn' and conv = '1418') or
        (subcorpus='CGNvl-cs' and conv = 'fv901142') or
        (subcorpus='CGNvl-cs' and conv = 'fv901175') or
        (subcorpus='CGNnl-cs' and conv = 'fn008218') or
        (subcorpus='CGNnl-cs' and conv = 'fn008058') or
        (subcorpus='CGNvl-cs' and conv = 'fv901155') or
        (subcorpus='CGNvl-cs' and conv = 'fv901001') or
        (subcorpus='CFzho-t' and conv = '5553') or
        (subcorpus='CGNvl-cs' and conv = 'fv901122') or
        (subcorpus='CGNnl-cs' and conv = 'fn008129') or
        (subcorpus='CGNnl-cs' and conv = 'fn008034') or
        (subcorpus='CGNnl-cs' and conv = 'fn008060') or
        (subcorpus='CGNvl-cs' and conv = 'fv901126') or
        (subcorpus='CGNvl-cs' and conv = 'fv901176') or
        (subcorpus='CHspa' and conv = '2023') or
        (subcorpus='CGNvl-cs' and conv = 'fv701035') or
        (subcorpus='CGNvl-cs' and conv = 'fv901162') or
        (subcorpus='CFeng-s' and conv = '6295') or
        (subcorpus='CGNnl-cs' and conv = 'fn008013') or
        (subcorpus='CHzho' and conv = '975') or
        (subcorpus='CGNvl-cs' and conv = 'fv701054') or
        (subcorpus='CHdeu' and conv = '4828') or
        (subcorpus='CGNnl-cs' and conv = 'fn008131') or
        (subcorpus='CGNvl-cs' and conv = 'fv901204') 
    )
")

nrow(noisy_conv)

long_gap_conv <- sqldf("
  select a.subcorpus, a.conv
   from all_conv2 a
    join igmax i
     on i.subcorpus = a.subcorpus and i.conv = a.conv  
  where 
    (--one or more gaps is 30 seconds or more
     max_gap >= 30 or
     ig_max >= 30 )
   and not exists
   (select 1 from noisy_conv n
     where n.subcorpus = a.subcorpus and n.conv = a.conv)
")
nrow(long_gap_conv)

#the min_gap filter is applied here because a few conversations have nothing but noise and don't show up in ch_sound
#depending on which filters are in place, they may or may not get filtered out before this point

good_conv <- sqldf("
  select x.*
   from all_conv2 x
  where not exists
    (select 1 from noisy_conv y
     where y.subcorpus = x.subcorpus and y.conv = x.conv) and
    not exists
    (select 1 from long_gap_conv z
     where z.subcorpus = x.subcorpus and z.conv = x.conv) and
    --also exclude any that have super long overlaps (some files have nothing but noise)
    min_gap > -10 and
    --and limit to conversations at least 12 minutes long
    conv_length >= 720
")
nrow(good_conv)

all_excl<-sqldf("
  select subcorpus, Corpus, a.LangCd, conv
   from all_conv a
     join codes c
       on c.code = a.subcorpus
    where not exists
     (select 1 
       from good_conv b
       where b.subcorpus = a.subcorpus and b.conv = a.conv)
")

nrow(trans)

#refilter trans and add min_begin column & LangCd, and cap gap at 3.5s
trans <- sqldf("
  select a.subcorpus, a.params, a.conv, a.ch, a.begin_sound, a.end_sound, a.begin_turn, 
     case when a.gap > 3.5 then 3.5 else gap end gap,
     a.debug, b.min_begin, b.LangCd
   from trans a
     join good_conv b 
       on b.subcorpus = a.subcorpus and b.conv = a.conv
") 
trans$LangCd <- as.factor(trans$LangCd)
trans$subcorpus <- as.factor(trans$subcorpus)
trans$conv <- as.factor(trans$conv)
trans$params <- as.factor(trans$params)

nrow(trans)

conv <- sqldf("
 select x.*, 
   codes.Corpus, codes.RegionLangCd
 from good_conv x
   join codes on codes.Code = x.subcorpus
")
#
conv$Corpus <- as.factor(conv$Corpus )
conv$LangCd <- as.factor(conv$LangCd )
conv$RegionLangCd <- as.factor(conv$RegionLangCd )
f<-as.factor(conv$subcorpus)
conv$subcorpus<-factor(f, levels=unique(f[order(conv$LangCd, conv$subcorpus)]), ordered=TRUE)
conv$params <- as.factor(conv$params)

conv$single_speaker_dur<-conv$conv_length - conv$overlap_dur - conv$gap_dur
conv_long<-sqldf("
 select LangCd, conv, conv_length, gap_dur Duration, gap_cnt Count, 'Gap' Type from conv
  union
 select LangCd, conv, conv_length, overlap_dur Duration, overlap_cnt Count, 'Overlap' from conv
  union
 select LangCd, conv, conv_length, single_speaker_dur Duration, null Count, 'Single Speaker' from conv
  union
 select LangCd, conv, conv_length, 0 Duration, zero_cnt Count, 'Zero gap' Type from conv
")
conv_long$LangCd <- as.factor(conv_long$LangCd)
conv_long$Type <- as.factor(conv_long$Type)
#reorder languages by pct single speaker
df<-sqldf("
 select LangCd 
   from conv
   group by LangCd
  order by sum(single_speaker_dur)/sum(conv_length)
")
conv_long$LangCd2 <-factor(conv_long$LangCd,levels=df$LangCd,ordered=TRUE )

conv_long2<-sqldf("
  select LangCd, conv, conv_length, Type, Count,
    case when Type = 'Overlap' then Duration * -1 else Duration end Duration
   from conv_long
  where Type in ('Gap','Overlap')
")

conv_end<-sqldf("
 select subcorpus, conv, max(end) max_end
  from sound
  group by subcorpus, conv
")

ended_conv<-sqldf("select a.*
 from conv a
   join conv_end b 
    on a.subcorpus = b.subcorpus and a.conv = b.conv
 where round(b.max_end/60,1) not in (15,30)
")

##minute
#subtract min_begin from begin_sound because we want to count from when they actually start speaking, not the beginning of the recording
trans$adj_begin<-(trans$begin_sound - trans$min_begin)

#calculate transition begin as the beginning of the sound unless it's a positive gap, in which case the gap needs to be subtracted from the beginning of the sound
trans<-sqldf("
 select *,
  case debug
    when 'h' then adj_begin - gap
    else adj_begin
  end trans_begin
  from trans
")

trans$minute<-floor((trans$trans_begin)/60)

minute<-sqldf("
 select x.*, c.min_begin, c.max_end
 from
 (
 select *, max(max_minute_begin_sound) over (partition by subcorpus, conv) max_begin
 from 
 (
 select subcorpus, conv, minute, count(1) trans_cnt,
  sum(case when gap < 0 then 1 else 0 end) overlap_cnt, 
  sum(case when gap < 0 then abs(gap) else 0 end) overlap_dur,
  sum(case when gap > 0 then gap else 0 end) gap_dur,
  sum(case when gap > 0 then 1 else 0 end) gap_cnt,
  max(begin_sound) max_minute_begin_sound
 from trans
  where params='_30_200_1.25'
  group by subcorpus, conv, minute
 )
 ) x 
  join conv c
    on x.subcorpus = c.subcorpus and x.conv = c.conv 
")

#repeat rows for each minute
#tmpdf<- conv[rep(row.names(conv), trunc(conv$max_end/60)),]
tmpdf<- conv[rep(row.names(conv), ceiling((conv$max_end)/60)),]
#create a row number called minute for each of the repeated rows
group_cols <- c('subcorpus', 'conv')
tmpdf2 <- tmpdf %>% group_by(across(all_of(group_cols))) %>% mutate(minute = row_number()-1)
#join to minute
#this creates a row with zeros for any minute without a transition 
all_minute0<-sqldf("
 select *, trans_cnt / dur trans_rate, overlap_cnt / trans_cnt overlap_trans_ratio,
  overlap_cnt / dur overlap_rate, overlap_dur / dur overlap_dur_ratio,
  gap_cnt / dur gap_rate, gap_dur / dur gap_dur_ratio
 from
 (
 select x.subcorpus, x.conv, x.minute, x.Corpus, x.LangCd, x.RegionLangCd,
   count(1) conv_cnt,
   case minute 
     when 0 then 'beg' 
     else 'rest'
   end min1, 
   case minute
     when 0 then 'b0'
     when 1 then 'b1'
     when 2 then 'b2'
     when 3 then 'b3'
     else 'rest'
   end min4,
   case minute 
     when 0 then 'beg' 
     when 1 then 'beg2'
     when 2 then 'beg2'
     when 3 then 'beg2'
     else 'rest'
   end beg_cat,  
   cast(sum(x.trans_cnt) as float) trans_cnt, 
   cast(sum(x.overlap_cnt) as float) overlap_cnt, 
   sum(x.overlap_dur) overlap_dur,
   cast(sum(x.gap_cnt) as float) gap_cnt, 
   sum(x.gap_dur) gap_dur,
   case when minute = max_minute then max_end - (60 * minute)
    else 60
   end dur,
   ended_conv, max_minute
  from
  (
  select n.subcorpus, n.conv, n.minute, 
        n.Corpus, n.LangCd, n.RegionLangCd, n.conv_length,
        ifnull(m.trans_cnt,0) trans_cnt, 
        ifnull(m.overlap_cnt,0) overlap_cnt, ifnull(m.overlap_dur,0) overlap_dur,
        ifnull(m.gap_dur,0)     gap_dur,     ifnull(m.gap_cnt,0) gap_cnt,
        max(n.minute) over (partition by n.subcorpus, n.conv) max_minute,
        (select 1 from ended_conv e where e.subcorpus = n.subcorpus and e.conv = n.conv) ended_conv,
        m.max_end
   from tmpdf2 n
    left join minute m 
      on m.subcorpus = n.subcorpus and m.conv = n.conv and m.minute = n.minute
 ) x
  group by x.subcorpus, x.conv, x.minute
 )
")

all_minute<-sqldf("
 select * 
  from all_minute0
  where minute < 
    case 
     when ended_conv = 1 then max_minute -1 
      else max_minute 
    end
")
all_minute$LangCd <- as.factor(all_minute$LangCd )
all_minute$subcorpus <- as.factor(all_minute$subcorpus )
all_minute$overlap_trans_ratio<-all_minute$overlap_cnt/all_minute$trans_cnt
all_minute$beg_cat <- factor(all_minute$beg_cat,levels=c('rest','beg','beg2'))
all_minute$min1 <- factor(all_minute$min1,levels=c('rest','beg'))
all_minute$min4 <- factor(all_minute$min4,levels=c('rest','b0','b1','b2','b3'))
all_minute$minute_f <- factor(all_minute$minute,levels=c('28','1',
'2',
'3',
'4',
'5',
'6',
'7',
'8',
'9',
'10',
'11',
'12',
'13',
'14',
'15',
'16',
'17',
'18',
'19',
'20',
'21',
'22',
'23',
'24',
'25',
'26',
'27'))


#transitions from ended conversations
ended_trans<-sqldf("
 select a.subcorpus, a.conv, a.ch, a.gap, a.LangCd, a.adj_begin, a.trans_begin, a.minute, c.conv_length
  from trans a
   join ended_conv c
    on c.conv = a.conv
   where a.params = '_30_200_1.25' 
")
ended_trans$min_from_end<-ceiling(((ended_trans$conv_length - ended_trans$trans_begin)/60)*-1)
ended_trans$LangCd <- as.factor(ended_trans$LangCd )
ended_trans$subcorpus <- as.factor(ended_trans$subcorpus )

end_min0<-sqldf("
 select subcorpus, conv, min_from_end, count(1) trans_cnt,
  sum(case when gap < 0 then 1 else 0 end) overlap_cnt, 
  sum(case when gap < 0 then abs(gap) else 0 end) overlap_dur,
  sum(case when gap > 0 then gap else 0 end) gap_dur,
  sum(case when gap > 0 then 1 else 0 end) gap_cnt
 from ended_trans
  --ignore partial first minute  
  where trans_begin > conv_length % 60
  group by subcorpus, conv, min_from_end
")

#repeat rows for each minute from the end
tmpdf<- ended_conv[rep(row.names(ended_conv), trunc(ended_conv$max_end/60)-1),]
#create a row number called min_from_end for each of the repeated rows
group_cols <- c('subcorpus', 'conv')
tmpdf2 <- tmpdf %>% group_by(across(all_of(group_cols))) %>% mutate(min_from_end = (row_number()-1)*-1)
#join to end_min0
end_min<-sqldf("
 select subcorpus, conv, min_from_end, Corpus, LangCd,
   count(1) conv_cnt,
   case min_from_end
     when 0 then 'fnl' 
     else 'rest'
   end fnl2way,
   case min_from_end
     when 0 then 'fnl' 
     when -1 then 'fnl-1'
     else 'rest'
   end fnl3way,
   sum(trans_cnt) trans_cnt, 
   sum(overlap_cnt) overlap_cnt, 
   sum(overlap_dur) overlap_dur,
   sum(gap_cnt) gap_cnt, 
   sum(gap_dur) gap_dur
  from
  (
  select n.subcorpus, n.conv, n.min_from_end, 
        n.Corpus, n.LangCd, n.RegionLangCd, n.conv_length,
        ifnull(m.trans_cnt,0) trans_cnt, 
        ifnull(m.overlap_cnt,0) overlap_cnt, ifnull(m.overlap_dur,0) overlap_dur,
        ifnull(m.gap_dur,0)     gap_dur,     ifnull(m.gap_cnt,0) gap_cnt
   from tmpdf2 n
    left join end_min0 m 
      on m.subcorpus = n.subcorpus and m.conv = n.conv and m.min_from_end = n.min_from_end
 )
 group by subcorpus, conv, min_from_end
")
end_min$LangCd <- as.factor(end_min$LangCd )
end_min$subcorpus <- as.factor(end_min$subcorpus )
end_min$overlap_trans_ratio<-end_min$overlap_cnt/end_min$trans_cnt
end_min$fnl2way <- factor(end_min$fnl2way,levels=c('rest','fnl'))
end_min$fnl3way <- factor(end_min$fnl3way,levels=c('rest','fnl-1','fnl'))

#combine beg and end
beg_fnl<-sqldf("
 select b.*, 
   cast(trans_cnt as float) / dur trans_rate, 
   cast(overlap_dur as float) / dur overlap_dur_ratio,
   cast(gap_dur as float)/ dur gap_dur_ratio
 from
 (
 select a.subcorpus, a.LangCd, a.conv, a.begfnl,
  count(1) trans_cnt,
  sum(case when gap < 0 then 1 else 0 end) overlap_cnt, 
  sum(case when gap < 0 then abs(gap) else 0 end) overlap_dur,
  sum(case when gap > 0 then gap else 0 end) gap_dur,
  sum(case when gap > 0 then 1 else 0 end) gap_cnt,
  case begfnl
   --subtract 60 for each of the buckets in the inner query below
   when 'rest' then conv_length - 240
   else 60
  end dur
 from
 (
  select *, 
   case when minute = 0 then 'beg' 
        when minute = 1 then 'beg+1'
        when min_from_end = -1 then 'last-1'
        when min_from_end = 0 then 'last' 
        else 'rest' end begfnl
  from ended_trans
 ) a
 group by a.subcorpus, a.LangCd, a.conv, a.begfnl
 ) b
")
beg_fnl$begfnl <- factor(beg_fnl$begfnl,levels=c('beg','beg+1','beg+2','rest','last-2','last-1','last'))
beg_fnl$begfnl2 <- factor(beg_fnl$begfnl,levels=c('rest','beg','beg+1','beg+2','last-2','last-1','last'))

#pretty version of params variable
trans$Parameters<-trans$params
levels(trans$Parameters)[levels(trans$Parameters)=='_30_200_1.25'] <- 'Sound: 30 ms\nSilence: 200 ms'
levels(trans$Parameters)[levels(trans$Parameters)=='_50_50_1.25'] <- 'Sound: 50 ms\nSilence: 50 ms'

#refilter ig
#take out excluded conv's and gaps after last transition
ig<-sqldf("
 select ig.*
  from ig
   join conv on
    conv.subcorpus = ig.subcorpus and
    conv.params = ig.params and
    conv.conv = ig.conv
  where ig.begin_sound < conv.conv_length
")
ig$params <- as.factor(ig$params)

#all gaps together, excluding those less than the minimum intraturn gap
all_gaps<-sqldf("
select a.*, b.Corpus, b.LangCd, b.RegionLangCd
from 
(select t.subcorpus, t.params, t.conv, t.ch, t.begin_sound, t.gap, 'inter' type
  from trans t 
    join conv c on
     c.subcorpus = t.subcorpus and
     c.params = t.params and
     c.conv = t.conv
  where t.gap >= 0.2
 union all
 select subcorpus, params, conv, ch, begin_sound, intraturn_gap, 'intra'
  from ig
) a
 join codes b
   on b.Code = a.subcorpus
 where b.Corpus in ('CallHome','CallFriend')
")
all_gaps$subcorpus <- as.factor(all_gaps$subcorpus)
all_gaps$LangCd <- as.factor(all_gaps$LangCd)
all_gaps$params <- as.factor(all_gaps$params)
all_gaps$Corpus <- as.factor(all_gaps$Corpus)

all_gaps_sum<-sqldf("
 select *, sum(gap) over (partition by LangCd) tot_gap
 from
 (
 select LangCd, type, sum(gap) gap, count(1) count
   from all_gaps
   where params='_30_200_1.25'
   group by LangCd, type
 )
")
#reorder languages by pct intra
df<-sqldf("
 select LangCd, gap/tot_gap
  from all_gaps_sum
  where type = 'intra'
  group by LangCd
  order by 2
")
all_gaps_sum$LangCd2 <-factor(all_gaps_sum$LangCd,levels=df$LangCd,ordered=TRUE )


all_minute_long<-sqldf("
 select LangCd, conv, minute, gap_dur Duration, gap_cnt Count, 'Gap' Type from all_minute
  union
 select LangCd, conv, minute, overlap_dur Duration, overlap_cnt Count, 'Overlap' from all_minute
")
all_minute_long$LangCd <- as.factor(all_minute_long$LangCd)
all_minute_long$Type <- as.factor(all_minute_long$Type)


##cleanup
rm(df)
rm(f)
rm(tmpdf)
rm(tmpdf2)
rm(same_pitch)
rm(all_conv)
rm(igmax)
