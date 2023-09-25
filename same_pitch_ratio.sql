select subcorpus_cd, conv_id, 
  greatest(no_lag_ratio, 
    lag01_ratio, lag02_ratio, lag03_ratio, lag04_ratio, lag05_ratio, lag06_ratio, lag07_ratio,
	lead01_ratio, lead02_ratio, lead03_ratio, lead04_ratio, lead05_ratio, lead06_ratio, lead07_ratio)
from
(
select subcorpus_cd, conv_id, 
  close_no_lag_cnt / ch_cnt no_lag_ratio,
  close_lag01_cnt / ch_cnt lag01_ratio,
  close_lag02_cnt / ch_cnt lag02_ratio,
  close_lag03_cnt / ch_cnt lag03_ratio,
  close_lag04_cnt / ch_cnt lag04_ratio,
  close_lag05_cnt / ch_cnt lag05_ratio,
  close_lag06_cnt / ch_cnt lag06_ratio,
  close_lag07_cnt / ch_cnt lag07_ratio,
  close_lead01_cnt / ch_cnt lead01_ratio,
  close_lead02_cnt / ch_cnt lead02_ratio,
  close_lead03_cnt / ch_cnt lead03_ratio,
  close_lead04_cnt / ch_cnt lead04_ratio,
  close_lead05_cnt / ch_cnt lead05_ratio,
  close_lead06_cnt / ch_cnt lead06_ratio,
  close_lead07_cnt / ch_cnt lead07_ratio
from
(
select subcorpus_cd, conv_id, cast(count(1) as float) ch_cnt, --count(no_lag), 
   --count(lag01) lag01,count(lag02) lag02,count(lag03) lag03,count(lag04) lag04,count(lag05) lag05,count(lag06) lag06,count(lag07) lag07,
   --count(lead01) lead01,count(lead02) lead02,count(lead03) lead03,count(lead04) lead04,count(lead05) lead05,count(lead06) lead06,count(lead07) lead07,
   sum(case when no_lag < 5 then 1 else 0 end) close_no_lag_cnt,
   sum(case when lag01 < 5 then 1 else 0 end) close_lag01_cnt,
   sum(case when lag02 < 5 then 1 else 0 end) close_lag02_cnt,
   sum(case when lag03 < 5 then 1 else 0 end) close_lag03_cnt,
   sum(case when lag04 < 5 then 1 else 0 end) close_lag04_cnt,
   sum(case when lag05 < 5 then 1 else 0 end) close_lag05_cnt,
   sum(case when lag06 < 5 then 1 else 0 end) close_lag06_cnt,
   sum(case when lag07 < 5 then 1 else 0 end) close_lag07_cnt,   
   sum(case when lead01 < 5 then 1 else 0 end) close_lead01_cnt,
   sum(case when lead02 < 5 then 1 else 0 end) close_lead02_cnt,
   sum(case when lead03 < 5 then 1 else 0 end) close_lead03_cnt,
   sum(case when lead04 < 5 then 1 else 0 end) close_lead04_cnt,
   sum(case when lead05 < 5 then 1 else 0 end) close_lead05_cnt,
   sum(case when lead06 < 5 then 1 else 0 end) close_lead06_cnt,
   sum(case when lead07 < 5 then 1 else 0 end) close_lead07_cnt
from
(
 select a.subcorpus_cd, a.conv_id, a.ch, a.pt, a.freq, 
   abs(a.freq - nolag.freq) no_lag, 
   abs(a.freq - lag01.freq) lag01,
   abs(a.freq - lag02.freq) lag02,
   abs(a.freq - lag03.freq) lag03,
   abs(a.freq - lag04.freq) lag04,
   abs(a.freq - lag05.freq) lag05,
   abs(a.freq - lag06.freq) lag06,
   abs(a.freq - lag07.freq) lag07,
   abs(a.freq - lead01.freq) lead01,
   abs(a.freq - lead02.freq) lead02,
   abs(a.freq - lead03.freq) lead03,
   abs(a.freq - lead04.freq) lead04,
   abs(a.freq - lead05.freq) lead05,
   abs(a.freq - lead06.freq) lead06,
   abs(a.freq - lead07.freq) lead07
   --sum(case when abs(a.freq - nolag.freq) < 5 then 1 else 0 end) close_no_lag_cnt,
 from c_pitch a
   left join c_pitch nolag
     on a.subcorpus_cd = nolag.subcorpus_cd and 
      a.conv_id = nolag.conv_id and
	  nolag.ch = 1 and
	  a.pt = nolag.pt
   left join c_pitch lead01
     on a.subcorpus_cd = lead01.subcorpus_cd and 
      a.conv_id = lead01.conv_id and
	  lead01.ch = 1 and
	  (a.pt ) = ((lead01.pt ) - 10)
   left join c_pitch lead02
     on a.subcorpus_cd = lead02.subcorpus_cd and 
      a.conv_id = lead02.conv_id and
	  lead02.ch = 1 and
	  (a.pt ) = ((lead02.pt ) - 20)
   left join c_pitch lead03
     on a.subcorpus_cd = lead03.subcorpus_cd and 
      a.conv_id = lead03.conv_id and
	  lead03.ch = 1 and
	  (a.pt ) = ((lead03.pt ) - 30)
   left join c_pitch lead04
     on a.subcorpus_cd = lead04.subcorpus_cd and 
      a.conv_id = lead04.conv_id and
	  lead04.ch = 1 and
	  (a.pt ) = ((lead04.pt ) - 40)
   left join c_pitch lead05
     on a.subcorpus_cd = lead05.subcorpus_cd and 
      a.conv_id = lead05.conv_id and
	  lead05.ch = 1 and
	  (a.pt ) = ((lead05.pt ) - 50)
   left join c_pitch lead06
     on a.subcorpus_cd = lead06.subcorpus_cd and 
      a.conv_id = lead06.conv_id and
	  lead06.ch = 1 and
	  (a.pt ) = ((lead06.pt ) - 60)
   left join c_pitch lead07
     on a.subcorpus_cd = lead07.subcorpus_cd and 
      a.conv_id = lead07.conv_id and
	  lead07.ch = 1 and
	  (a.pt ) = ((lead07.pt ) - 70)
   left join c_pitch lag01
     on a.subcorpus_cd = lag01.subcorpus_cd and 
      a.conv_id = lag01.conv_id and
	  lag01.ch = 1 and
	  (a.pt ) = ((lag01.pt ) + 10)
   left join c_pitch lag02
     on a.subcorpus_cd = lag02.subcorpus_cd and 
      a.conv_id = lag02.conv_id and
	  lag02.ch = 1 and
	  (a.pt ) = ((lag02.pt ) + 20)
   left join c_pitch lag03
     on a.subcorpus_cd = lag03.subcorpus_cd and 
      a.conv_id = lag03.conv_id and
	  lag03.ch = 1 and
	  (a.pt ) = ((lag03.pt ) + 30)
   left join c_pitch lag04
     on a.subcorpus_cd = lag04.subcorpus_cd and 
      a.conv_id = lag04.conv_id and
	  lag04.ch = 1 and
	  (a.pt ) = ((lag04.pt ) + 40)
   left join c_pitch lag05
     on a.subcorpus_cd = lag05.subcorpus_cd and 
      a.conv_id = lag05.conv_id and
	  lag05.ch = 1 and
	  (a.pt ) = ((lag05.pt ) + 50)
   left join c_pitch lag06
     on a.subcorpus_cd = lag06.subcorpus_cd and 
      a.conv_id = lag06.conv_id and
	  lag06.ch = 1 and
	  (a.pt ) = ((lag06.pt ) + 60)
   left join c_pitch lag07
     on a.subcorpus_cd = lag07.subcorpus_cd and 
      a.conv_id = lag07.conv_id and
	  lag07.ch = 1 and
	  (a.pt ) = ((lag07.pt ) + 70)
  where --a.subcorpus_cd = 'CFspa-c' and a.conv_id = '6863' and
    a.ch = 2 
 ) x
 group by subcorpus_cd, conv_id
 ) y
 ) z
 order by 3