cd "/Users/td/Documents/repos/hospital-transfers-multipackage"
import delimited "simple.csv", clear

sort id adm sep

gen adm_tmp = adm
gen sep_tmp = sep
rename adm time1
rename sep time2

gen fileseq = _n
by id: gen episode=_n

reshape long time, i(id transfer episode) j(var)
gen variable = "adm" if var==1
replace variable = "sep" if var==2
drop var

gen time_adj = time + 0.1 if (variable == "sep" & transfer==1)
replace time_adj = time if time_adj == .

gen inout = 1 if variable == "adm"
replace inout = -1 if variable == "sep"

gsort id time_adj episode -inout

by id: gen cumsum = sum(inout)
by id: gen newstay = 1 if (cumsum==1 & _n==1 | cumsum[_n-1]==0)
replace newstay = 0 if newstay == .
by id: gen stayseq = sum(newstay)


*temp_trans <- dcast(temp_melt, id + episode + transfer + stayseq  ~ variable, value.var="time") %>% 
*  arrange(id, episode) %>% 
*  group_by(id, stayseq) %>% 
*  mutate(transseq = row_number() - 1,
*         sepdate_fin = max(sep_tmp)) %>% 
*  rename(admdate = adm_tmp, sepdate = sep_tmp)  %>% 
*  select(id, episode, admdate, sepdate, transfer, stayseq, transseq, sepdate_fin) %>% 
*  mutate(totlos = ifelse(sepdate_fin - admdate==0, 1, sepdate_fin - admdate))


list, sepby(id)


