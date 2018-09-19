library(tidyverse)
library(reshape2)

temp <- read_csv(file="simple.csv") %>% 
  rename(sepdate=sep, admdate=adm) %>% 
  mutate(fileseq = row_number()) %>% 
  group_by(id) %>% 
  mutate(episode = row_number(),
         adm_tmp = admdate,
         sep_tmp = sepdate)


# Begin working on transfers ----------------------------------------------
temp_melt <- melt(temp, 
     id.vars=c("id", "transfer", "episode", "admdate", "sepdate"), 
     measure.vars = c("adm_tmp", "sep_tmp"), value.name="time") %>% 
  mutate(time_adj = case_when(
    variable == "sep_tmp" & transfer==1 ~ time + 0.1,
    TRUE ~ as.numeric(time))) %>% 
  mutate(inout = ifelse(variable=="adm_tmp", 1, -1)) %>% 
  arrange(id, time_adj, episode, -inout) %>% 
  group_by(id) %>% 
  mutate(cumsum = cumsum(inout),
         newstay = as.numeric(cumsum==1 & (row_number()==1 | lag(cumsum)==0)),
         stayseq = cumsum(newstay))

#filter(temp_melt, id %in% c(7,9))

temp_trans <- dcast(temp_melt, id + episode + transfer + stayseq  ~ variable, value.var="time") %>% 
  arrange(id, episode) %>% 
  group_by(id, stayseq) %>% 
  mutate(transseq = row_number() - 1,
         sepdate_fin = max(sep_tmp)) %>% 
  rename(admdate = adm_tmp, sepdate = sep_tmp)  %>% 
  select(id, episode, admdate, sepdate, transfer, stayseq, transseq, sepdate_fin) %>% 
  mutate(totlos = ifelse(sepdate_fin - admdate==0, 1, sepdate_fin - admdate))

# filter(temp_trans, id %in% c(7,9))

# filter(temp_trans, transseq == 0) %>% 
#   ungroup %>% 
#   summarise(mean = mean(totlos))

