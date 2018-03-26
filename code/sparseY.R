#每首歌比例>0.2的sentiment
songsent=song %>% 
  transmute(serial = 1:5100,
            year = rep(1965:2015, each = 100),
            text = Lyrics) %>%
  unnest_tokens(word, text)%>%
  inner_join(get_sentiments("nrc"))%>%
  group_by(serial,year,sentiment)%>%
  summarise(count=n())%>%
  arrange(serial,year,desc(count))%>%
  select(year,everything())%>%
  group_by(year,serial)%>%
  mutate(perc=count/sum(count))%>%
  filter(perc>0.2)
head(songsent)
#稀疏矩阵
dtm.s=cast_sparse(songsent, serial, sentiment)
head(dtm.s)