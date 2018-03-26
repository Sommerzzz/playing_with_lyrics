create.Scorelist=function(Lyrics.list,sample=0){
  lyrics.list=Lyrics.list$lyrics.list;
  songs.catalog=Lyrics.list$songs.catalog;
  
  score.list=list();
  score.songs=data.frame();
  
  if(sample==0){
    r=seq(1,length(lyrics.list));
  }else{
    #r=as.integer(seq(1,length(lyrics.list),length(lyrics.list)/sample));
    r=sample;
  }#end if(sample==0)
  
  i=1;
  for(song.num in r){
    lyric=lyrics.list[[song.num]];
    phonemeRhyme=phoneme.rhyme(lyric);
    phonemeSong=phoneme.song(lyric);
    score.list[[i]]=rhymeGroup(phonemeRhyme,phonemeSong);
    score.songs=rbind(score.songs,songs.catalog[song.num,1:4]);
    i=i+1;
    print(paste("Year",songs.catalog$Year[song.num],"Song",songs.catalog$Rank[song.num],"succeed.No.",song.num));
  }#end creating score.list
  
  result.list=list();
  result.list$score.list=score.list;
  result.list$score.songs=score.songs;
  return(result.list);
}

Score.df=function(Score.list){
  score.list=Score.list$score.list;
  score.songs=Score.list$score.songs;
  Rhyme=c();
  RhymeScore=c();
  Score.dist=data.frame();
  for(i in 1:length(score.list)){
    if(sum(score.list[[i]]$pct)>2){
      Rhyme=append(Rhyme,1);
      Score=compute.score(score.list[[i]]);
      RhymeScore=append(RhymeScore,Score$score);
      Score.dist.app=data.frame(phnm=score.list[[i]]$phnm,
                                score=Score$score.dist,stringsAsFactors=FALSE) %>%
        group_by(phnm)%>%
        summarise(score=sum(score));
      len=length(Score.dist.app$phnm);
      Score.dist.app=Score.dist.app %>%
        mutate(Year=rep(score.songs$Year[i],len),
               Rank=rep(score.songs$Rank[i],len),
               Song=rep(score.songs$Song[i],len))
      Score.dist=rbind(Score.dist,Score.dist.app);
    }else{
      Rhyme=append(Rhyme,0);
      RhymeScore=append(RhymeScore,0);
    }#end  if(sum(score.list[[i]]$pct)>0.33)
  }#end for(i in 1:length(Score.list))
  
  result=list();
  result$score.songs=cbind(score.songs,Rhyme,RhymeScore);
  result$score.dist=Score.dist;
  return(result);
}

compute.score=function(score.df){
  score=c();
  for(i in 1:nrow(score.df)){
    avgloc=score.df$avgloc[i];
    if(score.df$type[i]=="C"){
      if(avgloc==1){
        score=append(score,3*score.df$n[i]);
      }else{
        score=append(score,((2-avgloc)*2+(avgloc-1)*1)*score.df$n[i]);
      }#end if(avgloc==1)
    }else{
      score=append(score,((2-avgloc)*5+(avgloc-1)*4)*score.df$n[i]);
    }#end if(score.df$type[i]=="C")
  }#end for(i in 1:nrow(score.df))
  pct.temp=score.df$pct/sum(score.df$pct);
  Score=pct.temp*score;
  
  result=list();
  result$score=sum(Score);
  result$score.dist=Score;
  return(result);
}