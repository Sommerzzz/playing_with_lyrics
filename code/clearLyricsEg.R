songs.list=songs.list[1:5,];
songs.list$Lyrics=gsub("[^0-9a-zA-Z.,?! /&:;\'\n\t\\(\\)\\-]", "", songs.list$Lyrics);

clear.lyriclist=function(Lyrics.list){
  result=c();
  for(i in 1:length(Lyrics.list)){
    lyrics=Lyrics.list[i];
    lyric.list=unlist(strsplit(gsub("[/\n\t]", "\\^", lyrics), "\\^"));
    lyric.list=lyric.list[lyric.list!=""];
    
    if(length(lyric.list)<2){
      lyric.list=unlist(strsplit(gsub("[,./\n\t]", "\\^", lyrics), "\\^"))
    }
    lyric.list=lyric.list[lyric.list!=""];
    
    if(length(lyric.list)<2){
      result=append(result, i);
    }
  }
  
  return(result);
}

songs.list=songs.list[-clear.lyriclist(songs.list$Lyrics),];

clear.lyrics=function(lyrics){
  lyric.list=unlist(strsplit(gsub("[/\n\t]", "\\^", lyrics), "\\^"));
  lyric.list=lyric.list[lyric.list!=""];
  if(length(lyric.list)<2){
    lyric.list=unlist(strsplit(gsub("[,./\n\t]", "\\^", lyrics), "\\^"));
  }
  lyric.list=lyric.list[lyric.list!=""];
  
  upper=c("A","B","C","D","E","F","G","H","I","J","K","L","M","N","O","P","Q","R","S","T","U","V","W","X","Y","Z");
  lower=c("a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z");
  
  lyric.fixed=c();
  for(i in 1:length(lyric.list)){
    Flag=0;
    for(j in 1:nchar(lyric.list[i])){
      if((substring(lyric.list[i],j,j) %in% lower) & (substring(lyric.list[i],j+1,j+1) %in% upper)){
        Flag=j;
        break;
      }
    }
    if(Flag==0){
      lyric.fixed=append(lyric.fixed, lyric.list[i]);
    }else{
      lyric.fixed=append(lyric.fixed, substring(lyric.list[i],1,j));
      lyric.fixed=append(lyric.fixed, substring(lyric.list[i],(j+1),nchar(lyric.list[i])));
    }
  }
  
  return(lyric.fixed);
}

Lyrics.list=list();
for(i in 1:length(songs.list$Lyrics)){
  Lyrics.list[[i]]=clear.lyrics(songs.list$Lyrics[i]);
  print(paste("Year",songs.list$Year[i],"Song",songs.list$Rank[i],"succeed"));
}