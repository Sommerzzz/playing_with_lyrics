rhymeGroup=function(phonemeRhyme,phonemeSong){
  phnm.listdf=phnm.dfcontruct(phonemeRhyme$phnm.list);
  phnm.sumV12=phnm.summary(phnm.listdf%>%filter(type=="V"),c(1,2));
  phnm.sumV12=clear.rhyme(phnm.sumV12,phonemeRhyme$phnm.list,phonemeSong,c(1,2));
  phnm.sumC23=phnm.summary(phnm.listdf%>%filter(type=="C"),c(2,3));
  phnm.sumC23=clear.rhyme(phnm.sumC23,phonemeRhyme$phnm.list,phonemeSong,c(2,3));
  phnm.sumC1=phnm.summary(phnm.listdf%>%filter(type=="C"),1);
  phnm.sumC1=clear.rhyme(phnm.sumC1,phonemeRhyme$phnm.list,phonemeSong,1);
  phnm.sum=rbind(phnm.sumV12,phnm.sumC23,phnm.sumC1);
  return(phnm.sum)
}

phnm.dfcontruct=function(phnm.list){
  result.df=data.frame(stringsAsFactors=FALSE);
  for (i in 1:length(phnm.list)){
    result.df=rbind(result.df,phnm.list[[i]]);
  }
  return(result.df);
}

phnm.dfinline=function(phnm.list){
  result.df=data.frame(stringsAsFactors=FALSE);
  for (i in 1:length(phnm.list)){
    l=cbind(phnm.list[[i]],
            line=rep(i,length(phnm.list[[i]]$phnm)));
    result.df=rbind(result.df,l);
  }
  return(result.df);
}

phnmRN.rhyme=function(phnm.list,phnm.sum){
  vowel=length(grep(unlist(strsplit(phnm.sum," "))[1],phnm.list));
  tail=length(grep(unlist(strsplit(phnm.sum," "))[2],phnm.list));
  if(vowel!=0 & tail!=0){
    return(TRUE);
  }else{
    return(FALSE);
  }
}

rhyme.lineN=function(phnm.df,point){
  pointTag=length(grep(" ",point));
  if(pointTag>0){
    row.selected=c();
    for(j in 1:length(phnm.df$phnm)){
      if(phnmRN.rhyme(phnm.df$phnm[j],point)){
        row.selected=append(row.selected,j);
      }#end calling phnmRN.rhyme()
    }#end for(j in 1:length(phnm.list))
  }else{
    row.selected=grep(point,phnm.df$phnm);
  }
  loc.selected=phnm.df$location[row.selected];
  
  result=list();
  result$row.selected=row.selected;
  result$loc.selected=loc.selected;
  return(result);
}

phnm.summary=function(phnm.dataframe,loc.selected){
  phnm.select=phnm.dataframe %>%
    filter(phnm!="" & location %in% loc.selected) %>%
    group_by(phnm,type) %>%
    summarise(n=n(),
              avgloc=sum(location)/n());

  phnm.sure=as.data.frame(phnm.select);
  phnm.collected=data.frame();
  vowel.collected=c();
  col.collect=grep(" ",phnm.select$phnm);
  if (length(col.collect)>0){
    phnm.sure=as.data.frame(phnm.select[-grep(" ",phnm.select$phnm),]);
    phnm.collect=as.data.frame(phnm.select[grep(" ",phnm.select$phnm),]);
    for(i in 1:length(phnm.collect$phnm)){
      vowel.candidate=unlist(strsplit(phnm.collect$phnm[1]," "))[1];
      
      if (!(vowel.candidate %in% vowel.collected)){
        vowel.collected=append(vowel.collected,vowel.candidate);
        phnm.collected=rbind(phnm.collected,data.frame(phnm=paste(vowel.candidate,"-N"),
                                                       type="V",n=0,avgloc=0,stringsAsFactors=FALSE));
        phnm.collected=rbind(phnm.collected,data.frame(phnm=paste(vowel.candidate,"R"),
                                                       type="V",n=0,avgloc=0,stringsAsFactors=FALSE));
        phnm.collected=rbind(phnm.collected,data.frame(phnm=paste(vowel.candidate,"L"),
                                                       type="V",n=0,avgloc=0,stringsAsFactors=FALSE)); 
      }
      
      for(j in 1:length(phnm.collected$phnm)){
        if(phnmRN.rhyme(phnm.collect$phnm[i],phnm.collected$phnm[j])){
          phnm.collected$n[j]=phnm.collected$n[j]+phnm.collect$n[i];
          phnm.collected$avgloc[j]=phnm.collected$avgloc[j]+phnm.collect$avgloc[i]*phnm.collect$n[i];
        }#end calling phnmRN.rhyme()
      }#end for(j in 1:length(collect.vowel))
      
    }#end for(i in 1:length(phnm.collect))
    phnm.collected=phnm.collected%>%filter(n!=0);
    phnm.collected$avgloc=phnm.collected$avgloc/phnm.collected$n;
  }#end if(length(phnm.collect$phnm)>0)
  
  result=rbind(phnm.sure,phnm.collected) %>% 
    arrange(desc(n)) %>%
    mutate(pct=n/sum(n));
  return(result);
}

distance.check=function(phnm.check,phnm.list,loc.selected){
  phnm.select=phnm.dfinline(phnm.list)%>%filter(location %in% loc.selected);
  result=data.frame(stringsAsFactors=FALSE);
  for(i in 1:length(phnm.check$phnm)){
    point=phnm.check$phnm[i];
    rhyme.num=rhyme.lineN(phnm.select,point);
    row.selected=phnm.select$line[rhyme.num$row.selected];
    distance=row.selected[-1]-row.selected[-length(row.selected)];
    if(length(distance[distance<3])!=0){
      result=rbind(result,phnm.check[i,]);
    }#end if(length(distance[distance<3])!=0)
  }#end for(i in 1:length(phnm.sum$phnm))
  return(result);
}

clear.rhyme=function(phnm.sum,phnm.list,phonemeSong,loc.selected){
  phnm.sure=phnm.sum%>% filter(n>3);
  phnm.check=phnm.sum%>% filter(n>1 & n<4);
  if(nrow(phnm.check)>0){
    phnm.check=distance.check(phnm.check,phnm.list,loc.selected);
  }
  phnm.refresh=rbind(phnm.sure,phnm.check);
  
  if(nrow(phnm.refresh)>0){
    phnm.select=phnm.dfinline(phonemeSong$phnm.sum)%>%filter(location %in% loc.selected);
    for(i in 1:length(phnm.refresh$phnm)){
      point=phnm.refresh$phnm[i];
      rhyme.num=rhyme.lineN(phnm.select,point);
      word.num=phnm.select$line[rhyme.num$row.selected];
      phnm.refresh$n[i]=sum(phonemeSong$word.sum$n[word.num]);
      phnm.refresh$avgloc[i]=sum(phonemeSong$word.sum$n[word.num]*rhyme.num$loc.selected)/phnm.refresh$n[i];
      phnm.refresh$n[i]=phnm.refresh$n[i]/sum(phonemeSong$word.sum$n);
    }
    return(phnm.refresh);
    
  }else{
    return(NULL);
  }#end if(nrow(phnm.refresh)>0)
}