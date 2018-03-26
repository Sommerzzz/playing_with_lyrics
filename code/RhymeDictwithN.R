rhyme.dict=read.csv("cmudict-0.7b.csv", sep=",", header=FALSE, stringsAsFactors=FALSE);
colnames(rhyme.dict)=c("word",paste("Phnm",seq(1,length(rhyme.dict)-1)));
rhyme.dict$word=sub(pattern="\\(.*\\)", "", x=rhyme.dict$word);
rhyme.dict$word=sub(pattern="\\d{1,}", "", x=rhyme.dict$word);

vowel=c("AA","AE","AH","AO","AW","AY","EH","ER","EY","IH","IY","NG","OW","OY","UH","UW","N","M","R","L");
consonant=c("B","CH","D","DH","F","G","HH","JH","K","L","M","N","P","R","S","SH","T","TH","V","W","Y","Z","ZH");

pair.phoneme=function(word){
  #word=data.frame(word=c("inks"),n=c(3),stringsAsFactors=FALSE);
  #word=data.frame(word=c("magician"),n=c(14),stringsAsFactors=FALSE);
  #word=data.frame(word=c("restaurant"),n=c(14),stringsAsFactors=FALSE);
  #word=data.frame(word=c("fear"),n=c(14),stringsAsFactors=FALSE);
  #word=data.frame(word=c("mule"),n=c(14),stringsAsFactors=FALSE);
  
  search.list=rhyme.dict %>%
    filter(word==toupper(word$word))
 
  if(length(search.list$word)!=0){
    result.phnm=c();
    result.type=c();
    result.location=c();
    
    for(j in 1:length(search.list$word)){
      phonem=c(search.list[j,])[-1];
      phonem=phonem[phonem!=""];
      phonem=gsub("\\d{1,}","",phonem);
      
      point=length(phonem);
      if(phonem[point] %in% vowel){
        tag="V";
      }else{
        tag="C"
      }
      
      i=1;
      while(i<=3){
        if(phonem[point] %in% c("NG","N","M","R","L")){
          if(tag=="V"){
            if(point==1){
              result.phnm=append(result.phnm,phonem[point]);
              result.type=append(result.type,"C");
              result.location=append(result.location,i-1);
              return(data.frame(phnm=result.phnm,
                                location=result.location,
                                type=result.type,
                                stringsAsFactors=FALSE));
            }#end if(point==1)
            phnm.paste="";
            while(phonem[point] %in% c("NG","N","M","R","L")){
              phnm.point=gsub("NG|N|M","-N",phonem[point]);
              phnm.paste=paste0(phnm.point,phnm.paste,sep="");
              point=point-1;
            }#end refreshing phnm.paste in c("NG","N","M","R","L")
            phnm.paste=paste(phonem[point],phnm.paste);
            result.phnm=append(result.phnm,phnm.paste);
            result.type=append(result.type,"V");
            result.location=append(result.location,i);
            point=point-1;
            tag="C";
          }
          else{
            while(phonem[point] %in% consonant){
              result.phnm=append(result.phnm,phonem[point]);
              result.location=append(result.location,i);
              result.type=append(result.type,"C");
              point=point-1;
              tag="V";
              
              if(point==0){
                return(data.frame(phnm=result.phnm,
                                  location=result.location,
                                  type=result.type,
                                  stringsAsFactors=FALSE));
              }#end if(point==0)
            }#end while(phonem[point] %in% consonant)
          }#end else (tag=="C")
        }else if(phonem[point] %in% consonant){
          while(phonem[point] %in% consonant & !(phonem[point] %in% c("N","M","R","L"))){
            result.phnm=append(result.phnm,phonem[point]);
            result.location=append(result.location,i);
            result.type=append(result.type,"C");
            point=point-1;
            
            if(point==0){
              return(data.frame(phnm=result.phnm,
                                location=result.location,
                                type=result.type,
                                stringsAsFactors=FALSE));
            }#end if(point==0)
          }#end while(phonem[point] %in% consonant)
          tag="V";
        }else{
          result.phnm=append(result.phnm,phonem[point]);
          result.location=append(result.location,i);
          result.type=append(result.type,"V");
          point=point-1;
          tag="C";
        }#end else (phonem[point] %in% vowel)
        
        if(point==0){
          return(data.frame(phnm=result.phnm,
                            location=result.location,
                            type=result.type,
                            stringsAsFactors=FALSE));
        }else{
          i=i+1;
        }#end if(point==0)
        
      }#end while(i<=3) 
      
    }# end for(j in 1:length(search.list$word))
    
    return(data.frame(phnm=result.phnm,
                      location=result.location,
                      type=result.type,
                      stringsAsFactors=FALSE));
  }else{
      return(NULL);
  }
}

phoneme.rhyme=function(lyric){
  word.select=c();
  for(i in 1:length(lyric)){
    word.list=unlist(strsplit(lyric[i]," "));
    word.list=gsub("[^a-zA-Z\']","",word.list);
    word.select=append(word.select,word.list[length(word.list)]);
  }
  
  word.list=data.frame(word=word.select)%>%filter(word!="")
  word.sum=word.list %>%
    group_by(word) %>%
    summarise(n=n());
  
  result.list=list();
  for(i in 1:length(word.list$word)){
    result.list[[i]]=pair.phoneme(data.frame(word=word.list[i,],n=1));
  }
  
  result.sum=list();
  for(i in 1:length(word.sum$word)){
    result.sum[[i]]=pair.phoneme(word.sum[i,]);
  }
  
  result=list();
  result$word.list=word.list;
  result$phnm.list=result.list;
  result$word.sum=word.sum;
  result$phnm.sum=result.sum;
  return(result);
}

phoneme.song=function(lyric){
  word.select=c();
  for(i in 1:length(lyric)){
    word.list=unlist(strsplit(lyric[i]," "));
    word.list=gsub("[^a-zA-Z\']","",word.list);
    word.select=append(word.select,tolower(word.list));
  }
  
  word.sum=data.frame(word=word.select) %>%
    filter(word!="") %>%
    group_by(word) %>%
    summarise(n=n());
  
  result.sum=list();
  for(i in 1:length(word.sum$word)){
    result.sum[[i]]=pair.phoneme(word.sum[i,]);
  }
  
  result=list();
  result$word.sum=word.sum;
  result$phnm.sum=result.sum;
  return(result);
}
