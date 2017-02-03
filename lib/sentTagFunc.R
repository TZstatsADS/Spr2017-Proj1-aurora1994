sentTag<-function(fulltext){
  tense<-NULL
  for(j in 1:length(fulltext)){
    s <- as.String(fulltext[j])
    sent_token_annotator <- Maxent_Sent_Token_Annotator()
    word_token_annotator <- Maxent_Word_Token_Annotator()
    a1 <- annotate(s, list(sent_token_annotator, word_token_annotator))
    pos_tag_annotator <- Maxent_POS_Tag_Annotator()
    a2 <- annotate(s, pos_tag_annotator, a1)
    a3w <- subset(a2, type == "word")
    tags <- sapply(a3w$features,  `[[` , "POS")
    x<-sprintf("%s/%s", s[a3w], tags)
    r<-NULL
    for (i in 1:length(tags)){
     if (x[i]=="will/MD" && tags[i+1]=="VB" && tags[i+2]=="VBG"){
        r=c("Future Continuous")
        break
      }
      else if (x[i]=="will/MD" && tags[i+1]=="VB" && tags[i+2]=="VBN" && tags[i+3]!="VBG"){
        r=c("Future Perfect")
        break
      }
      else if (x[i]=="will/MD" && tags[i+1]=="VB" && tags[i+2]=="VBN" && tags[i+3]=="VBG"){
        r=c("Future Perfect Continuous")
        break
      }
      else if (x[i]=="will/MD" && tags[i+1]=="VB"){
        r=c("Simple Future")
        break
      }
      else if (tags[i]=="VBD" && tags[i+1]=="VBG"){
        r=c("Past Continuous")
        break
      }
      else if (tags[i]=="VBD" && tags[i+1]=="VBN" && tags[i+2]!="VBG"){
        r=c("Past Perfect")
        break
      }
      else if (tags[i]=="VBD" && tags[i+1]=="VBN" && tags[i+2]=="VBG"){
        r=c("Past Perfect Continuous")
        break
      }
      else if (tags[i]=="VBD"){
        r=c("Simple Past")
        break
      }
      else if ((tags[i]=="VBZ" ||tags[i]=="VBP") && tags[i+1]=="VBG"){
        r=c("Present Continuous")
        break
      }
      else if ((tags[i]=="VBZ" ||tags[i]=="VBP") && tags[i+1]=="VBN" && tags[i+2]!="VBG"){
        r=c("Present Perfect")
        break
      }
      else if ((tags[i]=="VBZ" ||tags[i]=="VBP") && tags[i+1]=="VBN" && tags[i+2]=="VBG"){
        r=c("Present Perfect Continuous")
        break
      }
      else if (tags[i]=="VBZ" || tags[i]=="VBP"){
        r=c("Simple Present")
      }
    }
    if (length(r)==0){r<-c("Undefined")}
    tense[j]=r
    gc()
  }
  return(tense)
}
  


