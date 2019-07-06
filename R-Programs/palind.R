palind=function(word){
  charsplit<-strsplit(word,"")[[1]]
  revchar<-rev(charsplit)
  all(charsplit==revchar)
}
palind("love")

