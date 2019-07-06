# max function

maxvar<-function(x){ 
  maxa=x[1]
  for (i in 1:length(x)) {
    if(maxa>x[i]){
      maxa=maxa
    }else{
      maxa=x[i]
    }
  }
  return(maxa)
}

maxvar(a)

# test

b<-c(1:200)
maxvar(b)
c<-c(0,3,-0.5)
c
maxvar(c)
d<-matrix(nrow = 2,ncol = 2,2)
maxvar(d)

