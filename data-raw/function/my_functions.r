# fill in median value into NA
fill.na<-function(df,n=0){
  dimm=dim(df)
  for (i in 1:dimm[1]) {
    nun_na<-df[i,!is.na(df[i,])]
    median_r=median(as.numeric(nun_na[2:length(nun_na)]))
    df[i,is.na(df[i,])]=median_r
  }
  return(df)
}
fill.na2<-function(df,n=0){
  dimm=dim(df)
  for (i in 1:dimm[2]) {
    nun_na<-df[!is.na(df[,i]),i]
    median_r=median(as.numeric(nun_na[2:length(nun_na)]))
    df[is.na(df[,i]),i]=median_r
  }
  return(df)
}

# Z-score normalization
Zscore<-function(x){y<-(x-mean(x))/sd(x); return(y)}

# delete NAs
del.na <- function(x){y<-x[!is.na(x)];return(y)}

# cut data into bins
cutbin<- function(x,nbins){
  if (require(ggplot2)) {
  labs<-levels(cut_number(x,nbins))
  options(warn=-1) # turning off warning messages globally
  bins = c(as.numeric(sub("\\((.+),.*", "\\1", labs)),as.numeric( sub("[^,]*,([^]]*)\\]", "\\1", labs)))
  bins<-unique(bins); bins[1]<-min(x)
  options(warn=0) # turning on warning messages
  return(bins)}
}

# show color panel
showpanel <- function(col)
{
  image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n" )
}

rename_Matrix = function(x) {cols <- colnames(x);
newnames <- c();
for (i in cols) {y<-unlist(strsplit(i,"\\."))[1];newnames<-c(newnames,unlist(strsplit(y,"\\_"))[1])};
colnames(x)<-newnames;
return(x)
};

showpanel <- function(col) {image(z=matrix(1:100, ncol=1), col=col, xaxt="n", yaxt="n" )}

# boxplot with jitters:
BoxJetter <- function(vec1,vec2,lable_vec1,label_vec2,my.bg) {
  y <- c(rep(lable_vec1,length(vec1)),rep(label_vec2,length(vec2)))
  y <- ordered(y,c(lable_vec1,label_vec2))
  x <- c(vec1,vec2)
  boxplot(x ~ y, border=4, col="light grey", boxwex=0.5)
  points(jitter(c(rep(1,length(vec1)),rep(2,length(vec2))), 0.7),
         c(vec1,vec2),col=my.bg,cex=2, pch=19)
  p.value <- wilcox.test(vec1,vec2)
  return(p.value)
} 