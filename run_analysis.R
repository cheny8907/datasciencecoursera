#get current working directory and set work directory to datasets
f=getwd()
setwd("UCI HAR Dataset")

#read data ino R
xtsr<-read.table("test/X_test.txt")
xtar<-read.table("train/X_train.txt")
ytsr<-read.table("test/Y_test.txt")
ytar<-read.table("train/Y_train.txt")
subta<-read.table("train/subject_train.txt")
subts<-read.table("test/subject_test.txt")
al<-read.table("activity_labels.txt",stringsAsFactors = FALSE)
al<-al[,2]

#create a vector "exm" to indicate which columns to extract
exr<-read.table("features.txt",stringsAsFactors = FALSE)
exr<-exr[,2]
ex<-c()
exm<-c()
for (i in 1:length(exr)){
  x<-unlist(strsplit(exr[i],"-"))
  x<-x[2]
  if (substr(x,1,nchar(x)-2) %in% c("mean","std")){
      ex<-c(ex,exr[i])
      exm<-c(exm,i)
  }  
}

#extract the columns from X_test(train),and form a new dataset
xta<-xtar[,exm]
xts<-xtsr[,exm]
x<-rbind(xta,xts)

#convert Y to descriptive name yn,and add yn and subject to x.Give column names to the dataset
y<-rbind(ytar,ytsr)
yn<-c()
for (i in 1:nrow(y)){
  xx<-y[i,1]
  z<-al[xx]
  yn<-c(yn,z)
}
sub<-rbind(subta,subts)
data<-cbind(sub,yn,x)
names(data)<-c("subject","activity",ex)

# creates a tidy data set with the average of each variable for each activity and each subject.
l<-list()
s<-split(data,data$activity)
for (i in 1:length(s)){
  d<-split(s[[i]],s[[i]]$subject)
  for (j in 1:length(d)){
    q<-d[[j]]
    a<-sapply(q[,ex],mean)
    b<-data.frame(subject=names(d)[j],activity=names(s)[i],t(a))
    l[[length(l)+1]]<-b
  }  
}

c<-l[[1]]
for (i in 2:length(l)){
  c<-rbind(c,l[[i]])
}
names(c)<-c("subject","activity",ex)
setwd(f)
write.table(c,"tidy.txt",row.name=FALSE)


