final<-function(dtpath,id1){
  empdata1 <- dtpath
  data1<-subset(empdata1,empdata1[,1]==id1)
  ans<-subset(data1,select=c(localband))
}

final_plot<-function(dtpath,id2){
  data11 <- dtpath
  data111<-subset(data11,data11[,1]==id2)
  yy<-subset(data111,select=c(a,b,c,d,e,localband))
  yy=unlist(yy)
  xx=c("year2011","year2012","year2013","year2014","year2015","Predicted band")
  p<-plot_ly(
    x=yy,
    y=xx,
    name="performance",
    type="bar")
  p
}