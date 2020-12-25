input<-readline(prompt = "Enter the sport : ")

data_set<-read.csv("D:/Studies/Paper Vizag/athlete_events.csv")
sports=data_set$Sport
#data_set<-data.frame(data_set$Age,data_set$Height,data_set$Weight,data_set$Points)
data_set<-data.frame(Age=data_set$Age,Height=data_set$Height,Weight=data_set$Weight,Sport=data_set$Sport,Points=data_set$Points)
#head(data_set, 6)
#ggscatter(data_set, x = "Age", y = "Points", 

#          add = "reg.line", conf.int = TRUE, 
#          cor.coef = TRUE, cor.method = "pearson",
#          xlab = "Age", ylab = "Medal Points")
#res <- cor.test(data_set$Age, data_set$Points, method = "pearson")
sports<-unique(sports)
new_data<-list()
for (i in seq(1,length(sports) , by=1)){
  new_data[[i]] <- subset(data_set,Sport==sports[i])
  new_data[[i]] <- data.frame(Age=new_data[[i]]$Age,Height=new_data[[i]]$Height,Weight=new_data[[i]]$Weight,Points=new_data[[i]]$Points)
  }
result=list()
for (i in seq(1, length(sports) , by=1)){
  result[[i]]= lm(Points~Age+Height+Weight,new_data[[i]])
}

for(i in seq(1,length(sports),by=1)){
  if(input==sports[[i]])
  {
    break
  }
}

k=i
correl_val<-matrix(nrow=length(sports),ncol=3)
for (i in seq(1,length(sports) , by=1)){
  correl_val[i,1] =  cor(new_data[[i]]$Age, new_data[[i]]$Points, method = "pearson",use = "complete.obs")
  correl_val[i,2] =  cor(new_data[[i]]$Height, new_data[[i]]$Points, method = "pearson",use = "complete.obs")
  correl_val[i,3] =  cor(new_data[[i]]$Weight, new_data[[i]]$Points, method = "pearson",use = "complete.obs")
}
sports[[k]]
print(summary(result[[k]]))
print(result[[k]])
library(psych)
pairs.panels(new_data[[k]])
