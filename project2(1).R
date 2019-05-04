setwd("E:/伯克利春季学期/stat154/project2")
header = c("x", "y", "expertlabel", "NDAI", "SD", "CORR", "DF", "CF", "BF", "AF", "AN")
df1 <- read.table("./image_data/image1.txt", col.names = header)
df2 <- read.table("./image_data/image2.txt", col.names = header)
df3 <- read.table("./image_data/image3.txt", col.names = header)

#2a Data Split

### the first way
dfTest1 <- df3
dfValidation1 <- df2[df2$x>=median(df2$x)&df2$y>+median(df2$y),]
dfTraining1 <- rbind(df1,df2[!(df2$x<=median(df2$x)&df2$y<=median(df2$y)),])
write.csv(dfTest1,file="./dfTest1.csv",quote=F,row.names = F)
write.csv(dfValidation1,file="./dfValidation1.csv",quote=F,row.names = F)
write.csv(dfTraining1,file="./dfTraining1.csv",quote=F,row.names = F)

library(boot)
gene_cv <- function(Model,features,labels,k){
set.seed(17)
cv.error.10=rep(0,10)
for (i in 1:10){
  cv.error.10[i]=cv.glm(x,Model,k)$delra[i]
}
return(cv.error.10)
}

### the second way
dfTest2 <- df3
spec = c(train = .75,validate = .25)
g = sample(cut(
  seq(nrow(df2)), 
  nrow(df2)*cumsum(c(0,spec)),
  labels = names(spec)
))
res = split(df2, g)
dfValidation2 <- res$validate
dfTraining2 <- rbind(df1,res$train)

write.csv(dfTest2,file="./dfTest2.csv",quote=F,row.names = F)
write.csv(dfValidation2,file="./dfValidation2.csv",quote=F,row.names = F)
write.csv(dfTraining2,file="./dfTraining2.csv",quote=F,row.names = F)

#2b Baseline-trivial classifieaccuracy1 <- sum(dfValidation1$expertlabel==-1)/28650
accuracy1va <- sum(dfValidation1$expertlabel==-1)/28650
accuracy2va <- sum(dfValidation2$expertlabel==-1)/28778
accuracy1test <- sum(dfTest1$expertlabel==-1)/115217
accuracy2test <- sum(dfTest2$expertlabel==-1)/115217
accuracy1va
accuracy2va
accuracy1test
accuracy2test


#2c First Order Importance
library(ggplot2)
###1
hist_NDAI = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = NDAI,fill="Clouded"), bins = 70, color = "red", alpha = 0.2) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = NDAI,fill="Not Clouded"), bins = 70, color = "blue", alpha = 0.5)+ 
  labs(title = "NDAI in Clouded and Unclouded Pixels") +
  theme_minimal()  
hist_NDAI
hist_SD = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = SD,fill="Clouded"), bins = 70, color = "red", alpha = 0.2) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = SD,fill="Not Clouded"), bins = 70, color = "blue", alpha = 0.5)+ 
  labs(title = "SD in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_SD
hist_CORR = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = CORR, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = CORR, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "CORR in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_CORR
hist_DF = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = DF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = DF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "DF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_CF = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = CF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = CF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "CF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_CF
hist_BF = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = BF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = BF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "BF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_BF
hist_AF = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = AF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = AF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "AF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_AF
hist_AN = ggplot() + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == 1,], aes(x = AN, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining1[dfTraining1[,"expertlabel"] == -1,], aes(x = AN, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "AN in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_AN

library("plyr")
skewness <- function(x){
  return(3*(mean(x)-median(x))/sd(x))
}
range_1 <- function(x){
  return(mean(x)-1.96*sd(x))
}
range_2 <- function(x){
  return(mean(x)+1.96*sd(x))
}
normalize <- function(x){
  return(mean((x-mean(x)/sd(x))))
}

ddply(dfTraining1, .(expertlabel),summarize,mean_NDAI=mean(NDAI),mean_SD=mean(SD),mean_CORR=mean(CORR),mean_DF=mean(DF),mean_CF=mean(CF),mean_BF=mean(BF),mean_AF=mean(AF),mean_AN=mean(AN))
ddply(dfTraining1, .(expertlabel),summarize,skewness_NDAI=skewness(NDAI),skewness_SD=skewness(SD),skewness_CORR=skewness(CORR),skewness_DF=skewness(DF),skewness_CF=skewness(CF),skewness_BF=skewness(BF),skewness_AF=skewness(AF),skewness_AN=skewness(AN))
ddply(dfTraining1, .(expertlabel),summarize,range_1_NDAI=range_1(NDAI),range_1_SD=range_1(SD),range_1_CORR=range_1(CORR),range_1_DF=range_1(DF),range_1_CF=range_1(CF),range_1_BF=range_1(BF),range_1_AF=range_1(AF),range_1_AN=range_1(AN))
ddply(dfTraining1, .(expertlabel),summarize,range_2_NDAI=range_2(NDAI),range_2_SD=range_2(SD),range_2_CORR=range_2(CORR),range_2_DF=range_2(DF),range_2_CF=range_2(CF),range_2_BF=range_2(BF),range_2_AF=range_2(AF),range_2_AN=range_2(AN))

###2
hist_NDAI2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = NDAI, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = NDAI, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "NDAI in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_NDAI2
hist_SD2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = SD, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = SD, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "SD in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_SD2
hist_CORR2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = CORR, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = CORR, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "CORR in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_CORR2
hist_DF2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = DF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = DF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "DF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_CF2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = CF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = CF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "CF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_CF2
hist_BF2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = BF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = BF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "BF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_BF2
hist_AF2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = AF, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = AF, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "AF in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_AF2
hist_AN2 = ggplot() + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == 1,], aes(x = AN, fill = "Clouded"), bins = 70, color = "black", alpha = 0.7) + 
  geom_histogram(data = dfTraining2[dfTraining2[,"expertlabel"] == -1,], aes(x = AN, fill = "Not Clouded"), bins = 70, color = "black", alpha = 0.5) + 
  labs(title = "AN in Clouded and Unclouded Pixels") +
  theme_minimal()
hist_AN2 

ddply(dfTraining2, .(expertlabel),summarize,mean(NDAI),mean(SD),mean(CORR),mean(DF),mean(CF),mean(BF),mean(AF),mean(AN))
ddply(dfTraining2, .(expertlabel),summarize,skewness(NDAI),skewness(SD),skewness(CORR),skewness(DF),skewness(CF),skewness(BF),skewness(AF),skewness(AN))
ddply(dfTraining2, .(expertlabel),summarize,range_1(NDAI),range_1(SD),range_1(CORR),range_1(DF),range_1(CF),range_1(BF),range_1(AF),range_1(AN))
ddply(dfTraining2, .(expertlabel),summarize,range_2(NDAI),range_2(SD),range_2(CORR),range_2(DF),range_2(CF),range_2(BF),range_2(AF),range_2(AN))

qqnorm(dfTraining1$NDAI, pch = 1, frame = FALSE)
qqnorm(dfTraining1$SD, pch = 1, frame = FALSE)
qqnorm(dfTraining1$CORR, pch = 1, frame = FALSE)



