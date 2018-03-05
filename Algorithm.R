###Read rds
setwd("C:/Users/user/Desktop")
df <- readRDS("finaldata.Rds")


###Choose 7 principle components, which explans 86.64% of the variance
pca <- prcomp(df,center = TRUE,scale. = TRUE) 

plot(pca,type = "l")
summary(pca)

####unscaled pca data
pcadf=data.frame(pca$x)[1:7]

##scaled pca data
finalpcadf=data.frame(scale(pcadf,center=TRUE,scale=TRUE))
save(finalpcadf,file="finalpcadf.Rda")

##1 distance of z-scores, n=2
distance=data.frame(finalpcadf^2)
score1=data.frame(sqrt(rowSums(distance)))
score1final = data.frame(cbind(1:nrow(score1), score1))
colnames(score1final) = c("Record", "Zscore")
summary(score1final)

save(score1final,file="score1final.Rda")

##score1 histogram
library(ggplot2)
ggplot(score1final,aes(x=Zscore))+
  geom_histogram()+
  scale_y_log10()

####2 autoencoder single layer, hidder neurons is 2
library(h2o)
localH2O = h2o.init()
finalpcadf.hex = as.h2o(finalpcadf)
autoencoder = h2o.deeplearning(x = names(finalpcadf.hex), training_frame = finalpcadf.hex,
                               autoencoder = TRUE,
                               hidden=2,
                               reproducible = F)

##MSE of input and output for each variable

finaldata.anon = h2o.anomaly(autoencoder, finalpcadf.hex, per_feature=TRUE)
err = as.data.frame(finaldata.anon)

##autoencoder score
score2=data.frame(sqrt(rowSums(err)))
score2final = data.frame(cbind(1:nrow(score2), score2))
colnames(score2final) = c("Record", "autoencoder")
summary(score2final)

save(score2final,file="score2final.Rda")


###score2 histogram
ggplot(score2final,aes(x=autoencoder))+
  geom_histogram()+
  scale_y_log10()

load("score1final.rda")
load("score2final.rda")
load("finalpcadf.rda")

####one record one quantile, get top 10 fraud
score10=merge(score1final,score2final,by="Record")
score10$rank1=ceiling(rank(score10$Zscore))
score10$rank2=ceiling(rank(score10$autoencoder))
score10$finalscore=(score10$rank1+score10$rank2)/2

top10=score10[order(score10$finalscore,decreasing=TRUE)[1:10],]

####1000 bins, calculate socres
score=merge(score1final,score2final,by="Record")
score$rank1=ceiling(rank(score$Zscore)/1049)
score$rank2=ceiling(rank(score$autoencoder)/1049)
score$finalscore=(score$rank1+score$rank2)/2


##348 recores have finalscore equals 1000
x=score[score$finalscore==1000,]
nrow(x)

## final score histogram
ggplot(score,aes(x=finalscore))+
  geom_histogram()


totalrank=score10[order(score10$finalscore,decreasing=TRUE),]


##combine scores with original data, which has 30 original variables
library(data.table)
data=fread("ny.csv",data.table = FALSE,integer64 = "double")

rankany = merge(totalrank,data,by.x="Record",by.y="RECORD")
rank=rankany[order(rankany$finalscore,decreasing=TRUE),]

write.csv(rank,"rank.csv")


