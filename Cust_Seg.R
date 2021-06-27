#packages
library(tidyverse)
library(ggvis)
library(GGally)
book_data<-data.frame(CharlesBookClub_2_)
###############Select only relevant variables
book_data<-book_data%>%select(Gender, M, R, "F", FirstPurch, ChildBks, YouthBks, CookBks,
                              DoItYBks, RefBks, ArtBks,GeogBks,ItalCook,ItalAtlas, ItalArt,
                              Florence)
glimpse(book_data)
###########Exploratory Data analysis and Figures
###########
###############
book_data%>%ggpairs()
#########Florence sold yes no
florence<- ggplot(book_data,aes(x=Florence))+
  theme_bw()+
  geom_bar()+
  labs(y="Number of Purchases Art History of Florence")+
  scale_x_continuous(name="0=No and 1=Yes",breaks =seq(0,1,1))+
  scale_y_continuous(name="Number Art History of Florence Sold",
                     breaks=seq(0,4000,250))+
  ggtitle("Counts of Art History of Florence Sold")

florence
#######################histograms by gender
gender<-ggplot(book_data, aes(x=Florence))+
  geom_bar()+
  labs(title="Counts of Art History of FLorence Sold By Gender","Number Sold",subtitle="0=Male and 1=Female")+
  theme_bw()+
  facet_wrap(~Gender)
gender
################RFM frequency and histogram plots
recency<-ggplot(book_data, aes(R))+
  geom_histogram(aes(y=..density..),
                 binwidth=2,
                 colour="blue",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")+
  theme_bw()+
  scale_x_continuous(breaks=seq(2,36,2))+
  labs(title="Histogram and PDF of Recency",x="Recency",subtitle="Mean represented as Green Line" )+
geom_vline(aes(xintercept=mean(R)),
           color="green",linetype="dashed",size=1)
recency
#########Frequency
frequency<-ggplot(book_data, aes(F))+
  geom_histogram(aes(y=..density..),
                 binwidth=1,
                 colour="red",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")+
  theme_bw()+
  scale_x_continuous(breaks=seq(1,13,1))+
  labs(title="Histogram and PDF of Frequency",x="Frequency",subtitle="Mean represented as Green Line" )+
  geom_vline(aes(xintercept=mean(F)),
             color="green",linetype="dashed",size=1)
frequency
################Monetary
monetary<-ggplot(book_data, aes(M))+
  geom_histogram(aes(y=..density..),
                 binwidth=30,
                 colour="purple",fill="white")+
  geom_density(alpha=.2,fill="#FF6666")+
  theme_bw()+
  scale_x_continuous(breaks=seq(1,500,30))+
  labs(title="Histogram and PDF of Monetary Value",x="Frequency",subtitle="Mean represented as Green Line" )+
  geom_vline(aes(xintercept=mean(M)),
             color="green",linetype="dashed",size=1)
monetary
###################Art book and italian art books
book_data$Gender=as.factor(book_data$Gender)
art<-ggplot(book_data,aes(Florence,fill=Gender))+
  geom_histogram(binwidth=1,color="#1380A1")+
  theme_bw()+
  facet_wrap(~ArtBks)+  
  labs(title="Histogram of Art of Florence Sold by Gender and number of Genaeral Art Books sold",x="Frequency",subtitle="0=Male and 1=Female" )
art
#######################Italian Art books sold
italianart<-ggplot(book_data,aes(Florence,fill=Gender))+
  geom_histogram(binwidth=1,color="#1380A1")+
  theme_bw()+
  facet_wrap(~ItalArt)+  
  labs(title="Histogram of Art of Florence Sold by Gender and number of Genaeral Art Books sold",x="Frequency",subtitle="0=Male and 1=Female" )
italianart
######################
##################
###################First reduce dimenstionality of the data with pca
book_data$Gender=as.numeric(book_data$Gender)
book_pca<-prcomp(book_data,scale=T)
###########examine the output
summary(book_pca)
#######################visualize eigenvalues
fviz_eig(book_pca)
############looks like pca 1 through pca 9 accounts for approx 80% of the variability in the data
book_pca$rotation[,1:9]
##################graph the principal components ( some eda on pca!)
comps<-data.frame(book_pca$x[,1:9])
plot(comps,pch=16,col=rgb(0,0,0,.5))
##########
fviz_pca_var(book_pca,
             col.var="contrib",
             gradient.cols=c("#00AFBB", "#E7B800", "#FC4E07"),
             repel=T)
############Determine number of clusters
# Determine number of clusters elbow method
wss <- (nrow(comps)-1)*sum(apply(comps,2,var))
for (i in 2:15) wss[i] <- sum(kmeans(comps,
                                     nstart=25,
                                     iter.max=100,
                                     centers=i)$withinss)
plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")
#######silhouette method
fviz_nbclust(book_data,kmeans,method="silhouette")
#####################Apply kmeans
k<-kmeans(comps,2,nstart=25,iter.max=1000)
###########pull out cluster values and attach to original dataset
cluster<-k$cluster
cluster<-data.frame(cluster)
book_data_clusters<-bind_cols(book_data,cluster)
glimpse(book_data_clusters)
###########################
cluster<-ggplot(book_data_clusters, aes(x=cluster))+
  geom_bar()+
  labs(title="Counts of Art History of FLorence Sold By Gender","Number Sold",subtitle="1=Cluster1 and 2=Cluster2")+
  theme_bw()+
  facet_wrap(~Florence)
cluster
#####################
boxplot(book_data$Gender ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Gender by Cluster')

boxplot(book_data$M ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Monetary Value by Cluster')

boxplot(book_data$R ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Recency by Cluster')

boxplot(book_data$F ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Frequency by Cluster')

boxplot(book_data$FirstPurch ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Firstpurch by Cluster')

boxplot(book_data$ChildBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Childbooks by Cluster')
boxplot(book_data$YouthBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Youthbooks by Cluster')
boxplot(book_data$CookBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Cookbooks by Cluster')
boxplot(book_data$DoItYBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='DoItYourselfBooks by Cluster')
boxplot(book_data$RefBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Referencebooks by Cluster')
boxplot(book_data$ArtBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Art Books by Cluster')
boxplot(book_data$GeogBks ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Geography books by Cluster')
boxplot(book_data$ItalCook ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='ItalianCookingBooks by Cluster')
boxplot(book_data$ItalAtlas ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='ItalianAtlasBooks by Cluster')
boxplot(book_data$ItalArt ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='ItalianArtBooks by Cluster')
boxplot(book_data$Florence ~ k$cluster,
        xlab='Cluster', ylab='Gender',
        main='Florence Purchased by Cluster')
