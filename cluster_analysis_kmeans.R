dta_memo <- read.table("C:/Users/USER/Documents/1課程/專題/資料分析_記憶/資料整合_V2_06_representation_dele.txt", header = T)




#資料整理
dta_memo$representation<-as.factor(dta_memo$representation)
dta_memo$Ans           <-as.numeric(dta_memo$Ans)
dta_memo$RT            <-as.numeric(dta_memo$RT)
dta_memo               <-dta_memo[, -1] # 故移除掉pattern欄位
dta_memo               <-dta_memo[, -3] # 故移除掉pattern欄位
#dta_memo$Pattern<-as.character(dta_memo$Pattern)
head(dta_memo)

##Find mean, sd for each group
dta_Ans_avg<- tapply(dta_memo$Ans,dta_memo$representation,mean)
dta_RT_avg<- tapply(dta_memo$RT,dta_memo$representation,mean)


#行列轉換(236*3的矩陣,pattern, ans, rt)
new_dta <- cbind(dta_Ans_avg,dta_RT_avg)
head(new_dta)

# 分群
kmeans.cluster <- kmeans(new_dta, centers=10) 
# 群內的變異數
kmeans.cluster$withinss

# 視覺化 k-means 分群結果(基於ggplot2的語法)
require(factoextra)
fviz_cluster(kmeans.cluster,           # 分群結果
             data  = new_dta,      # 資料
             geom = c("point","text" ), # 點和標籤(point & label)
             frame.type = "norm" )      # 框架型態

#最佳分群

# Elbow Method 應用在 K-Means
fviz_nbclust(new_dta, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,        # 在 X=3的地方 
             linetype = 2)          # 畫一條垂直虛線

kmeans.cluster$cluster
write.table(kmeans.cluster$cluster, file = "C:/Users/USER/Desktop/kmeans.cluster.txt",sep = " ", quote = FALSE, na = "NA")