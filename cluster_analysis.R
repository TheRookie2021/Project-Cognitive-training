

dta_memo <- read.table("C:/Users/USER/Documents/1課程/專題/資料分析_記憶/資料整合_V2_03.txt", header = T)

#資料整理
dta_memo$Ans<-as.numeric(dta_memo$Ans)
dta_memo$RT<-as.numeric(dta_memo$RT)

#dta_memo$Pattern<-as.character(dta_memo$Pattern)
head(dta_memo)
#由於分群屬於「非監督式學習」的演算法，
#因此我們先把iris內的品種(Species)欄位拿掉，以剩下的資料進行分群：
dta_memo <- dta_memo[, -1] # 故移除掉pattern欄位
dta_memo <- dta_memo[, -3] # 故移除掉pattern欄位
head(dta_memo)        

#1. 階層式分群(Hierarchical Clustering)
E.dist <- dist(dta_memo, method="euclidean") # 歐式距離
M.dist <- dist(dta_memo, method="manhattan") # 曼哈頓距離

# 讓圖片以1x2的方式呈現，詳情請見(4)繪圖-資料視覺化
par(mfrow=c(1,2)) 

# 使用歐式距離進行分群
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="歐式距離")
# 使用曼哈頓距離進行分群
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="曼哈頓距離")

#有了「距離矩陣」後，要如何把資料結合起來
hclust(E.dist, method="single")   # 最近法
hclust(E.dist, method="complete") # 最遠法
hclust(E.dist, method="average")  # 平均法
hclust(E.dist, method="centroid") # 中心法
hclust(E.dist, method="ward.D2")  # 華德法

#歐式距離搭配華德法
E.dist <- dist(data, method="euclidean")      # 歐式距離
h.cluster <- hclust(E.dist, method="ward.D2") # 華德法

# 視覺化
plot(h.cluster)
abline(h=9, col="red")

