dta_memo <- read.table("C:/Users/USER/Documents/1�ҵ{/�M�D/��Ƥ��R_�O��/��ƾ�X_V2_06_representation_dele.txt", header = T)




#��ƾ�z
dta_memo$representation<-as.factor(dta_memo$representation)
dta_memo$Ans           <-as.numeric(dta_memo$Ans)
dta_memo$RT            <-as.numeric(dta_memo$RT)
dta_memo               <-dta_memo[, -1] # �G������pattern���
dta_memo               <-dta_memo[, -3] # �G������pattern���
#dta_memo$Pattern<-as.character(dta_memo$Pattern)
head(dta_memo)

##Find mean, sd for each group
dta_Ans_avg<- tapply(dta_memo$Ans,dta_memo$representation,mean)
dta_RT_avg<- tapply(dta_memo$RT,dta_memo$representation,mean)


#��C�ഫ(236*3���x�},pattern, ans, rt)
new_dta <- cbind(dta_Ans_avg,dta_RT_avg)
head(new_dta)

# ���s
kmeans.cluster <- kmeans(new_dta, centers=10) 
# �s�����ܲ���
kmeans.cluster$withinss

# ��ı�� k-means ���s���G(���ggplot2���y�k)
require(factoextra)
fviz_cluster(kmeans.cluster,           # ���s���G
             data  = new_dta,      # ���
             geom = c("point","text" ), # �I�M����(point & label)
             frame.type = "norm" )      # �ج[���A

#�̨Τ��s

# Elbow Method ���Φb K-Means
fviz_nbclust(new_dta, 
             FUNcluster = kmeans,# K-Means
             method = "wss",     # total within sum of square
             k.max = 12          # max number of clusters to consider
) +
  
  labs(title="Elbow Method for K-Means") +
  
  geom_vline(xintercept = 3,        # �b X=3���a�� 
             linetype = 2)          # �e�@��������u

kmeans.cluster$cluster
write.table(kmeans.cluster$cluster, file = "C:/Users/USER/Desktop/kmeans.cluster.txt",sep = " ", quote = FALSE, na = "NA")