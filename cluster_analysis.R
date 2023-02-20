

dta_memo <- read.table("C:/Users/USER/Documents/1�ҵ{/�M�D/��Ƥ��R_�O��/��ƾ�X_V2_03.txt", header = T)

#��ƾ�z
dta_memo$Ans<-as.numeric(dta_memo$Ans)
dta_memo$RT<-as.numeric(dta_memo$RT)

#dta_memo$Pattern<-as.character(dta_memo$Pattern)
head(dta_memo)
#�ѩ���s�ݩ�u�D�ʷ����ǲߡv���t��k�A
#�]���ڭ̥���iris�����~��(Species)��쮳���A�H�ѤU����ƶi����s�G
dta_memo <- dta_memo[, -1] # �G������pattern���
dta_memo <- dta_memo[, -3] # �G������pattern���
head(dta_memo)        

#1. ���h�����s(Hierarchical Clustering)
E.dist <- dist(dta_memo, method="euclidean") # �ڦ��Z��
M.dist <- dist(dta_memo, method="manhattan") # �ҫ��y�Z��

# ���Ϥ��H1x2���覡�e�{�A�Ա��Ш�(4)ø��-��Ƶ�ı��
par(mfrow=c(1,2)) 

# �ϥμڦ��Z���i����s
h.E.cluster <- hclust(E.dist)
plot(h.E.cluster, xlab="�ڦ��Z��")
# �ϥΰҫ��y�Z���i����s
h.M.cluster <- hclust(M.dist) 
plot(h.M.cluster, xlab="�ҫ��y�Z��")

#���F�u�Z���x�}�v��A�n�p����Ƶ��X�_��
hclust(E.dist, method="single")   # �̪�k
hclust(E.dist, method="complete") # �̻��k
hclust(E.dist, method="average")  # �����k
hclust(E.dist, method="centroid") # ���ߪk
hclust(E.dist, method="ward.D2")  # �ؼw�k

#�ڦ��Z���f�t�ؼw�k
E.dist <- dist(data, method="euclidean")      # �ڦ��Z��
h.cluster <- hclust(E.dist, method="ward.D2") # �ؼw�k

# ��ı��
plot(h.cluster)
abline(h=9, col="red")
