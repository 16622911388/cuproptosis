

#if (!requireNamespace("BiocManager", quietly = TRUE))
#    install.packages("BiocManager")
#BiocManager::install("ConsensusClusterPlus")


library(ConsensusClusterPlus)      #引用包
expFile="diffGeneExp.txt"          #表达数据文件
workDir="C:\\Users\\16622\\Desktop\\RA\\14.cluster"     #工作目录
setwd(workDir)      #设置工作目录

#读取输入文件
data=read.table(expFile, header=T, sep="\t", check.names=F, row.names=1)
data=as.matrix(data)

#去除对照组样品, 只保留实验组样品
group=sapply(strsplit(colnames(data),"\\_"), "[", 2)
data=data[,group=="Treat"]

#聚类
maxK=9     #设置最大的k值
results=ConsensusClusterPlus(data,
              maxK=maxK,
              reps=50,
              pItem=0.8,
              pFeature=1,
              title=workDir,
              clusterAlg="km",
              distance="euclidean",
              seed=123456,
              plot="png")

#一致性打分
calcICL(results, title="consensusScore", plot="png")

#输出分型结果
clusterNum=2        #分几类，根据前面的图形判断
cluster=results[[clusterNum]][["consensusClass"]]
cluster=as.data.frame(cluster)
colnames(cluster)=c("Cluster")
cluster$Cluster=paste0("C", cluster$Cluster)
outTab=cbind(t(data), cluster)
outTab=rbind(ID=colnames(outTab), outTab)
write.table(outTab, file="cluster.txt", sep="\t", quote=F, col.names=F)



