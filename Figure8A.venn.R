
#install.packages("VennDiagram")


library(VennDiagram)       #���ð�
diseaseFile="hubGenes_MMturquoise.txt"                 #��������������Ľ���ļ�
clusterFile="cluster.hubGenes_MMturquoise.txt"         #���͹���������Ľ���ļ�
setwd("C:\\biowolf\\geoCRG\\21.venn")     #���ù���Ŀ¼
geneList=list()

#��ȡ��������������Ľ���ļ�
rt=read.table(diseaseFile, header=F, sep="\t", check.names=F)
geneNames=as.vector(rt[,1])              #��ȡ��������
geneNames=gsub("^ | $","",geneNames)     #ȥ��������β�Ŀո�
uniqGene=unique(geneNames)               #����ȡunique
geneList[["Disease WGCNA"]]=uniqGene     #������WGCNA�ĺ��Ļ��򱣴浽geneList����

#��ȡ���͹���������Ľ���ļ�
rt=read.table(clusterFile, header=F, sep="\t", check.names=F)
geneNames=as.vector(rt[,1])              #��ȡ��������
geneNames=gsub("^ | $","",geneNames)     #ȥ��������β�Ŀո�
uniqGene=unique(geneNames)               #����ȡunique
geneList[["Cluster WGCNA"]]=uniqGene     #�ѷ���WGCNA�ĺ��Ļ��򱣴浽geneList����

#����vennͼ
venn.plot=venn.diagram(geneList,filename=NULL,fill=c("cornflowerblue", "darkorchid1"),scaled=FALSE,cat.pos=c(-1,1),cat.col = c("cornflowerblue", "darkorchid1"),cat.cex=1)
pdf(file="venn.pdf", width=5, height=5)
grid.draw(venn.plot)
dev.off()

#����������Ļ�����б�
interGenes=Reduce(intersect,geneList)
write.table(file="interGenes.txt", interGenes, sep="\t", quote=F, col.names=F, row.names=F)


