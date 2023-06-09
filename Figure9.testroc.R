
#install.packages("pROC")


library(pROC)

inputFile="test.normalize.txt" 
outFile="ROC.pdf"         
setwd("C:\\Users\\16622\\Desktop\\text\\GSE15573\\25.testROC")

data=read.table(inputFile, header=T, sep="\t", check.names=F, row.names=1)

data = t(data)

dat1 = data[,c("FAM96A","CGRRF1")]

write.csv(dat1,file = "abcd.csv")



inputTxt = "abcd1.txt"

rt=read.table(inputTxt,header=T,sep=",",check.names=F,row.names=1)  
y=colnames(rt)[1]

#定义颜色
bioCol=c("red","blue","green","yellow")
if(ncol(rt)>4){
  bioCol=rainbow(ncol(rt))}

#绘制
pdf(file=outFile,width=5,height=5)
roc1=roc(rt[,y], as.vector(rt[,2]))
aucText=c( paste0(colnames(rt)[2],", AUC=",sprintf("%0.3f",auc(roc1))) )
plot(roc1, col=bioCol[1])
for(i in 3:ncol(rt)){
  roc1=roc(rt[,y], as.vector(rt[,i]))
  lines(roc1, col=bioCol[i-1])
  aucText=c(aucText, paste0(colnames(rt)[i],", AUC=",sprintf("%0.3f",auc(roc1))) )
}
legend("bottomright", aucText,lwd=2,bty="n",col=bioCol[1:(ncol(rt)-1)])
dev.off()

