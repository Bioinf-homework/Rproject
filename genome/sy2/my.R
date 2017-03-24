# setwd("D://this term//大三下//基因组//实验课//experiment2")

CpG <- read.table("cpgIslandExt.txt")
refgene <- read.table("refGene.txt")

refgene <- refgene[order(refgene[,3]),]

CpG.distance <- array(1000000000, dim=nrow(refgene));

now=""
for (i in (1:nrow(refgene))) {
	if(refgene[i,3]!=now){
		chr.CpG <- CpG[CpG[,1]==as.character(refgene[i,3]), ];

		CpG.position <- (chr.CpG[,2]+chr.CpG[,3])/2
		now = refgene[i,3]
	}
	print (i)
	if(refgene[i,4]=="+")
	{
		CpG.min <- min(abs(refgene[i,5]-CpG.position));
		CpG.distance[i] <- CpG.min;
	}else{
		CpG.min <- min(abs(refgene[i,6]-CpG.position));
		CpG.distance[i] <- CpG.min;
	}

}

hist(CpG.distance[CpG.distance < 10000],nclass=300)