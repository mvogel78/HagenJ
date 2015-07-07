load("20140209druckschritt.rdata")
require(car)

x$auff.med <- recode(x$Auffussen_1)

for(i in 1:6){
    name <- paste0("auff.med_",i)
    x[,name] <- recode(x[,paste0("Auffussen_",i)],"c(1,4,7)=-1;c(2,5,8)=0;c(3,6,9)=1")
    name <- paste0("auff.sag_",i)
    x[,name] <- recode(x[,paste0("Auffussen_",i)],"1:3=1;4:6=0;7:9=-1")
    print(table(x[,paste0("Auffussen_",i)],x$Huf))
}

cor.test(x$Auffussen_1,x$DV.med_1,method = "spearman")


cor.test(x$Auffussen_1,x$DV.lat_1,method = "spearman")
cor.test(x$Auffussen_1,x$DV.lat_1,method = "spearman")

require(ggplot2)
ggplot(x,aes(x=factor(auff.sag_1),y=DV.lat_1)) + geom_boxplot()
ggplot(x,aes(x=factor(auff.med_1),y=DV.lat_1)) + geom_boxplot()

table(x$auff.med_1,x$GM.S)
table(x$auff.sag_1,x$GM.S)

require(vcd)
mosaic(table(factor(x$auff.sag_1),x$GM.S,dnn = list("auf.sag","gm.s")),shade=T,legend=T)
mosaic(table(factor(x$auff.sag_1),x$GM.S,dnn = list("auf.sag","gm.s")),shade=T,legend=T)

for(i in 1:6){
    bearb <- "nach"
    mosaic(table(factor(x[x$Bearbeitung==bearb,paste0("auff.sag_",i)]),factor(x[x$Bearbeitung==bearb,paste0("auff.med_",i)]),dnn = list("Auffussen sagittal","Auffussen medial")),shade=T,legend=T)
    savePlot(paste0("mosaicauffussen_",bearb,"_",i,".png"))
}

## nochmal mit Gruppe
for(i in 1:6){
    bearb <- "nach"
    mosaic(table(x$Gruppe[x$Bearbeitung==bearb],factor(x[x$Bearbeitung==bearb,paste0("auff.sag_",i)]),factor(x[x$Bearbeitung==bearb,paste0("auff.med_",i)]),dnn = list("Gruppe","Auffussen sagittal","Auffussen medial")),shade=T,legend=T)
    savePlot(paste0("mosaicauffussenmitgruppe_",bearb,"_",i,".png"))
}

