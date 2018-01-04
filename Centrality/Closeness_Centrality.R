library("qgraph")

date = read.table("date_3RUN.txt")
date <-t(date[1])
xdate <- as.Date(date ,format = "%d.%m.%Y")
delta_covar02 <- read.csv("02_Citi_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar03 <- read.csv("03_BoA_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar04 <- read.csv("04_Barclays_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar05 <- read.csv("05_BNP_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar06 <- read.csv("06_CreditSuisse_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar07 <- read.csv("07_DB_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar08 <- read.csv("08_GS_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar09 <- read.csv("09_HSBC_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar10 <- read.csv("10_JPM_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar11 <- read.csv("11_MS_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar12 <- read.csv("12_RBS_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar13 <- read.csv("13_SocGen_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar14 <- read.csv("14_UBS_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")
delta_covar15 <- read.csv("16_AIG_dcovar_adjacent_3RUN_DEC2013.csv",sep=";",dec=",")

col <- array(0,196)
name_vector = c("CITI","BOA","BARC","BNP","CS","DB","GS","HSBC","JPM","MS","RBS","SG","UBS","AIG")

#Calculate eigenvector centrality value for each vertex
evecc <- function(Y)
{
  diag(Y) <- 0
  tmp <- eigen(Y)$vec[,1]
  tmp <- abs(tmp)
  tmp
}

eigen_ce <- function(Y)
{
  n <- nrow(Y) #number of row
  e <- evecc(Y)
  Y.sgn <- matrix(0,n,n)
  Y.sgn[1,] <- 1
  #Y.sgn <- Y.sgn+t(Y.sgn)
  e.sgn <- evecc(Y.sgn)
  sum(max(e)-e)/sum(max(e.sgn)-e.sgn)
}

degree_centrality <- matrix(0,561,14)
closeness_centrality <- matrix(0,561,14)
eigenvec_centrality <- matrix(0,561,14)
#eigenvec_cenvalue <- array(0,561)

#Calculcate eigenvector centrality for all days
for ( i in 1:561){
  col = c(t(delta_covar02[i,2:15]),t(delta_covar03[i,2:15]),t(delta_covar04[i,2:15]),t(delta_covar05[i,2:15]),t(delta_covar06[i,2:15]),t(delta_covar07[i,2:15]),t(delta_covar08[i,2:15]),t(delta_covar09[i,2:15]),t(delta_covar10[i,2:15]),
          t(delta_covar11[i,2:15]),t(delta_covar12[i,2:15]),t(delta_covar13[i,2:15]),t(delta_covar14[i,2:15]),t(delta_covar15[i,2:15]))
  
  matrix.col <- t(matrix(col, nrow =14, ncol = 14))
  diag(matrix.col)<-0
  
  colnames(matrix.col) =  c("CITI","BOA","BARC","BNP","CS","DB","GS","HSBC","JPM","MS","RBS","SG","UBS","AIG")
  
  G <- as.directed(graph.adjacency(matrix.col, weighted = T,add.colnames='label'))
  degree_centrality[i,] <- degree(G, loops = FALSE)/(vcount(G) - 1)
  #sort(deg_B_S, decreasing = TRUE,label=T)
  
  #closeness centrality
  closeness_centrality[i,] <- closeness(G)*(vcount(G) - 1)
  eigenvec_centrality[i,] <- evcent(G)[[1]]
}

xdate <- as.Date(date,format = "%d.%m.%Y")
xdate_format <- format(xdate, "%d.%m.%Y")
starttime <- xdate[1]
endtime <-  xdate[length(date)]          #strptime("20111230", "%Y%m%d")
xrange <- c(starttime,endtime )
atx_sub <- seq( starttime,endtime, by=32)

rainbowcols <-rainbow(length(closeness_centrality_us[1,])+3)  
topocols <-topo.colors(length(closeness_centrality_nonus[1,])+3)  

arraycols <- c(rainbowcols[1],rainbowcols[2],topocols[1],topocols[3],topocols[5],
               topocols[6],rainbowcols[3],topocols[8],rainbowcols[9],rainbowcols[5],
               topocols[9],topocols[10],topocols[11],rainbowcols[10])

pdf("/Users/hienphamthu/Documents/PhD DISSERTATION/COVAR CDS/Eigenvector_Centrality/Closeness_3run.pdf",width=7,height=5)
plot(xdate,closeness_centrality[,1],xaxt="n",type="l", lwd = 2, col= arraycols[1], ylim = c(0,1.1),xlab="Date", ylab="Closeness Centrality for all FIs",cex.lab=1,cex.axis=1,cex.main=1,cex.sub=1)
lines(xdate,closeness_centrality[,2],lwd = 2, col= arraycols[2])
lines(xdate,closeness_centrality[,3],lwd = 2, col= arraycols[3])
lines(xdate,closeness_centrality[,4],lwd = 2, col= arraycols[4])
lines(xdate,closeness_centrality[,5],lwd = 2, col= arraycols[5])
lines(xdate,closeness_centrality[,6],lwd = 2, col= arraycols[6])
lines(xdate,closeness_centrality[,7],lwd = 2, col= arraycols[7])
lines(xdate,closeness_centrality[,8],lwd = 2, col= arraycols[8])
lines(xdate,closeness_centrality[,9],lwd = 2, col= arraycols[9])
lines(xdate,closeness_centrality[,10],lwd = 2, col= arraycols[10])
lines(xdate,closeness_centrality[,11],lwd = 2, col= arraycols[11])
lines(xdate,closeness_centrality[,12],lwd = 2, col= arraycols[12])
lines(xdate,closeness_centrality[,13],lwd = 2, col= arraycols[13])
lines(xdate,closeness_centrality[,14],lwd = 2, col= arraycols[14])
#lines(xdate,closeness_centrality[,15],lwd = 2, col= arraycols[15])
legend("topleft",x.intersp=0.25,xjust=-0.5,yjust=0, name_vector,lty=c(1,1,1,1,1,1,1,1,1,1,1,1,1,1),
       lwd=(4),col=arraycols,ncol=8,bty='n',cex=0.7,seg.len=0.5)
axis(1, at=atx_sub, labels=format(atx_sub,  "%d.%m.%Y"), padj=0.5,cex.axis = 1)
dev.off()



##################### NON-US ##########################
#======================================================
degree_centrality_nonus3run <- matrix(0,561,8)
closeness_centrality_nonus3run <- matrix(0,561,8)
eigenvec_centrality_nonus3run <- matrix(0,561,8)
#eigenvec_cenvalue_nonus3run <- array(0,561)

nonusdelta_covar04 <- matrix(0,561,8)
nonusdelta_covar05 <- matrix(0,561,8)
nonusdelta_covar06 <- matrix(0,561,8)
nonusdelta_covar07 <- matrix(0,561,8)
nonusdelta_covar09 <- matrix(0,561,8)
nonusdelta_covar12 <- matrix(0,561,8)
nonusdelta_covar13 <- matrix(0,561,8)
nonusdelta_covar14 <- matrix(0,561,8)

#adjecent matrix for US banks
nonusdelta_covar04 <- cbind(delta_covar04[,4],delta_covar04[,5],delta_covar04[,6],delta_covar04[,7],delta_covar04[,9],delta_covar04[,12],delta_covar04[,13],delta_covar04[,14])
nonusdelta_covar05 <- cbind(delta_covar05[,4],delta_covar05[,5],delta_covar05[,6],delta_covar05[,7],delta_covar05[,9],delta_covar05[,12],delta_covar05[,13],delta_covar05[,14])
nonusdelta_covar06 <- cbind(delta_covar06[,4],delta_covar06[,5],delta_covar06[,6],delta_covar06[,7],delta_covar06[,9],delta_covar06[,12],delta_covar06[,13],delta_covar06[,14])
nonusdelta_covar07 <- cbind(delta_covar07[,4],delta_covar07[,5],delta_covar07[,6],delta_covar07[,7],delta_covar07[,9],delta_covar07[,12],delta_covar07[,13],delta_covar07[,14])
nonusdelta_covar09 <- cbind(delta_covar09[,4],delta_covar09[,5],delta_covar09[,6],delta_covar09[,7],delta_covar09[,9],delta_covar09[,12],delta_covar09[,13],delta_covar09[,14])
nonusdelta_covar12 <- cbind(delta_covar12[,4],delta_covar12[,5],delta_covar12[,6],delta_covar12[,7],delta_covar12[,9],delta_covar12[,12],delta_covar12[,13],delta_covar12[,14])
nonusdelta_covar13 <- cbind(delta_covar13[,4],delta_covar13[,5],delta_covar13[,6],delta_covar13[,7],delta_covar13[,9],delta_covar13[,12],delta_covar13[,13],delta_covar13[,14])
nonusdelta_covar14 <- cbind(delta_covar14[,4],delta_covar14[,5],delta_covar14[,6],delta_covar14[,7],delta_covar14[,9],delta_covar14[,12],delta_covar14[,13],delta_covar14[,14])

#Calculcate eigenvector centrality for all days
for ( i in 1:561){  
  col = c(t(nonusdelta_covar04[i,]),t(nonusdelta_covar05[i,]),t(nonusdelta_covar06[i,]),t(nonusdelta_covar07[i,]),
          t(nonusdelta_covar09[i,]),t(nonusdelta_covar12[i,]),t(nonusdelta_covar13[i,]),t(nonusdelta_covar14[i,]))
  
  #matrix.col <- t(matrix(col, nrow =8, ncol = 8))
  matrix.col <- if(i==101) matrix(1,8,8) else t(matrix(col, nrow =8, ncol = 8))
  diag(matrix.col)<-0
  colnames(matrix.col) = c("BARC","BNP","CS","DB","HSBC","RBS","SG","UBS")
  
  #Convert adjacent matrix into a graph object
  G <- as.directed(graph.adjacency(matrix.col, weighted = T,add.colnames='label'))
  
  #degree centrality
  degree_centrality_nonus3run[i,] <- degree(G, loops = FALSE)/(vcount(G) - 1)
  #sort(deg_B_S, decreasing = TRUE,label=T)
  
  #closeness centrality
  closeness_centrality_nonus3run[i,] <- closeness(G)*(vcount(G) - 1)
  
  #eigenvector centrality
  eigenvec_centrality_nonus3run[i,] <- evcent(G)[[1]]
  
}

pdf("/Users/hienphamthu/Documents/PhD DISSERTATION/COVAR CDS/Eigenvector_Centrality/Closeness_nonus_3run.pdf",width=7,height=5)
plot(xdate,closeness_centrality_nonus3run[,1],xaxt="n",type="l", lwd = 2, col= arraycols[3], ylim = c(0,1.1),xlab="Date", ylab="Closeness Centrality for Non-US FIs",cex.lab=1,cex.axis=1,cex.main=1,cex.sub=1)
lines(xdate,closeness_centrality_nonus3run[,2],lwd = 2, col= arraycols[4])
lines(xdate,closeness_centrality_nonus3run[,3],lwd = 2, col= arraycols[5])
lines(xdate,closeness_centrality_nonus3run[,4],lwd = 2, col= arraycols[6])
lines(xdate,closeness_centrality_nonus3run[,5],lwd = 2, col= arraycols[8])
lines(xdate,closeness_centrality_nonus3run[,6],lwd = 2, col= arraycols[11])
lines(xdate,closeness_centrality_nonus3run[,7],lwd = 2, col= arraycols[12])
lines(xdate,closeness_centrality_nonus3run[,8],lwd = 2, col= arraycols[13])
legend("topleft",x.intersp=0.25,xjust=-0.5,yjust=0, c("BARC","BNP","CS","DB","HSBC","RBS","SG","UBS"),lty=c(1,1,1,1,1,1,1,1,1),
       lwd=(4),col=c(arraycols[3],arraycols[4],arraycols[5],arraycols[6],arraycols[8],arraycols[11],arraycols[12],arraycols[13]),ncol=8,bty='n',cex=0.7,seg.len=0.5)
axis(1, at=atx_sub, labels=format(atx_sub,  "%d.%m.%Y"), padj=0.5,cex.axis = 1)
dev.off()







##################### US ##########################
#======================================================

degree_centrality_us3run <- matrix(0,561,6)
closeness_centrality_us3run <- matrix(0,561,6)
eigenvec_centrality_us3run <- matrix(0,561,6)


usdelta_covar02 <- matrix(0,561,6)
usdelta_covar03 <- matrix(0,561,6)
usdelta_covar08 <- matrix(0,561,6)
usdelta_covar10 <- matrix(0,561,6)
usdelta_covar11 <- matrix(0,561,6)
usdelta_covar15 <- matrix(0,561,6)
#usdelta_covar16 <- matrix(0,561,6)

#adjecent matrix for US banks
usdelta_covar02 <- as.matrix(cbind(delta_covar02[,2],delta_covar02[,3],delta_covar02[,8],delta_covar02[,10],delta_covar02[,11],delta_covar02[,15]))
usdelta_covar03 <- cbind(delta_covar03[,2],delta_covar03[,3],delta_covar03[,8],delta_covar03[,10],delta_covar03[,11],delta_covar03[,15])
usdelta_covar08 <- cbind(delta_covar08[,2],delta_covar08[,3],delta_covar08[,8],delta_covar08[,10],delta_covar08[,11],delta_covar08[,15])
usdelta_covar10 <- cbind(delta_covar10[,2],delta_covar10[,3],delta_covar10[,8],delta_covar10[,10],delta_covar10[,11],delta_covar10[,15])
usdelta_covar11 <- cbind(delta_covar11[,2],delta_covar11[,3],delta_covar11[,8],delta_covar11[,10],delta_covar11[,11],delta_covar11[,15])
usdelta_covar15 <- cbind(delta_covar15[,2],delta_covar15[,3],delta_covar15[,8],delta_covar15[,10],delta_covar15[,11],delta_covar15[,15])

#Calculcate eigenvector centrality for all days for US institution
for (i in 1:561){ 
  col = c(t(usdelta_covar02[i,]),t(usdelta_covar03[i,]),t(usdelta_covar08[i,]),t(usdelta_covar10[i,]),
          t(usdelta_covar11[i,]),t(usdelta_covar15[i,]))
  
  #matrix.col <- t(matrix(col, nrow =6, ncol = 6))
  matrix.col <- if(i==278) matrix(1,6,6) else if(i==331) matrix(1,6,6) else t(matrix(col, nrow =6, ncol = 6))
  diag(matrix.col)<-0
  colnames(matrix.col) = c("CITI","BOA","GS","JPM","MS","AIG")
  
  #Convert adjacent matrix into a graph object
  G <- as.directed(graph.adjacency(matrix.col, weighted = T,add.colnames='label'))
  
  #degree centrality
  degree_centrality_us3run[i,] <- degree(G, loops = FALSE)/(vcount(G) - 1)
  #sort(deg_B_S, decreasing = TRUE,label=T)
  
  #closeness centrality
  closeness_centrality_us3run[i,] <- closeness(G)*(vcount(G) - 1)
  
  #eigenvector centrality
  eigenvec_centrality_us3run[i,] <- evcent(G)[[1]]
}


pdf("/Users/hienphamthu/Documents/PhD DISSERTATION/COVAR CDS/Eigenvector_Centrality/Closeness_us_3run.pdf",width=7,height=5)
plot(xdate,closeness_centrality_us3run[,1],xaxt="n",type="l", lwd = 2, col= arraycols[1], ylim = c(0,1.1),xlab="Date", ylab="Closeness Centrality of US FIs",cex.lab=1,cex.axis=1,cex.main=1,cex.sub=1)
lines(xdate,closeness_centrality_us3run[,2],lwd = 2, col= arraycols[2])
lines(xdate,closeness_centrality_us3run[,3],lwd = 2, col= arraycols[7])
lines(xdate,closeness_centrality_us3run[,4],lwd = 2, col= arraycols[9])
lines(xdate,closeness_centrality_us3run[,5],lwd = 2, col= arraycols[10])
lines(xdate,closeness_centrality_us3run[,6],lwd = 2, col= arraycols[14])
#lines(xdate,closeness_centrality_us[,7],lwd = 2, col= arraycols[15])
legend("topleft",x.intersp=0.1,xjust=0.5,yjust=0, c("CITI","BOA","GS","JPM","MS","AIG"),lty=c(1,1,1,1,1,1,1,1,1),
       lwd=(4),col=c(arraycols[1],arraycols[2],arraycols[7],arraycols[9],arraycols[10],arraycols[14]),ncol=8,bty='n',cex=0.7,seg.len=0.5)
axis(1, at=atx_sub, labels=format(atx_sub,  "%d.%m.%Y"), padj=0.5,cex.axis = 1)
dev.off()

#plot network
plot(G, layout=layout_with_fr, vertex.size=6,
     vertex.label.dist=0.5, vertex.color="red", edge.arrow.size=0.5)
