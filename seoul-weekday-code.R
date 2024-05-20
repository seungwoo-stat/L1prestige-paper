### CENTRALITY vs. PRESTIGE - CODE

library(latex2exp)
library(igraph)
library(L1centrality)

seoulweekday <- readRDS("seoul-move-weekday.rds")
seoulweekday.edge <- as.data.frame(as_edgelist(seoulweekday))
seoulweekday.edge$movement <- 1/E(seoulweekday)$weight
names(seoulweekday.edge)[1:2] <- c("start","end")

eta <- seoulweekday.edge$movement[seoulweekday.edge$start == seoulweekday.edge$end]
nametemp <- seoulweekday.edge$start[seoulweekday.edge$start == seoulweekday.edge$end]
eta <- eta[match(V(seoulweekday)$name,nametemp)]

L1cent <- L1cent(seoulweekday, eta = eta, mode = "centrality")
L1pres <- L1cent(seoulweekday, eta = eta, mode = "prestige")

{ # 7 x 7 plot
plot(ecdf(L1cent)(L1cent), ecdf(L1pres)(L1pres), asp = 1,
     pch = 21, col = "black", 
     cex = 1.5,
     bg = heat.colors(424, alpha = 0.8)[rank(V(seoulweekday)$resident - V(seoulweekday)$worker)],
     xlab = TeX("$L_1$ centrality (uniform margin)"),
     ylab = TeX("$L_1$ prestige (uniform margin)"),
     main=TeX("$L_1$ centrality vs. prestige", bold = TRUE)
     )
op <- par(fig=c(grconvertX(c(0.95, 1), from="user", to="ndc"),
                grconvertY(c(0.8, 0.95), from="user", to="ndc")), 
          mar=c(0,0,0,0), 
          new=TRUE
          )

plot(c(0, 1), c(0, 1), type='n', axes=F, xlab='', ylab='')
legend_image <- as.raster(matrix(heat.colors(424, alpha = 1), ncol=1))
rasterImage(legend_image, 0, 0, 1, 1)
mtext(c("High","Low"), 2, 0, at=c(1-0.07, 0+0.07), las=2, cex=.9)
mtext(TeX('rank(# workers $-$ # residents)', bold = TRUE), 3, 0, adj=1)
par(op)
}

index <- rank(V(seoulweekday)$resident - V(seoulweekday)$worker)
cor.test(ecdf(L1pres)(L1pres), ecdf(L1cent)(L1cent), alternative = "less")
cor.test(-index, ecdf(L1cent)(L1cent), alternative = "less")
cor.test(-index, ecdf(L1pres)(L1pres), alternative = "greater")


### Figure in the Introduction: 9 x 9 plot
library(sp)
library(sf)
library(shape)
seoulmap <- readRDS("seoulmap.rds")
centers <- st_centroid(st_as_sf(seoulmap))
centers <- data.frame(n = paste(centers$gu,centers$dong),
                      matrix(unlist(centers$geometry),ncol=2,byrow=TRUE))
edgelist <- seoulweekday.edge[seoulweekday.edge$movement >= 2000,]
edgelist <- edgelist[edgelist$start != edgelist$end,]
edgelist <- edgelist[order(edgelist$movement),]

curved.Arrow <- \(x0,y0,x1,y1,lwd=1,alpha0=0.3,alpha1=1){
  theta <- atan2(y1-y0,x1-x0)
  rot.max <- matrix(c(cos(theta),sin(theta),-sin(theta),cos(theta)),ncol=2)
  
  x.temp <- seq(-1,0.6,length.out = 15)
  x.temp <- c(x.temp,seq(x.temp[15]*2 - x.temp[14],1,length.out=15))
  y.temp <- cosh(x.temp)/3
  
  segs.new <- rot.max %*% rbind(x.temp, y.temp)/2*sqrt((y1-y0)^2 + (x1-x0)^2)
  x.new <- segs.new[1,] - segs.new[1,1] + x0
  y.new <- segs.new[2,] - segs.new[2,1] + y0
  segments(x.new[1:29], y.new[1:29],
           x.new[2:30], y.new[2:30],
           col = rev(heat.colors(19, alpha = seq(alpha1,alpha0,length.out=19)))[c(1:15,rep(16,3),rep(17,4),rep(18,3),rep(19,4))], 
           lwd = c(seq(lwd,lwd*3,length.out=15),seq(lwd*3,0,length.out=14)), 
           lend = 1)
}

op <- par(mar = c(1, 1, 1, 1))
plot(seoulmap, main = "(a) Weekday morning movements",
     col = "gray80", 
     border = "white")
for(i in 1:nrow(edgelist)){
  start <- centers[match(edgelist[i,]$start, centers$n),c("X1","X2")]
  end <- centers[match(edgelist[i,]$end, centers$n),c("X1","X2")]
  curved.Arrow(start$X1, start$X2, end$X1, end$X2, lwd = log(edgelist[i,]$movement - 1999)/4,
               alpha0 = 0.2, alpha1 = 0.3)
}
L1pres <- sort(L1pres, decreasing = TRUE)
L1pres.top <- names(L1pres)[L1pres >= quantile(L1pres, 1-5/424)]
plot(seoulmap[paste(seoulmap$gu,seoulmap$dong) %in% L1pres.top,],
     col = NA, border = "black", lwd = 2, add = TRUE)
text(centers[match(L1pres.top,centers$n),c("X1","X2")] + c(c(3,1.5,1.5,2,1.5)/100,c(0,-0.5,-0.2,0,0)/100), 
     paste0("(",1:5,")"), font=2)
curved.Arrow(126.8, 37.65, 126.8, 37.68, lwd = 5)
text(126.8, 37.65, "source",pos=1,font=2)
text(126.8, 37.68, "destination",pos=3,font=2)
par(op)


