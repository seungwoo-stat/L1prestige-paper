### LOCAL PRESTIGE - CODE

library(latex2exp)
library(igraph)
library(L1centrality)
library(sp)

seoulweekend <- readRDS("seoul-move-weekend.rds")
seoulweekend.edge <- as.data.frame(as_edgelist(seoulweekend))
seoulweekend.edge$movement <- 1/E(seoulweekend)$weight
names(seoulweekend.edge)[1:2] <- c("start","end")

seoulweekend.edge$start.gu <- V(seoulweekend)$gu[match(seoulweekend.edge$start,V(seoulweekend)$name)]
seoulweekend.edge$end.gu <- V(seoulweekend)$gu[match(seoulweekend.edge$end,V(seoulweekend)$name)]

eta <- seoulweekend.edge$movement[seoulweekend.edge$start == seoulweekend.edge$end]
nametemp <- seoulweekend.edge$start[seoulweekend.edge$start == seoulweekend.edge$end]
eta <- eta[match(V(seoulweekend)$name,nametemp)]

globalpres <- L1cent(seoulweekend, eta = eta, mode = "prestige")
localpres <- L1centLOC(seoulweekend, eta = eta,
                       alpha = (seq(15,420,by=5) + 0.1)/424, # add 0.1 to prevent rounding errors
                       mode = "prestige")
# saveRDS(localpres, "localpres.rds")
# localpres <- readRDS("localpres.rds")

localpres.mat <- do.call(cbind, localpres |> lapply(\(l) ecdf(l)(l)))
rownames(localpres.mat) <- names(localpres[[1]])
margindiff <- apply(localpres.mat, 1, \(v) max(v) - min(v))
largediff.index <- which(margindiff >= 0.6)
nowonindex <- which(names(margindiff) |> startsWith("Nowon-gu"))
dobongindex <- which(names(margindiff) |> startsWith("Dobong-gu"))

### Figure: Multiscale view of Seoul weekend flow network: 10 x 7
dblue <- "#2d3192"; ngreen <- "#4da94e" 
op <- par(mar = c(5, 4, 4, 11) + 0.1)
plot(NA, xlim = c(15/424, 1.05), ylim = c(0,1),
     xlab = TeX("$\\alpha$"), 
     ylab = TeX("Local $L_1$ prestige (uniform margin)"),
     main = "Multiscale view of Seoul weekend flow network",
     bty = "l")
set.seed(0)
matlines(x = (seq(15,420,by=5)/424), 
         y = t(localpres.mat[sample((1:424)[-largediff.index],100),]), # randomly sample 100 dongs
         lty = 1, col = "gray", lwd = 1)
matlines(x = (seq(15,420,by=5)/424),
         y = t(localpres.mat[intersect(nowonindex, largediff.index),]),
         lty = 1, col = ngreen, lwd = 3)
matlines(x = (seq(15,420,by=5)/424),
         y = t(localpres.mat[intersect(dobongindex, largediff.index),]),
         lty = 1, col = dblue, lwd = 3)

textlabel <- localpres.mat[largediff.index, ncol(localpres.mat)] |> sort() |> names()
textgu <- V(seoulweekend)$gu[match(textlabel, V(seoulweekend)$name)]
textcol <- textgu
textcol[textgu == "Nowon-gu"] <- ngreen
textcol[textgu == "Dobong-gu"] <- dblue
textlabel <- V(seoulweekend)$dong[match(textlabel, V(seoulweekend)$name)]
textpos <- localpres.mat[largediff.index, ncol(localpres.mat)] |> sort()
textpos.margin <- seq(textpos[1], 0.6, length.out= length(textpos)+1)

text(x = rep(1.05, length(textcol)), 
     y = textpos.margin + c(rep(0,length(textpos.margin)-1), 0.01),
     c(textlabel,"Dongs with large change"), 
     cex = c(rep(1,length(textpos.margin)-1), 1.1),
     pos = 4, col = c(textcol, "black"), 
     xpd = NA, font = c(rep(1,length(textpos.margin)-1),2))
matlines(x = c(1,1.05), y = rbind(textpos, textpos.margin[-length(textpos.margin)]),
         col = "black", lty = 1, xpd = NA)
# par(mar = c(5, 4, 4, 2) + 0.1)
par(op)


### Analysis (Table 5-1)
## total number of incoming and outgoing movements - derive hubs
startsum <- aggregate(seoulweekend.edge$movement, by = list(start = seoulweekend.edge$start), sum)
endsum <- aggregate(seoulweekend.edge$movement, by = list(end = seoulweekend.edge$end), sum)
names(startsum)[2] <- "move"
names(endsum)[2] <- "move"

start.fence <- quantile(startsum$move,0.75) + 1.5*IQR(startsum$move)
end.fence <- quantile(endsum$move,0.75) + 1.5*IQR(endsum$move)

start.outlier <- startsum[startsum$move >= start.fence, 1]
end.outlier <- endsum[endsum$move >= end.fence, 1]
start.end.outlier <- intersect(start.outlier, end.outlier) # hubs' name
list(start.outlier, end.outlier, start.end.outlier) |> sapply(length) # number of outliers: 22, 22, 21

## distinct behavior of Dobong-gu and Nowon-gu
# maximum number of incoming (outgoing) movements from (to) hubs to each dongs
fromhubs <- by(seoulweekend.edge, seoulweekend.edge$end, 
                   \(m) m[m$start %in% start.end.outlier,"movement"] |> max()) |> c()
tohubs <- by(seoulweekend.edge, seoulweekend.edge$start, 
                 \(m) m[m$end %in% start.end.outlier,"movement"] |> max()) |> c()
fromhubs.gu <- aggregate(fromhubs, by = list(V(seoulweekend)$gu[match(names(fromhubs),V(seoulweekend)$name)]),max)
knitr::kable(t(fromhubs.gu[order(fromhubs.gu$x)[1:5],]),format="latex")
tohubs.gu <- aggregate(tohubs, by = list(V(seoulweekend)$gu[match(names(tohubs),V(seoulweekend)$name)]),max)
knitr::kable(t(tohubs.gu[order(tohubs.gu$x)[1:5],]),format="latex")

# proportion of incoming (outgoing) movements from (to) the four northernmost regions
fromnorth <- by(seoulweekend.edge, seoulweekend.edge$end,
                    \(m) sum(m[m$start.gu %in% c("Nowon-gu","Dobong-gu","Jungnang-gu","Gangbuk-gu"),"movement"])/
                      sum(m$movement)) |> c()
tonorth <- by(seoulweekend.edge, seoulweekend.edge$start,
                    \(m) sum(m[m$end.gu %in% c("Nowon-gu","Dobong-gu","Jungnang-gu","Gangbuk-gu"),"movement"])/
                      sum(m$movement)) |> c()
aggregate(fromnorth, by = list(V(seoulweekend)$gu[match(names(fromnorth),V(seoulweekend)$name)]),
          mean) |> View()
aggregate(tonorth, by = list(V(seoulweekend)$gu[match(names(tonorth),V(seoulweekend)$name)]),
          mean) |> View()

# average number of hubs included in the alpha = 15/424 neighborhood vertex
nbpres <- L1centNB(seoulweekend, eta = eta, mode = "prestige")
# saveRDS(nbpres, "nbpres.rds")
# nbpres <- readRDS("nbpres.rds")
nb.leq.15 <- nbpres |> 
  sapply(\(l) sum(which(names(sort(l, decreasing = TRUE)) %in% start.end.outlier) <= 16))
aggregate(nb.leq.15, by = list(V(seoulweekend)$gu[match(names(nb.leq.15), V(seoulweekend)$name)]), mean)

# average number of northernmost regions included in the alpha = 15/424 neighborhood vertex
nb.leq.15.ndjg <- nbpres |>
  sapply(\(l) sum(V(seoulweekend)$gu[match(names(sort(l, decreasing = TRUE))[1:16], V(seoulweekend)$name)] %in% c("Nowon-gu","Dobong-gu","Jungnang-gu","Gangbuk-gu")))
aggregate(nb.leq.15.ndjg, by = list(V(seoulweekend)$gu[match(names(nb.leq.15.ndjg), V(seoulweekend)$name)]), mean)

# number of Nowon-gu and Dobong-gu regions that have local L1 prestige (alpha = 420/424) below the median
sum(localpres.mat[startsWith(rownames(localpres.mat),"Nowon-gu") | 
                    startsWith(rownames(localpres.mat),"Dobong-gu"), 82] <= median(localpres.mat[,82]))

# correlation: movement from northern regions vs local L1 prestige (alpha = 15/424)
endsum.north <- by(seoulweekend.edge, seoulweekend.edge$end,
                       \(m) sum(m[m$start.gu %in% c("Nowon-gu","Dobong-gu","Jungnang-gu","Gangbuk-gu"),"movement"])) |> c()
local.15.424 <- localpres.mat[rownames(localpres.mat) |> startsWith("Nowon-gu") | rownames(localpres.mat) |> startsWith("Dobong-gu"),1]
cor.test(local.15.424, endsum.north[names(local.15.424)], alternative = "greater")


### Figure: Map of Seoul: 7 x 7
seoulmap <- readRDS("seoulmap.rds")

## borders of the first-tier divisions
gu.name <- seoulmap$gu |> unique()
seoulmap.gu <- raster::bind(raster::aggregate(seoulmap[seoulmap$gu == gu.name[1],]),
                            raster::aggregate(seoulmap[seoulmap$gu == gu.name[2],]))
for(i in 3:25){
  seoulmap.gu <- raster::bind(seoulmap.gu,
                              raster::aggregate(seoulmap[seoulmap$gu == gu.name[i],]))
}

jpurple <- "#9d1b85"; gorange <- "#f18d00" 
op <- par(mar = c(3, 2, 2, 1))
plot(seoulmap, 
     col = "gray70", 
     border = "white")
plot(seoulmap[paste(seoulmap$gu,seoulmap$dong) %in% start.end.outlier,], 
     col = "red", 
     border = "white", add = TRUE)
plot(seoulmap[seoulmap$gu == "Nowon-gu",], 
     col = ngreen,
     border = "white", add = TRUE)
plot(seoulmap[seoulmap$gu == "Dobong-gu",], 
     col = dblue,
     border = "white", add = TRUE)
plot(seoulmap[seoulmap$gu == "Jungnang-gu",], 
     col = jpurple,
     border = "white", add = TRUE)
plot(seoulmap[seoulmap$gu == "Gangbuk-gu",], 
     col = gorange,
     border = "white", add = TRUE)
plot(seoulmap.gu, border = "black", lwd=2, add = TRUE)
legend(x=126.76,y=37.7,bty = "n",
       cex=1.3,
       col=c("red",ngreen,dblue,jpurple,gorange),pch=15,
       legend=c("Hubs of Seoul","Nowon-gu","Dobong-gu","Jungnang-gu","Gangbuk-gu"))
par(op)


### Figure in the Introduction: 9 x 9 plot
library(sf)
library(shape)
centers <- st_centroid(st_as_sf(seoulmap))
centers <- data.frame(n = paste(centers$gu,centers$dong),
                      matrix(unlist(centers$geometry),ncol=2,byrow=TRUE))
edgelist <- seoulweekend.edge[seoulweekend.edge$movement >= 4000,]
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
           col = rev(hcl.colors(19, "YlGn", alpha = seq(alpha1,alpha0,length.out=19)))[c(1:15,rep(16,3),rep(17,4),rep(18,3),rep(19,4))], 
           lwd = c(seq(lwd,lwd*3,length.out=15),seq(lwd*3,0,length.out=14)), 
           lend = 1)
}

op <- par(mar = c(1, 1, 1, 1))
plot(seoulmap, main = "(b) Weekend movements",
     col = "gray80", 
     border = "white")
for(i in 1:nrow(edgelist)){
  start <- centers[match(edgelist[i,]$start, centers$n),c("X1","X2")]
  end <- centers[match(edgelist[i,]$end, centers$n),c("X1","X2")]
  curved.Arrow(start$X1, start$X2, end$X1, end$X2, lwd = log(edgelist[i,]$movement - 1999)/4,
               alpha0 = 0.2, alpha1 = 0.4)
}
plot(seoulmap[paste(seoulmap$gu,seoulmap$dong) %in% names(largediff.index),],
     col = NA, border = "black", lwd = 2, add = TRUE)
curved.Arrow(126.8, 37.65, 126.8, 37.68, lwd = 5)
text(126.8, 37.65, "source",pos=1,font=2)
text(126.8, 37.68, "destination",pos=3,font=2)
par(op)