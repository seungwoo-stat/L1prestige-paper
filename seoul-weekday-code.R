### CENTRALITY vs. PRESTIGE - CODE

library(latex2exp)
library(igraph)
# devtools::install_github("seungwoo-stat/L1centrality")
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

