# Code related to talk
library(tidyverse)
library(tourr)
library(geozoo)
library(dplyr)
library(viridis)
library(rlang)

s5 <- sphere.hollow(p=5)
colnames(s5$points) <- paste0("V", 1:5)
bases <- save_history(flea[, 1:5], grand_tour(1),
                         max = 5)
tour_path <- interpolate(bases, 0.1)

quartz()
animate_xy(s5$points, axes = "bottomleft")

tour_path_df <- as.data.frame.table(tour_path)
tour_path_mat <- matrix(tour_path_df$Freq, ncol=5, byrow=T)
colnames(tour_path_mat) <- paste0("V", 1:5)

pal <- viridis_pal()(20)
col <- c(rep(pal[1], 2500), rep(pal[2], 48))

s5_tp <- rbind(s5$points, tour_path_mat)
animate_xy(s5_tp, axes = "bottomleft", col=col)

s4 <- sphere.hollow(p=4)
colnames(s4$points) <- paste0("V", 1:4)
bases <- save_history(flea[, 1:4], grand_tour(1),
                      max = 30)
tour_path <- interpolate(bases, 0.1)
tour_path_df <- as.data.frame.table(tour_path)
tour_path_mat <- matrix(tour_path_df$Freq, ncol=4, byrow=T)
colnames(tour_path_mat) <- paste0("V", 1:4)
s4_tp <- rbind(s4$points, tour_path_mat)
col <- c(rep(pal[2], nrow(s4$points)), rep(pal[1], nrow(tour_path_mat)))
pch <- c(rep(".", nrow(s4$points)), rep("o", nrow(tour_path_mat)))
edges <- cbind((nrow(s4$points)+1):(nrow(s4$points)+nrow(tour_path_mat)-1), (nrow(s4$points)+2):(nrow(s4$points)+nrow(tour_path_mat)))
colnames(edges) <- c("from", "to")
animate(s4_tp, grand_tour(),
        display_xy(axes = "bottomleft",
                   col=col, pch=pch, edges=edges))

s3 <- sphere.hollow(p=3)
colnames(s3$points) <- paste0("V", 1:3)
bases <- save_history(flea[, 1:3], grand_tour(1),
                      max = 30)
tour_path <- interpolate(bases, 0.1)
tour_path_df <- as.data.frame.table(tour_path)
tour_path_mat <- matrix(tour_path_df$Freq, ncol=3, byrow=T)
colnames(tour_path_mat) <- paste0("V", 1:3)
s3_tp <- rbind(s3$points, tour_path_mat)
col <- c(rep(pal[15], nrow(s3$points)), rep(pal[1], nrow(tour_path_mat)))
pch <- c(rep(".", nrow(s3$points)), rep("o", nrow(tour_path_mat)))
edges <- cbind((nrow(s3$points)+1):(nrow(s3$points)+nrow(tour_path_mat)-1), (nrow(s3$points)+2):(nrow(s3$points)+nrow(tour_path_mat)))
colnames(edges) <- c("from", "to")
animate(s3_tp, grand_tour(),
        display_xy(axes = "bottomleft",
                   col=col, pch=pch, edges=edges))

library(plotly)
s5 <- sphere.hollow(p=5)
colnames(s5$points) <- paste0("V", 1:5)
bases <- save_history(flea[, 1:5], grand_tour(2),
                      max = 3)
tour_path <- interpolate(bases, 0.1)
d <- dim(tour_path)
mydat <- NULL
for (i in 1:d[3]) {
  cat(i, "\n")
  pd <- s5$points %*% matrix(tour_path[,,i], ncol=2)
  mydat <- rbind(mydat, cbind(pd, rep(i, nrow(pd))))
}
colnames(mydat) <- c("x", "y", "indx")
df <- as.tibble(mydat)

p <- ggplot(data = df, aes(x = x, y = y) ) +
    geom_point(size = .7,
      aes(frame = indx)) +
  theme_void() +
  coord_fixed() +
  theme(legend.position = "none")
ggplotly(p)
