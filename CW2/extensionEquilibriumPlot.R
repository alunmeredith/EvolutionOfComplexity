load('resultsList.rda')
library(RColorBrewer)
levels <- 11


####

test <- resultsList[[8]]
test <- test %>% select(-Sum, -group)
test <- as.matrix(test)
test2 <- apply(test, 1, which.max)
dim(test2) <- c(100,8,5)
dim(test) <- c(100,8,5,11)

# Calcualte average over 5 repeats
average <- apply(test, MARGIN = c(1,2,4), mean)

# Find currently dominant greed level
dom <- apply(average, MARGIN = c(1,2), which.max)

# Draw graph
heatmap(dom, Rowv = NA, Colv = NA)

dat_long <- reshape2::melt(dom)
dat_long$value <- factor(dat_long$value)
names(dat_long) <- c("Time", "GroupSize", "Greed")

gg <- ggplot(dat_long) +
  geom_tile(aes(x=GroupSize, y=Time, fill=Greed)) +
  scale_fill_manual(values = grey.colors(11)) +
  theme_bw() +
  scale_y_continuous(breaks = seq(0,100,20))
gg
