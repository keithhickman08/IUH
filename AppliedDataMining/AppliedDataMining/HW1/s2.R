x <-seq(1,200,by=5)
y <- (1 + 1/x)^x
png("plot2.png")
plot(x,y)
dev.off()