x <-seq(1,50,by=2)
y <- 2*x - 30
png("plot1.png")
plot(x,y)
dev.off()