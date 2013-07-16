temp <- function(i,const, k) {
    const / (i ** k)
}
dsan <- function(delta, i, c, k) {
    exp(delta/temp(i, c, k))
}

ks <- c(2)
max.step <- 250
deltas <- c(-1, -3, -5)

for (const in c(1,150, 250)) {
    setEPS()
    postscript(sprintf('dsan-%d.eps', const), width=8)
    #png('dsan.png', width = 800, height = 300)

    for (k in ks) {
        plot(dsan(1, (0:max.step), const, k), type = "n", xlab="time [step]",
             ylab="P(X)", main="",
             ylim=c(0,1), xlim=c(0,50), cex.main=1.75)
        itr <- 1
        for (delta in deltas) {
            lines(dsan(delta, (0:max.step), const, k), type = "l", lty=itr)
            itr <- itr + 1
        }
        legend.names <- c(expression(Delta == -1),
                          expression(Delta == -3),
                          expression(Delta == -5))
        legend("topright", legend=legend.names, lty=1:itr, cex=1.75)
    }
    dev.off()
}
