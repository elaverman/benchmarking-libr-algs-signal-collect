hostname <- Sys.info()[["nodename"]]

if (hostname == 'deepthought-rmbp.local') {
    # thesisdir <- '~/Dev/BA/signal-collect-dcops/'
    thesisdir <- '~/Dev/dcop-bachelor-thesis/signal-collect-dcops/'
} else {
    thesisdir <- '~/Dev/dcop-bachelor-thesis/signal-collect-dcops/'
}

setwd(thesisdir)



readData <- function(filename) {
    # skip the first row since header=FALSE wasn't enough
    # R didn't seem to recognize the first row as a header
    # since it wasn't the same length as all other rows
    d <- read.csv(filename, header=FALSE, fill=TRUE, skip=1)

    # construct header
    fstline <- readLines(filename, 1)
    fstline <- gsub('\"', '', fstline)

    header.fst <- strsplit(fstline, ',')[[1]]
    header.fst <- header.fst[1:length(header.fst)-1]

    print(header.fst)

    header.nunnamed <- ncol(d) - length(header.fst)

    header.snd <- sapply(1:header.nunnamed, function (x) {
                         sprintf("ratio_%d", x) })

    d.header <- c(header.fst, header.snd)

    # assign header
    colnames(d) <- d.header

    # convert NA to 1
    d[is.na(d)] <- 1

    d
}



calculateMetrics <- function(dframe) {
    alg.names <- unique(dframe$algorithmName)
    num.col.idxs <- sapply(dframe, is.numeric) # logical
    dframe.num.only <- dframe[ , num.col.idxs]
    dframe.num.only[ dframe.num.only == -1.0 ] <- 0

    dframe.compressed <- data.frame()

    # Drop avgRatio if it exists
    #avg.ratio.idx <- which(names(dframe) == "avgRatio")
    #if (!is.null(avg.ratio.idx) || avg.ratio.idx != 0) {
        #dframe <- dframe[-avg.ratio.idx]
    #}

    for (alg in alg.names) {
        algdata <- dframe.num.only[dframe$algorithmName == alg, ]

        conv.opt.n <- algdata$timeToOptimum # time to optimum
        conv.opt.steps <- conv.opt.n[ conv.opt.n > 0 ] # filter out converged
        conv.opt.mean <- mean(conv.opt.steps) # mean number of steps

        # a vector or 1 and 0, indicating if the global optimum has been reached in the respective run
        conv.opt.converged <- ifelse(conv.opt.n != 0, 1, 0)
        conv.opt.ratio <- mean(conv.opt.converged)

        conv.nash.n <- algdata$timeToNashEq
        conv.nash.steps <- conv.nash.n[ conv.nash.n > 0 ]
        conv.nash.mean <- mean(c(conv.nash.steps, conv.opt.steps))

        # a vector or 1 and 0, indicating if a nash eq has been reached in the respective run
        conv.nash.converged <- ifelse(conv.nash.n != 0, 1, 0)
        # since a global optimum is also a nash eq, OR both vectors
        conv.nash.ratio <- mean(conv.opt.converged | conv.nash.converged)

        algdata.mean <- lapply(algdata, mean)

        ratio.start.idx <- which(names(algdata) == "ratio_1")
        all.ratios <- algdata[ratio.start.idx:length(algdata)]


        algdata.mean[["meanRatio"]] <- mean(sapply(algdata[15:length(algdata)], mean))

        algdata.mean[["optRatio"]] <- conv.opt.ratio
        algdata.mean[["nashRatio"]] <- conv.nash.ratio

        algdata.mean[["avgOptTime"]] <- conv.opt.mean
        algdata.mean[["avgNashTime"]] <- conv.nash.mean

        dframe.compressed <- rbind(dframe.compressed, algdata.mean)
    }

    dframe.compressed[["alg.names"]] <- alg.names
    n.cols <- length(dframe.compressed)
    ret <- as.data.frame(dframe.compressed[ , c(n.cols:(n.cols-5), 1:(n.cols-6))])
    drops <- c("graphNumber", "runNumber", "graphSize", "meanDegree")
    ret[ ,!(names(ret) %in% drops)]
}



plotAlgs <- function(d, xlim=NULL, ylim=c(0,1)) {

    par(cex=1.25)
    n.alg <- nrow(d)
    rainbowcols <- rainbow(n.alg, start=0.05)


    pchs <- 1:n.alg
    ltys <- 1:n.alg

    ret = list(plot.cols=rainbowcols)

    for (i in 1:n.alg) {

        alg <- d$alg.name[i] # the current algorithm

        entry <- d[d$alg.name == alg, ]

        # calculate metrics
        ratios.startidx <- which(colnames(d) == "ratio_1")
        ratios.all <- entry[ , ratios.startidx:ncol(entry)]

        ratios.averaged <- apply(ratios.all, 2, mean)

        xss <- 0:(length(ratios.averaged)-1)
        yss <- ratios.averaged

        if (i == 1) {
            xlim <- if (is.null(xlim)) c(0, length(xss)) else xlim
            yss.nonplot <- xss/(length(xss)-1)

            par(bg = "white")
            plot(xss, yss.nonplot,
                 type="n",
                 xlab="t [steps]", ylab=expression(Q[t]),
                 xlim=xlim,
                 ylim=ylim
            )

            ret$xlim <- xlim
            ret$ylim <- ylim
        }

        # plot lines
        lines(xss, yss, pch=pchs[i], col=rainbowcols[i], lwd=1.25, lty=ltys[i])

    }
    ret
}


filename <- '~/Dev/dcop-bachelor-thesis/signal-collect-dcops/results/full-50-1.csv'

filename.png <- paste(gsub('\\.csv', '', basename(filename)), '.png', sep='')
filename.ps <- paste(gsub('\\.csv', '', basename(filename)), '.eps', sep='')
filename.txt <- paste(gsub('\\.csv', '', basename(filename)), '.txt', sep='')
filename.png.abs <- paste(dirname(filename), '/', filename.png, sep='')
filename.ps.abs <- paste(dirname(filename), '/', filename.ps, sep='')
filename.txt.abs <- paste(dirname(filename), '/', filename.txt, sep='')

setEPS()
postscript(filename.ps.abs, width=8)

# Trim off excess margin space (bottom, left, top, right)
par(mar=c(3, 2.9, 0.2, 0.7))
# Trim off excess outer margin space (bottom, left, top, right)
par(oma=c(0,0,0,0))
# Trim off excess space for label and ticks (label, ticks, line)
par(mgp=c(1.9,0.6,0))

d <- readData(filename)

d.avg <- calculateMetrics(d)

plot.params <- plotAlgs(d.avg, ylim=c(0.00,1.0))

plot.xlim <- plot.params$xlim
plot.ylim <- plot.params$ylim

legend.x <- (plot.xlim[2])
legend.y <- (plot.ylim[1])

plot.cols <- plot.params$plot.cols

leg <-  c(
          expression(paste(eta == 5.0)),
          expression(paste(eta == 1.0)),
          expression(paste(eta == 0.5)),
          expression(paste(eta == 0.1)),
          expression(paste(eta == 0.05)),
          expression(paste(eta == 0.0))
         )

algnames <- d.avg$alg.name

leg <- c( "EP I = 0.9",
          "EP I = 0.7",
          "EP I = 0.5",
          "EP I = 0.3",
          "EP I = 0.1")

legend(legend.x, legend.y,
      #leg,
      algnames,
      fill=plot.cols, col=plot.cols, lwd=1.25, lty=(1:10), bg='white', box.lty=3, xjust=1, yjust=0)

# extraction and reordering of data in xx.txt
cols <- c(1,6,12,5,4,3,2, 13)

#d.avg[ ,1:13][c(1, 11, 10,
print(d.avg[ ,cols])

write.table(d.avg[ , cols], filename.txt.abs)

dev.off()
