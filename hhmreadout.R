hhm.readout <- function(file, type = c("prec", "temp"), dateyearhundred = 20) {
    size <- file.info(file)$size
    adat <- readBin(con = file, what= "raw", n = ttsize,)
    separator <- which(adat == "ff")
    separator.diff <- diff(separator)
    separator.loc <- which(separator.diff == 3)
    precip.end <- separator.loc[1]
    kezddate <- adat[(separator[precip.end]+4):(separator[precip.end]+6)]
    if(type[1] == "prec") {
        if(precip.end == 1) {
            stop("No rain registered!")
        }
        prec.newdate <- separator[1:(precip.end-1)]
        prec.month <- adat[prec.newdate+1]
        prec.day <- adat[prec.newdate+2]
        prec.hour <- adat[prec.newdate+3]
        prec.hourlydate <- paste0(dateyearhundred,kezddate[1], "-",
                                  prec.month, "-",
                                  prec.day, " ",
                                  prec.hour)
        ## Each tip determination
        first.tip.in.hour <- (prec.newdate + 4)
        end.tip.in.hour <- c(prec.newdate[-1], separator[precip.end])
        repeat.to.tips <- (end.tip.in.hour - first.tip.in.hour)/2

        prec.hourlydate.full <- rep(prec.hourlydate, repeat.to.tips)

        tip.index <- seq(first.tip.in.hour[1], by = 2, length.out = repeat.to.tips[1])
        if(length(first.tip.in.hour) > 1) {
            for(curr.date in 2:length(first.tip.in.hour)) {
                tip.index <- c(tip.index,
                               seq(first.tip.in.hour[curr.date],
                                   by = 2,
                                   length.out = repeat.to.tips[curr.date]
                                   )
                               )
            }
        }
        prec.min <- adat[tip.index]
        prec.sec <- adat[tip.index+1]
        result <- paste0(prec.hourlydate.full, ":", prec.min, ":", prec.sec)
    }
    else {
        temp.matrix <- matrix(strtoi(x = ttadat[8:25], base = 16L),
                              ncol = 3, byrow = TRUE)
        if(any(temp.matrix[,2] > 0)) {
            to.first <- bitwAnd(temp.matrix[,2], 15)*256
            to.third <- bitwAnd(temp.matrix[,2], 240)/16*256
            temp.matrix[,1] = temp.matrix[,1] + to.first
            temp.matrix[,3] = temp.matrix[,3] + to.third
            if(temp.matrix[,1] > 2048) {
                temp.matrix[,1] <- temp.matrix[,1] - 4096
                }
            if(temp.matrix[,3] > 2048) {
                temp.matrix[,3] <- temp.matrix[,1] - 4096
                }
        }
        result <- temp.matrix[,c(1,3)]
    }
    result
}
