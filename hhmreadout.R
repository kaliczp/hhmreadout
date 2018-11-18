hhm.readout <- function(file, type = c("prec", "temp"), dateyearhundred = 20) {
    ## Determination of size in bytes
    size <- file.info(file)$size
    adat <- readBin(con = file, what= "raw", n = size,)
    ## Search for "ff" separator byte.
    separator <- which(adat == "ff")
    ## Between precipitation and temperature "ff 00 00 ff" is the separator
    ## followed by the %y %m %d date and the 1 hour temperature.
    separator.diff <- diff(separator)
    separator.loc <- which(separator.diff == 3)
    precip.end <- separator[separator.loc[1]]
    kezddate <- adat[(precip.end+4):(precip.end+6)]
    if(type[1] == "prec") {
        if(precip.end == 1) {
            result <- numeric()
            warning("No rain registered!")
        } else {
        prec.newdate <- separator[1:(separator.loc[1]-1)]
        prec.month <- adat[prec.newdate+1]
        prec.day <- adat[prec.newdate+2]
        prec.hour <- adat[prec.newdate+3]
        prec.hourlydate <- paste0(dateyearhundred,kezddate[1], "-",
                                  prec.month, "-",
                                  prec.day, " ",
                                  prec.hour)
        ## Each tip determination
        first.tip.in.hour <- (prec.newdate + 4)
        end.tip.in.hour <- c(prec.newdate[-1], precip.end)
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
    }
    else {
        ## New days begin with "ff 00 00 ff" in temperature series
        newday.begin <- separator[separator.loc]
        day.and.separator.idx <- rep(newday.begin,each = 7) + 0:6
        day.and.separator <- matrix(adat[day.and.separator.idx], ncol=7, byrow=TRUE)
        day.and.separator.idx.mat <- matrix(day.and.separator.idx,
                                            ncol=7, byrow=TRUE)
        ## If separators are wrongly catched then the middle columns are not zeroes
        if(sum(as.numeric(day.and.separator[,2:3])) != 0) {
            ## First four column test of day.and.separator
            day.sep.first <- as.numeric(day.and.separator[,2]) != 0
            day.sep.second <- as.numeric(day.and.separator[,3]) != 0
            nodays.idx <- !(day.sep.first | day.sep.second)
            day.and.separator <- day.and.separator[nodays.idx,]
            day.and.separator.idx.mat <- day.and.separator.idx.mat[nodays.idx,]
            day.and.separator.idx <- as.numeric(t(day.and.separator.idx.mat))
        }
        ## If was no precipitation then no problem if it was some additional indexes
        if(precip.end == 1) {
            notemp.idx <- day.and.separator.idx
        } else {
            notemp.idx <- c(1:(precip.end-1), day.and.separator.idx)
        }
        notemp.diff <- diff(c(notemp.idx,size+1))
        temp.daily.length <- notemp.diff[notemp.diff > 1] - 1
        temp.daily.hours <- temp.daily.length / 42
        ## Correction for hourly sleep
        temp.daily.hours[1] <- temp.daily.hours[1] -1
        temp.daily.hours[length(temp.daily.hours)] <- temp.daily.hours[length(temp.daily.hours)] +1
        ## Date-time generation
        yeartens.vec <- as.character(day.and.separator[, 5])
        year.vec <- paste0(dateyearhundred,yeartens.vec)
        month.vec <- as.character(day.and.separator[, 6])
        day.vec <- as.character(day.and.separator[, 7])
        current.date <- paste(year.vec, month.vec, day.vec, sep = "-")

        generic.time <- paste(0:23,"00",sep=":")
        datetime.char.vec <- paste(rep(current.date, each=24), generic.time)
        datetime.full.vec <- as.POSIXct(datetime.char.vec)
        first.valid.date <- 24-temp.daily.hours[1]+1
        datetime.startok.vec <- datetime.full.vec[first.valid.date:length(datetime.full.vec)]
        datetime.vec <- datetime.startok.vec[1:sum(temp.daily.hours)]

        ## Work with temp data
        temp.vector <- strtoi(x = adat[-notemp.idx], base = 16L)
        temp.matrix <- matrix(temp.vector, ncol = 3, byrow = TRUE)
        ## Restore 12 bit integers
        if(any(temp.matrix[,2] > 0)) {
            to.first <- bitwAnd(temp.matrix[,2], 15)*256
            to.third <- bitwAnd(temp.matrix[,2], 240)/16*256
            temp.matrix[,1] = temp.matrix[,1] + to.first
            temp.matrix[,3] = temp.matrix[,3] + to.third
            if(any(temp.matrix[,1] > 2048)) {
                temp.matrix[,1] <- temp.matrix[,1] - 4096
                }
            if(any(temp.matrix[,3] > 2048)) {
                temp.matrix[,3] <- temp.matrix[,1] - 4096
                }
        }
        ## Drop the middle column (fractional bits)
        corrected.temp.vector <- as.vector(t(temp.matrix[,c(1,3)]) / 10)
        onlytemp.matrix <- matrix(corrected.temp.vector, ncol=28, byrow=T)
        onlytemp.df <- as.data.frame(onlytemp.matrix)[,1:12]
        result <- cbind(datetime.vec, onlytemp.df)
    }
    result
}
