hhm.readout <- function(file, type = c("prec", "temp"), dateyearhundred = 20) {
    ## Determination of size in bytes
    size <- file.info(file)$size
    adat <- readBin(con = file, what= "raw", n = size,)
    separator <- which(adat == "ff")
    separator.diff <- diff(separator)
    separator.loc <- which(separator.diff == 3)
    precip.end <- separator[separator.loc[1]]
    kezddate <- adat[(precip.end+4):(precip.end+6)]
    if(type[1] == "prec") {
        if(precip.end == 1) {
            stop("No rain registered!")
        }
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
    else {
        day.begin <- separator[separator.loc]+4
        full.day.length <- diff(day.begin[-1])
        if(any(full.day.length < 1015)) {
            wrong.separators <- which(full.day.length < 1015)
            wrong.sep.idx <- seq(2,length(wrong.separators),by=2)
            wrong.day.idx <- wrong.separators[wrong.sep.idx]+1
            day.begin <- day.begin[-wrong.day.idx]
        }
        day.date <- character()
        adat.tempvector <- numeric()
        for(akt.day in 1:length(day.begin)) {
            akt.day.date <- paste(paste0(dateyearhundred,
                                              adat[day.begin[akt.day]]),
                                       adat[day.begin[akt.day]+1],
                                       adat[day.begin[akt.day]+2],
                                       sep="-")
            first.temp <- day.begin[akt.day]+3
            last.temp <- ifelse(test = akt.day < length(day.begin),
                                yes = day.begin[akt.day + 1]-5,
                                no = size)
            hours.in.day <- (last.temp-first.temp+1)/ 42 # 42 data/day
            current.dates <- rep(akt.day.date,hours.in.day)
            if(akt.day == 1) {
                current.dates <- paste(current.dates,
                                       paste((24-hours.in.day):23,"00",sep=":"))
            } else {
                current.dates <- paste(current.dates,
                                       paste(0:(hours.in.day-1),"00",sep=":"))
            }
            day.date <- c(day.date, current.dates)
            adat.tempvector <- c(adat.tempvector,
                first.temp:last.temp)
        }
        temp.vector <- strtoi(x = adat[adat.tempvector], base = 16L)
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
        corrected.temp.vector <- as.vector(t(temp.matrix[,c(1,3)]) / 10)
        onlytemp.matrix <- matrix(corrected.temp.vector, ncol=28, byrow=T)[,1:12]
        result <- cbind(day.date, as.data.frame(onlytemp.matrix))
    }
    result
}
