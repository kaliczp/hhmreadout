hhm.readout <- function(file) {
    size <- file.info(file)$size
    adat <- readBin(con = file, what= "raw", n = ttsize,)
    separator <- which(adat == "ff")
    if(length(separator) < 3) {
        stop("No rain registered!")
    }
    csadat <- adat[(separator[1]+1):(separator[2]-1)]
    kezddate <- adat[(separator[3]+1):(separator[3]+3)]
    min <- csadat[seq(4,length(csadat)-1,2)]
    sec <- csadat[seq(5,length(csadat),2)]
    paste0("20",kezddate[1], "-", csadat[1], "-", csadat[2], " ",
           csadat[3], ":", min, ":", sec)
}
