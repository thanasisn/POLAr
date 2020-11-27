
#' Get sport details from polar ppd file.
#'
#' @param ppdfile The ppd file for one person
#'
#' @return A data.frame with the sports names, id and short name
#' @export
#'
get_sport_index <- function(ppdfile) {

    #### read file
    day_info <- read.table( ppdfile,
                            fill             = TRUE,
                            strip.white      = FALSE,
                            stringsAsFactors = FALSE,
                            blank.lines.skip = FALSE,
                            fileEncoding     = "ISO8859-7")

    #### extract the info we need
    start_indx  <- which( apply(day_info, 1, function(x) any(grepl("PersonSports",  x)))) + 1
    end_indx    <- which( apply(day_info, 1, function(x) any(grepl("PersonHRZones", x)))) - 1

    sports      <- day_info[start_indx:end_indx,]

    indesss     <- which( apply(sports, 1, function(x) any(grepl("[a-z]|[A-Z]", x))))

    Breaks      <- c(0, which(diff(indesss) != 1), length(indesss)) + 1

    type_id     <- sports[indesss[Breaks] - 2, ]
    type_name   <- sports[indesss[Breaks]    , ]
    type_short  <- sports[indesss[Breaks] + 1, ]

    names       <- apply(type_name,  1, FUN = function(x) {paste(x[!is.na(x)],collapse = " ")} )
    short       <- apply(type_short, 1, FUN = function(x) {paste(x[!is.na(x)],collapse = " ")} )

    #### output
    sport_index <- cbind(names, type_id = type_id[,1], short)
    return(na.omit(data.frame(sport_index)))
}


