

#### functions ####
get_value <- function (file_data,apattern) {
    value <- regmatches(file_data[grep(file_data,pattern=apattern)],
                        regexec(file_data[grep(file_data,pattern=apattern)],pattern="=.*"))[[1]]
    substr(value,start=2,stop=1000)
}




#### polar logs

## here are files congaing examples of heart rate and R-R heart rate
folder <- "./data-raw"


hrms_files <- list.files( path = folder,
                          recursive = TRUE,
                          full.names = TRUE,
                          pattern = ".hrm$")



file_data <- scan(file=hrms_files[1],what="character",fileEncoding="ISO8859-7")

head(file_data,n=25)





# read [Parms] ------------------------------------------------------------

version     <- get_value(file_data, "Version="     )
monitor     <- get_value(file_data, "Monitor="     )
smode       <- get_value(file_data, "SMode="       )
date        <- get_value(file_data, "Date="        )
starttime   <- get_value(file_data, "StartTime="   )
length      <- get_value(file_data, "Length="      )
interval    <- get_value(file_data, "Interval="    )
upper1      <- get_value(file_data, "Upper1="      )
upper2      <- get_value(file_data, "Upper2="      )
upper3      <- get_value(file_data, "Upper3="      )
lower1      <- get_value(file_data, "Lower1="      )
lower2      <- get_value(file_data, "Lower2="      )
lower3      <- get_value(file_data, "Lower3="      )
timer1      <- get_value(file_data, "Timer1="      )
timer2      <- get_value(file_data, "Timer2="      )
timer3      <- get_value(file_data, "Timer3="      )
activelimit <- get_value(file_data, "ActiveLimit=" )
maxHR       <- get_value(file_data, "MaxHR="       )
restHR      <- get_value(file_data, "RestHR="      )
startdelay  <- get_value(file_data, "StartDelay="  )
vo2max      <- get_value(file_data, "VO2max="      )
weight      <- get_value(file_data, "Weight="      )


params_vector <- c(version,
                   monitor,
                   smode,
                   date,
                   starttime,
                   length,
                   interval,
                   upper1,
                   upper2,
                   upper3,
                   lower1,
                   lower2,
                   lower3,
                   timer1,
                   timer2,
                   timer3,
                   activelimit,
                   maxHR,
                   restHR,
                   startdelay,
                   vo2max,
                   weight)

# sanity check
if ( version == 106 ){
    print("expected version")
} else {
    warning("check file version comptatibility")
}


# read notes --------------------------------------------------------------

first.index <- which(file_data=="[Note]")+1
last.index  <- which(file_data=="[IntTimes]")-1
span        <- last.index-first.index
if ( span >= 2 ){
  print("get notes")
  notes <- paste(file_data[first.index:last.index])
  print(notes)
}



# read file again to get tables -------------------------------------------


first.index <- which( file_data == "[IntTimes]" ) + 1
file_data2  <- read.table(file = hrms_files[1],
                          fill = TRUE,
                          skip = first.index,
                          strip.white = FALSE,
                          stringsAsFactors = FALSE,
                          blank.lines.skip = FALSE,
                          fileEncoding = "ISO8859-7")

get_atable <- function (file_data2,from,to) {
    first.index <- which(file_data2 == from)
    last.index  <- which(file_data2 == to) - 2
    span        <- last.index - first.index

    if ( span >= 2 ){
        print(from)
        file_data2[first.index:last.index,]
    }
}


get_alist <- function (file_data,from,to) {
  first.index <- which(file_data==from)
  last.index  <- which(file_data==to)-1
  span        <- last.index-first.index

    if ( span >= 2 ){
      print(from)
      file_data[first.index:last.index]
    }
  }



# get tables --------------------------------------------------------------

get_atable(file_data2,"[IntTimes]","[IntNotes]")

get_atable(file_data2,"[Summary-123]","[Summary-TH]")

get_atable(file_data2,"[Summary-TH]","[HRZones]")



# get lists ---------------------------------------------------------------

get_alist(file_data,"[Trip]","[HRData]")




# get HR data -------------------------------------------------------------

first.index <- which(file_data2=="[HRData]")
last.index  <- dim(file_data2)[1]
HRdata      <- file_data2[first.index:last.index,]
isna        <- apply(HRdata,2,function(x) {all(is.na(x))})


# remove empty columns
HRdata <- Filter( function(x) ! all( is.na(x) ), HRdata )



