
#' Create a new data frame by aggregate data by a time span
#'
#' @param data Polar exercise data frame to aggregate
#' @param by Time span to use. One of "years", "months", "weeks"
#'
#' @return A new data frame with new time resolution
#' @export
#'
sum_df_by <- function(data, by = "years") {
    require(lubridate)
    sum.df <- data.frame(date = as.POSIXct(0,origin = "1900-01-01"),
                         s_exercises = numeric(1),
                         s_distance        = numeric(1),
                         s_distance_old    = numeric(1),
                         s_total_time      = numeric(1),
                         s_descent         = numeric(1),
                         s_ascent          = numeric(1),
                         s_original_ascen  = numeric(1),
                         s_tl_exertion     = numeric(1),
                         s_energy_cons     = numeric(1),
                         s_original_energy = numeric(1),
                         a_weight          = numeric(1),
                         a_HRmaxmax        = numeric(1),
                         a_fat  = numeric(1),
                         a_HRavrg  = numeric(1),
                         a_HRmax  = numeric(1))
    sum.df <- sum.df[-1,]

    first_day = min(data$date)
    last_day  = max(data$date)

    # create bins
    if (by == "years") {
        bins = seq( from = as.POSIXct( paste0(year(first_day),    "-01-01") ),
                    to   = as.POSIXct( paste0(year(last_day) + 1, "-01-01") ),
                    by   = "year" )
    }

    if (by == "weeks") {
        all_mondays = unique( floor_date( x = data$date, unit="week" ) + 24*3600 )
        bins        = c( all_mondays, all_mondays[length(all_mondays)] + 7 * 24 * 3600 )
    }

    if (by=="months") {
        firstmonth = as.POSIXct( format(data$date[1],format="%Y-%m-01") )
        lastmonth  = as.POSIXct( seq.Date(as.Date(format(Sys.Date(),format="%Y-%m-01")),
                                          by = "month", length.out=2)[2] )
        bins       = seq( from = firstmonth, to = lastmonth, by = "month")
    }


    for (dd in 1:length(bins)){
        # bined data to process
        dummy = data[ data$date >= bins[dd] & data$date < bins[ dd + 1 ], ]

        # get sums
        bin_date          = bins[dd]
        s_exercises       = dim(dummy)[1]
        s_distance        = sum(as.numeric(dummy$dist_prod),       na.rm = TRUE )
        s_distance_old    = sum(as.numeric(dummy$distanse_old),    na.rm = TRUE )
        s_total_time      = sum(as.numeric(dummy$total_time),      na.rm = TRUE )
        s_descent         = sum(as.numeric(dummy$descent),         na.rm = TRUE )
        s_ascent          = sum(as.numeric(dummy$ascent),          na.rm = TRUE )
        s_original_ascen  = sum(as.numeric(dummy$original_ascen),  na.rm = TRUE )
        s_tl_exertion     = sum(as.numeric(dummy$tl_exertion),     na.rm = TRUE )
        s_energy_cons     = sum(as.numeric(dummy$energy_cons),     na.rm = TRUE )
        s_original_energy = sum(as.numeric(dummy$original_energy), na.rm = TRUE )

        a_weight  = sum( as.numeric(dummy$weigth), na.rm = TRUE ) /
                         sum( as.numeric(dummy$weigth) != 0 , na.rm=TRUE)
        a_HRmaxmax = sum(as.numeric(dummy$pl_HRmaxp),na.rm=TRUE)/sum(as.numeric(dummy$pl_HRmaxp)!=0,na.rm=TRUE)
        a_fat  = sum(as.numeric(dummy$fat_perc),na.rm=TRUE)/sum(as.numeric(dummy$fat_perc)!=0,na.rm=TRUE)
        a_HRavrg  = sum(as.numeric(dummy$HR_average),na.rm=TRUE)/sum(as.numeric(dummy$HR_average)!=0,na.rm=TRUE)
        a_HRmax  = sum(as.numeric(dummy$HR_max),na.rm=TRUE)/sum(as.numeric(dummy$HR_max)!=0,na.rm=TRUE)


        dummmydf=data.frame(bin_date ,
                            s_exercises ,
                            s_distance ,
                            s_distance_old ,
                            s_total_time ,
                            s_descent ,
                            s_ascent ,
                            s_original_ascen ,
                            s_tl_exertion ,
                            s_energy_cons ,
                            s_original_energy,
                            a_weight,
                            a_HRmaxmax,
                            a_fat,
                            a_HRavrg,
                            a_HRmax)

        #print(dummmydf)
        sum.df <- rbind(sum.df,dummmydf)

    }


    #print(sum.df)

    if (  sum.df$s_exercises[dim(sum.df)[1]]==0 & sum.df$bin_date[dim(sum.df)[1]] >= as.POSIXct(Sys.Date()) ) {
        sum.df <- sum.df[-dim(sum.df)[1],]
    }

    return(sum.df)
}
