
columnnames  = c("date",
                "exercise_nbr",
                "resting_HR",
                "orth_test_HR",
                "weigth",
                "sleep_hour",

                # "row 2" #
                "sleep_pat",

                # "row 3" #
                "day_idata",
                # reserved
                "pl_HRmaxp",
                "ovrtrain_tst",
                "fat_perc",
                # reserved

                # "row 4" #
                # reserved
                # reserved
                "pl_ownindex",
                "weather",
                "temperature",
                # reserved

                # "row 5" #
                # reserved   = day_info[7,1]
                # reserved   = day_info[7,2]
                "plans_nbr",
                # reserved   = day_info[7,4]
                # reserved   = day_info[7,5]
                # reserved   = day_info[7,6])

                "no_report",
                "no_manual",
                "dist_prod",
                "start_time",
                "total_time",

                # "row 2" #
                "sport_id",
                "distanse_old",
                "feeling",
                "recovery",
                # reserved     = day_info[ex+3,5]
                "energy_cons",

                # "row 3" #
                "distance",
                # reserved     = day_info[ex+4,2]
                # reserved     = day_info[ex+4,3]
                # reserved     = day_info[ex+4,4]
                "odometer",
                "ascent",

                # "row 4" #
                "tl_exertion",
                "pw_avg_wt_ze",
                "ver_speed_up",
                "ver_speed_dn",
                # reserved     = day_info[ex+5,5]
                "ver_spd_up_avg",

                # "row 5" #
                "zone_0_time",
                "zone_1_time",
                "zone_2_time",
                "zone_3_time",
                "zone_4_time",
                "zone_5_time",

                # "row 6" #
                "zone_6_time",
                "zone_7_time",
                "zone_8_time",
                "zone_9_time",
                "sport_unit",
                # reserved     = day_info[ex+7,6]

                # "row 7" #
                "zone_0_exert",
                "zone_1_exert",
                "zone_2_exert",
                "zone_3_exert",
                "zone_4_exert",
                "zone_5_exert",

                # "row 8" #
                "zone_6_exert",
                "zone_7_exert",
                "zone_8_exert",
                "zone_9_exert",
                "record_rate",
                "original_ascen",

                # "row 9" #
                "HR_average",
                "HR_max",
                "speed_averg",
                "speed_max",
                "cadence_avrg",
                "cadence_max",

                # "row 10" #
                "alti_averg",
                "alti_max",
                "power_averg",
                "power_max",
                "pedal_indx_avr",
                "pedal_indx_max",

                # "row 11" #
                # reserved     = day_info[ex+12,1]
                # reserved     = day_info[ex+12,2]
                # reserved     = day_info[ex+12,3]
                # reserved     = day_info[ex+12,4]
                "slope_count",
                "descent",

                # "row 12" #
                "avg_calory_rat",
                "ver_spd_dn_avg",
                "beat_sum",
                "L_R_balan_avg",
                "L_R_balan_max",
                "original_energy",

                # "row 13" #
                "power_zone_0_ti",
                "power_zone_1_ti",
                "power_zone_2_ti",
                "power_zone_3_ti",
                "power_zone_4_ti",
                "power_zone_5_ti",

                # "row 14" #
                "power_zone_6_ti",
                "power_zone_7_ti",
                "power_zone_8_ti",
                "power_zone_9_ti",
                # reserved     = day_info[ex+14,5]
                # reserved     = day_info[ex+14,6]

                # "row 15" #
                "ascent_hourly",
                "exerc_rank",
                "memor_full",
                "running_index",
                # reserved     = day_info[ex+15,5]
                "incline_max",

                # "row 16" #
                "stride_len_avg",
                "decline_max",
                "cycle_efficienc",
                "footpod_calibr",
                "wheel_size",
                # reserved     = day_info[ex+16,6]

                # "row 17" #
                "exercise_type",
                # reserved     = day_info[ex+17,2]
                # reserved     = day_info[ex+17,3]
                # reserved     = day_info[ex+17,4]
                # reserved     = day_info[ex+17,5]
                # reserved     = day_info[ex+17,6]

                # "row 18-24" #

                # "row 25-34" #
                "exercise_name",
                "exercise_notes",
                "hrm_filename",
                "hyperlink",
                "hyperlink_info",
                "gps_file",
                "RR_file",
                "previus_file",
                "next_file"
)


#' Read exercise data from pdd files.
#'
#' @description It will get info from pdd file for all exercises of this day.
#'
#' @param pddfile File path to pdd file to read
#'
#' @return A data frame with the all exercises. One row for each.
#' @export
#'
read_pdd_file <- function(pddfile) {

    # read file ---------------------------------------------------------------
    day_info <- read.table( pddfile,
                            fill             = TRUE,
                            strip.white      = FALSE,
                            stringsAsFactors = FALSE,
                            blank.lines.skip = FALSE,
                            fileEncoding     = "ISO8859-7")


    # read day info -----------------------------------------------------------

    # "row 1" #
    date         = day_info[3,1]                    # yyyymmdd
    exercise_nbr = as.numeric(day_info[3,2])
    resting_HR   = day_info[3,3]                    # bpm
    orth_test_HR = day_info[3,4]                    # bpm
    weigth       = as.numeric(day_info[3,5])/100    # kg
    sleep_hour   = day_info[3,6]                    # sec

    # "row 2" #
    sleep_pat    = day_info[4,1]                    # categorical

    # "row 3" #
    day_idata    = day_info[5,1]                    # categorical
    # reserved   = day_info[5,2]
    pl_HRmaxp    = day_info[5,3]                    # bpm
    ovrtrain_tst = day_info[5,4]                    # state * 10000 + index * 100
    fat_perc     = as.numeric(day_info[5,5])/10     # fat%
    # reserved   = day_info[5,6]                    # user value

    # "row 4" #
    # reserved   = day_info[6,1]                    # user value
    # reserved   = day_info[6,2]
    pl_ownindex  = day_info[6,3]                    # index
    weather      = day_info[6,4]                    # categorical
    temperature  = as.numeric(day_info[6,5])/10     # celsius
    # reserved   = day_info[6,6]

    # "row 5" #
    # reserved   = day_info[7,1]
    # reserved   = day_info[7,2]
    plans_nbr    = day_info[7,3]
    # reserved   = day_info[7,4]
    # reserved   = day_info[7,5]
    # reserved   = day_info[7,6]

    day_vector   = c(date,
                     exercise_nbr,
                     resting_HR,
                     orth_test_HR,
                     weigth,
                     sleep_hour,

                     # "row 2" #
                     sleep_pat,

                     # "row 3" #
                     day_idata,
                     # reserved
                     pl_HRmaxp,
                     ovrtrain_tst,
                     fat_perc,
                     # reserved

                     # "row 4" #
                     # reserved
                     # reserved
                     pl_ownindex,
                     weather,
                     temperature,
                     # reserved

                     # "row 5" #
                     # reserved   = day_info[7,1]
                     # reserved   = day_info[7,2]
                     plans_nbr
                     # reserved   = day_info[7,4]
                     # reserved   = day_info[7,5]
                     # reserved   = day_info[7,6]
    )



    # loop all exercises in a day ---------------------------------------------

    # find rows for all exercise info
    exer_indx = which(apply(day_info, 1, function(x) any(grepl("ExerciseInfo", x))))
    gather = data.frame(matrix(NA, nrow = 1, ncol = 106))
    # gather = data.frame()

    for (ex in exer_indx){

        # read infos for an exercise ----------------------------------------------

        # "row 1" #
        # reserved      = day_info[ex + 2, 1]
        no_report       = day_info[ex + 2, 2]                 # categorical
        no_manual       = day_info[ex + 2, 3]                 # categorical
        dist_prod       = day_info[ex + 2, 4]                 # meters
        start_time      = day_info[ex + 2, 5]                 # seconds from midnight
        total_time      = day_info[ex + 2, 6]                 # seconds

        # "row 2" #
        sport_id        = day_info[ex + 3, 1]                 # categorical
        distanse_old    = as.numeric(day_info[ex + 3, 2])/10  # km (dropped var)
        feeling         = day_info[ex + 3, 3]                 # categorical
        recovery        = day_info[ex + 3, 4]                 # categorical
        # reserved      = day_info[ex + 3, 5]
        energy_cons     = day_info[ex + 3, 6]                 # kcal

        # "row 3" #
        distance        = day_info[ex + 4, 1]                 # meters
        # reserved      = day_info[ex + 4, 2]
        # reserved      = day_info[ex + 4, 3]
        # reserved      = day_info[ex + 4, 4]
        odometer        = day_info[ex + 4, 5]                 # km
        ascent          = day_info[ex + 4, 6]                 # meters

        # "row 4" #
        tl_exertion     = day_info[ex + 5, 1]                 # count
        pw_avg_wt_ze    = day_info[ex + 5, 2]                 # watts
        ver_speed_up    = day_info[ex + 5, 3]                 # ft/min
        ver_speed_dn    = day_info[ex + 5, 4]                 # ft/min
        # reserved      = day_info[ex + 5, 5]
        ver_spd_up_avg  = day_info[ex + 5, 6]                 # ft/min

        # "row 5" #
        zone_0_time     = day_info[ex + 6, 1]                 # seconds
        zone_1_time     = day_info[ex + 6, 2]                 # seconds
        zone_2_time     = day_info[ex + 6, 3]                 # seconds
        zone_3_time     = day_info[ex + 6, 4]                 # seconds
        zone_4_time     = day_info[ex + 6, 5]                 # seconds
        zone_5_time     = day_info[ex + 6, 6]                 # seconds

        # "row 6" #
        zone_6_time     = day_info[ex + 7, 1]                 # seconds
        zone_7_time     = day_info[ex + 7, 2]                 # seconds
        zone_8_time     = day_info[ex + 7, 3]                 # seconds
        zone_9_time     = day_info[ex + 7, 4]                 # seconds
        sport_unit      = as.numeric(day_info[ex + 7, 5])/100
        # reserved      = day_info[ex + 7, 6]

        # "row 7" #
        zone_0_exert    = day_info[ex + 8, 1]                 # exertion count
        zone_1_exert    = day_info[ex + 8, 2]                 # exertion count
        zone_2_exert    = day_info[ex + 8, 3]                 # exertion count
        zone_3_exert    = day_info[ex + 8, 4]                 # exertion count
        zone_4_exert    = day_info[ex + 8, 5]                 # exertion count
        zone_5_exert    = day_info[ex + 8, 6]                 # exertion count

        # "row 8" #
        zone_6_exert    = day_info[ex + 9, 1]                 # exertion count
        zone_7_exert    = day_info[ex + 9, 2]                 # exertion count
        zone_8_exert    = day_info[ex + 9, 3]                 # exertion count
        zone_9_exert    = day_info[ex + 9, 4]                 # exertion count
        record_rate     = day_info[ex + 9, 5]                 # seconds
        original_ascen  = day_info[ex + 9, 6]                 # meter

        # "row 9" #
        HR_average      = day_info[ex + 10, 1]                # bmp
        HR_max          = day_info[ex + 10, 2]                # bmp
        speed_averg     = as.numeric(day_info[ex + 10, 3])/10 # km/h
        speed_max       = as.numeric(day_info[ex + 10, 4])/10 # km/h
        cadence_avrg    = day_info[ex + 10, 5]                # rpm
        cadence_max     = day_info[ex + 10, 6]                # rpm

        # "row 10" #
        alti_averg      = day_info[ex + 11, 1]                # meters
        alti_max        = day_info[ex + 11, 2]                # meters
        power_averg     = day_info[ex + 11, 3]                # watts
        power_max       = day_info[ex + 11, 4]                # watts
        pedal_indx_avr  = day_info[ex + 11, 5]                # %
        pedal_indx_max  = day_info[ex + 11, 6]                # %

        # "row 11" #
        # reserved      = day_info[ex + 12, 1]
        # reserved      = day_info[ex + 12, 2]
        # reserved      = day_info[ex + 12, 3]
        # reserved      = day_info[ex + 12, 4]
        slope_count     = day_info[ex + 12, 5]
        descent         = day_info[ex + 12, 6]

        # "row 12" #
        avg_calory_rat  = day_info[ex + 13, 1]                # kcal/h
        ver_spd_dn_avg  = day_info[ex + 13, 2]                # ft/min
        beat_sum        = day_info[ex + 13, 3]                # beats
        L_R_balan_avg   = day_info[ex + 13, 4]                # left
        L_R_balan_max   = day_info[ex + 13, 5]                # left
        original_energy = day_info[ex + 13, 6]                # kcal

        # "row 13" #
        power_zone_0_ti = day_info[ex + 14, 1]                # seconds
        power_zone_1_ti = day_info[ex + 14, 2]                # seconds
        power_zone_2_ti = day_info[ex + 14, 3]                # seconds
        power_zone_3_ti = day_info[ex + 14, 4]                # seconds
        power_zone_4_ti = day_info[ex + 14, 5]                # seconds
        power_zone_5_ti = day_info[ex + 14, 6]                # seconds

        # "row 14" #
        power_zone_6_ti = day_info[ex + 14, 1]                # seconds
        power_zone_7_ti = day_info[ex + 14, 2]                # seconds
        power_zone_8_ti = day_info[ex + 14, 3]                # seconds
        power_zone_9_ti = day_info[ex + 14, 4]                # seconds
        # reserved      = day_info[ex + 14, 5]
        # reserved      = day_info[ex + 14, 6]

        # "row 15" #
        ascent_hourly   = day_info[ex + 15, 1]                # m/s
        exerc_rank      = day_info[ex + 15, 2]                # categorical
        memor_full      = day_info[ex + 15, 3]                # logical
        running_index   = day_info[ex + 15, 4]                # index
        # reserved      = day_info[ex + 15, 5]
        incline_max     = as.numeric(day_info[ex + 15, 6])/10 # %

        # "row 16" #
        stride_len_avg  = day_info[ex + 16, 1]                # mm
        decline_max     = as.numeric(day_info[ex + 16, 2])/10 # %
        cycle_efficienc = day_info[ex + 16, 3]                # index
        footpod_calibr  = as.numeric(day_info[ex + 16, 4])/1000
        wheel_size      = day_info[ex + 16, 5]                # mm
        # reserved      = day_info[ex + 16, 6]

        # "row 17" #
        exercise_type   = day_info[ex + 17, 1]                # categorical
        # reserved      = day_info[ex + 17, 2]
        # reserved      = day_info[ex + 17, 3]
        # reserved      = day_info[ex + 17, 4]
        # reserved      = day_info[ex + 17, 5]
        # reserved      = day_info[ex + 17, 6]

        # "row 18-24" #

        # "row 25-34" #
        exercise_name   = paste(day_info[ex + 26, !is.na(day_info[ex + 26, ])], collapse = " ")
        exercise_notes  = paste(day_info[ex + 27, !is.na(day_info[ex + 27, ])], collapse = " ")
        hrm_filename    = paste(day_info[ex + 28, !is.na(day_info[ex + 28, ])], collapse = " ")
        hyperlink       = paste(day_info[ex + 29, !is.na(day_info[ex + 29, ])], collapse = " ")
        hyperlink_info  = paste(day_info[ex + 30, !is.na(day_info[ex + 30, ])], collapse = " ")
        gps_file        = paste(day_info[ex + 31, !is.na(day_info[ex + 31, ])], collapse = " ")
        RR_file         = paste(day_info[ex + 32, !is.na(day_info[ex + 32, ])], collapse = " ")
        previus_file    = paste(day_info[ex + 33, !is.na(day_info[ex + 33, ])], collapse = " ")
        next_file       = paste(day_info[ex + 34, !is.na(day_info[ex + 34, ])], collapse = " ")

        exercise_vector = c(no_report,
                            no_manual,
                            dist_prod,
                            start_time,
                            total_time,

                            # "row 2" #
                            sport_id,
                            distanse_old,
                            feeling,
                            recovery,
                            # reserved     = day_info[ex+3,5]
                            energy_cons,

                            # "row 3" #
                            distance,
                            # reserved     = day_info[ex+4,2]
                            # reserved     = day_info[ex+4,3]
                            # reserved     = day_info[ex+4,4]
                            odometer,
                            ascent,

                            # "row 4" #
                            tl_exertion,
                            pw_avg_wt_ze,
                            ver_speed_up,
                            ver_speed_dn,
                            # reserved     = day_info[ex+5,5]
                            ver_spd_up_avg,

                            # "row 5" #
                            zone_0_time,
                            zone_1_time,
                            zone_2_time,
                            zone_3_time,
                            zone_4_time,
                            zone_5_time,

                            # "row 6" #
                            zone_6_time,
                            zone_7_time,
                            zone_8_time,
                            zone_9_time,
                            sport_unit,
                            # reserved     = day_info[ex+7,6]

                            # "row 7" #
                            zone_0_exert,
                            zone_1_exert,
                            zone_2_exert,
                            zone_3_exert,
                            zone_4_exert,
                            zone_5_exert,

                            # "row 8" #
                            zone_6_exert,
                            zone_7_exert,
                            zone_8_exert,
                            zone_9_exert,
                            record_rate,
                            original_ascen,

                            # "row 9" #
                            HR_average,
                            HR_max,
                            speed_averg,
                            speed_max,
                            cadence_avrg,
                            cadence_max,

                            # "row 10" #
                            alti_averg,
                            alti_max,
                            power_averg,
                            power_max,
                            pedal_indx_avr,
                            pedal_indx_max,

                            # "row 11" #
                            # reserved     = day_info[ex+12,1]
                            # reserved     = day_info[ex+12,2]
                            # reserved     = day_info[ex+12,3]
                            # reserved     = day_info[ex+12,4]
                            slope_count,
                            descent,

                            # "row 12" #
                            avg_calory_rat,
                            ver_spd_dn_avg,
                            beat_sum,
                            L_R_balan_avg,
                            L_R_balan_max,
                            original_energy,

                            # "row 13" #
                            power_zone_0_ti,
                            power_zone_1_ti,
                            power_zone_2_ti,
                            power_zone_3_ti,
                            power_zone_4_ti,
                            power_zone_5_ti,

                            # "row 14" #
                            power_zone_6_ti,
                            power_zone_7_ti,
                            power_zone_8_ti,
                            power_zone_9_ti,
                            # reserved     = day_info[ex+14,5]
                            # reserved     = day_info[ex+14,6]

                            # "row 15" #
                            ascent_hourly,
                            exerc_rank,
                            memor_full,
                            running_index,
                            # reserved     = day_info[ex+15,5]
                            incline_max,

                            # "row 16" #
                            stride_len_avg,
                            decline_max,
                            cycle_efficienc,
                            footpod_calibr,
                            wheel_size,
                            # reserved     = day_info[ex+16,6]

                            # "row 17" #
                            exercise_type,
                            # reserved     = day_info[ex+17,2]
                            # reserved     = day_info[ex+17,3]
                            # reserved     = day_info[ex+17,4]
                            # reserved     = day_info[ex+17,5]
                            # reserved     = day_info[ex+17,6]

                            # "row 18-24" #

                            # "row 25-34" #
                            exercise_name,
                            exercise_notes,
                            hrm_filename,
                            hyperlink,
                            hyperlink_info,
                            gps_file,
                            RR_file,
                            previus_file,
                            next_file)


        # prepare line for df
        arecord = c(day_vector, exercise_vector)
        output  = c(arecord[1], as.numeric(arecord[2:97]), arecord[98:length(arecord)])
        gather  = rbind( gather, output)

    }
    ## display some process info
    # cat( c(pddfile, exercise_nbr, "\n"))
    ## apply column names
    colnames(gather) <- columnnames
    ## return data
    return(gather)
}

# pddfile = "/home/athan/Documents/Running/Polar/Athan/2016/20161220.pdd"
# pddfile = "/home/athan/Documents/Running/Polar/Athan_old/2012/20120816.pdd"

# gathered <- read_pdd_file( pddfile )

