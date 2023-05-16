#' @importFrom magrittr %>%
NULL
#' read EnergyPlus simulation eplusout.csv
#'
#' @param result.csv.dir folder containing the result csv, usually eplusout.csv
#' @param f filename, usually eplusout.csv
#' @return a data frame containing emission.exfiltration, emission.exhaust,
#'     emission.ref, emission.rej, emission.surf, emission.overall corresponding
#'     to five AH component and the overall AH. Also containing energy.elec,
#'     energy.overall for energy consumption. The unit of AH and energy columns
#'     are J
read.eplusout <- function(result.csv.dir, f) {
    df = readr::read_csv(sprintf("%s/%s", result.csv.dir, f), col_types = readr::cols()) %>%
        dplyr::mutate(emission.exfiltration = `Environment:Site Total Zone Exfiltration Heat Loss [J](Hourly)`,
                      emission.exhaust = `Environment:Site Total Zone Exhaust Air Heat Loss [J](Hourly)`,
                      emission.ref = `SimHVAC:Air System Relief Air Total Heat Loss Energy [J](Hourly)`,
                      emission.rej = `SimHVAC:HVAC System Total Heat Rejection Energy [J](Hourly)`,
                      emission.surf = `Environment:Site Total Surface Heat Emission to Air [J](Hourly)`,
                      emission.overall = emission.exfiltration + emission.exhaust + emission.ref + emission.rej + emission.surf) %>%
        dplyr::mutate(energy.elec = `Electricity:Facility [J](Hourly)`) %>%
        dplyr::mutate(energy.overall = energy.elec) %>%
        {.}
    if ("NaturalGas:Facility [J](Hourly)" %in% names(df)) {
      df <- df %>%
          dplyr::mutate(energy.gas = `NaturalGas:Facility [J](Hourly)`) %>%
          dplyr::mutate(energy.overall = energy.elec + energy.gas)
    }
    if (nrow(df) != 8760) {
        print(sprintf("%s: %d", f, nrow(df)))
    }
    df %>%
        dplyr::select(`Date/Time`, starts_with("emission."), starts_with("energy")) %>%
        {.}
}

#' Convert timestamp
#'
#' @param df a data frame the simulation csv result eplusout.csv is read into using read.eplusout
#' @param year year of the simulation result. EnergyPlus output doesn't have year in the timestamp
#' @return a data frame with `Date/Time` converted to POSIXct
convert.timestamp <- function(df, year) {
    df %>%
        tidyr::separate(`Date/Time`, into = c("day", "hour"), sep="  ", remove = FALSE) %>%
        dplyr::mutate(hour = as.integer(gsub(":00:00", "", hour)) - 1) %>%
        dplyr::mutate(`Date/Time` = as.POSIXct(sprintf("%04d/%s %02d:00:00", year, day, hour), format="%Y/%m/%d %H:%M:%S", tz = "GMT")) %>%
        dplyr::select(-day, -hour) %>%
        {.}
}

#' Convert timestamp
#'
#' @param df eplusout.csv directly
#' @param year year of the simulation result. EnergyPlus output doesn't have year in the timestamp
#' @return a data frame with `Date/Time` converted to POSIXct
convert.timestamp.eplusout <- function(df, target.year) {
    df %>%
        dplyr::mutate(hour = hour - 1) %>%
        dplyr::mutate(year = target.year) %>%
        dplyr::mutate(`Date/Time` = as.POSIXct(sprintf("%04d/%02d/%02d %02d:00:00", year, month, day, hour), format="%Y/%m/%d %H:%M:%S", tz = "GMT")) %>%
        dplyr::select(-(year:minute)) %>%
        dplyr::select(`Date/Time`, everything()) %>%
        {.}
}
