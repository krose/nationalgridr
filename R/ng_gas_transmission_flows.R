
# https://data.nationalgas.com/reports/customisable-downloads




ng_gas_transmission_GET <- function(ids, query_type, date_from = NULL, date_to = NULL){

  if(query_type == "Last24Hours"){
    req <- httr::POST(url = "https://data.nationalgas.com/api/customisable-downloads-download",
                      body = paste0('{"ids":"', ids, '","fromDate":"', Sys.Date(), '","toDate":"', Sys.Date(), '","isLatest":true,"predefinedDate":"Last 24 Hours","type":"CSV"}'))
  } else if(query_type == "LastUpdate"){
    req <- httr::POST(url = "https://data.nationalgas.com/api/customisable-downloads-download",
                      body = paste0('{"ids":"', ids, '","fromDate":"', Sys.Date(), '","toDate":"', Sys.Date(), '","isLatest":true,"predefinedDate":"Latest Update","type":"CSV"}'))
  } else if(query_type == "LastHour"){
    req <- httr::POST(url = "https://data.nationalgas.com/api/customisable-downloads-download",
                      body = paste0('{"ids":"', ids, '","fromDate":"', Sys.Date(), '","toDate":"', Sys.Date(), '","isLatest":true,"predefinedDate":"Last Hour","type":"CSV"}'))
  } else if(query_type == "CustomDates"){
    req <- httr::POST(url = "https://data.nationalgas.com/api/customisable-downloads-download",
                      body = paste0('{"ids":"', ids, '","fromDate":"', date_from, '","toDate":"', date_to, '","isLatest":true,"type":"CSV"}'))
  } else {
    stop("qeury_type is not supported.")
  }

  if(httr::http_error(req)){
     httr::stop_for_status(req)
  }

  req_cont <- httr::content(req, as = "text")

  df <- ng_gas_transmission_readr(req_cont)

  df
}


ng_gas_transmission_readr <- function(req_cont){

  first_10_lines <- readr::read_lines(req_cont, n_max = 10)
  header_test <- which(stringr::str_detect(first_10_lines, "System Entry Name"))

  df <- readr::read_csv(req_cont,
                        skip = header_test - 1,
                        locale = readr::locale(tz = "GMT"),
                        col_types = "ccncccccc")

  names(df) <- c("system_entry_name", "published_time", "value", "timestamp",
                 "expired_y_n", "amended_y_n", "amended_timestamp", "substituted_y_n",
                 "late_received_y_n")

  df$published_time <- lubridate::with_tz(lubridate::dmy_hm(df$published_time, tz = "GMT"), tzone = "UTC")
  df$timestamp <- lubridate::with_tz(lubridate::dmy_hms(df$timestamp, tz = "GMT"), tzone = "UTC")
  df$amended_timestamp <- lubridate::with_tz(lubridate::dmy_hms(df$amended_timestamp, tz = "GMT"), tzone = "UTC")

  ng_storage_sites <- c("ALDBROUGH", "AVONMOUTH", "HILLTOP", "HOLE HOUSE FARM", "HOLFORD", "HORNSEA", "STUBLACH")

  df$is_storage_site <- df$system_entry_name %in% ng_storage_sites

  df
}

#' Nationalgrid gas download
#'
#' @param query_type The query to be used. Defaults to 'LastUpdate'
#' @param date_from Date. In combination with query_type='CustomDates'.
#' @param date_to Date. In combination with query_type='CustomDates'.
#'
#' @export
#'
#' @examples
#'
#' library(nationalgridr)
#'
#' ng_gas_transmission_flows_entry_point(query_type = "LastUpdate")
#' ng_gas_transmission_flows_entry_point(query_type = "Last24Hours")
#' ng_gas_transmission_flows_entry_point(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_entry_point <- function(query_type = c("LastUpdate", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  ids <- "562,564,572,570,539,559,549,575,579,582,560,563,576,578,573,544,571,568,589,541,540,577,542,561,543"

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      df <- ng_gas_transmission_GET(ids = ids, query_type = query_type, date_from = date_from, date_to = date_to)

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }
  } else if(query_type[1] == "Last24Hours"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastHour"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastUpdate") {
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else {
    stop("You didn't give a valid query_type.")
  }

  df$download_selection <- "supply by entry point"

  df <- ng_add_md5(df)

  df
}


#' Nationalgrid gas download
#'
#' @param query_type The query to be used. Defaults to 'LastUpdate'
#' @param date_from Date. In combination with query_type='CustomDates'.
#' @param date_to Date. In combination with query_type='CustomDates'.
#'
#' @export
#'
#' @examples
#'
#' library(nationalgridr)
#'
#' ng_gas_transmission_flows_terminals(query_type = "LastUpdate")
#' ng_gas_transmission_flows_terminals(query_type = "Last24Hours")
#' ng_gas_transmission_flows_terminals(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_terminals <- function(query_type = c("LastUpdate", "LastHour", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  ids <- "546,580,551,581,552,553,557,569,554,555,556"

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      df <- ng_gas_transmission_GET(ids = ids, query_type = query_type, date_from = date_from, date_to = date_to)

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }
  } else if(query_type[1] == "Last24Hours"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastHour"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastUpdate") {
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else {
    stop("You didn't give a valid query_type.")
  }

  df$download_selection <- "supply by terminals"

  df <- ng_add_md5(df)


  df
}



#' Nationalgrid gas download
#'
#' @param query_type The query to be used. Defaults to 'LastUpdate'
#' @param date_from Date. In combination with query_type='CustomDates'.
#' @param date_to Date. In combination with query_type='CustomDates'.
#'
#' @export
#'
#' @examples
#'
#' library(nationalgridr)
#'
#' ng_gas_transmission_flows_demand(query_type = "LastUpdate")
#' ng_gas_transmission_flows_demand(query_type = "Last24Hours")
#' ng_gas_transmission_flows_demand(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_demand <- function(query_type = c("LastUpdate", "LastHour", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  ids <- "601,602,585,586,583,603,584,587,588"

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      df <- ng_gas_transmission_GET(ids = ids, query_type = query_type, date_from = date_from, date_to = date_to)

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }
  } else if(query_type[1] == "Last24Hours"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastHour"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastUpdate") {
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else {
    stop("You didn't give a valid query_type.")
  }

  df$download_selection <- "demand data"

  df <- ng_add_md5(df)

  df
}


#' Nationalgrid gas download
#'
#' @param query_type The query to be used. Defaults to 'LastUpdate'
#' @param date_from Date. In combination with query_type='CustomDates'.
#' @param date_to Date. In combination with query_type='CustomDates'.
#'
#' @export
#'
#' @examples
#'
#' library(nationalgridr)
#'
#' ng_gas_transmission_flows_total_supply(query_type = "LastUpdate")
#' ng_gas_transmission_flows_total_supply(query_type = "Last24Hours")
#' ng_gas_transmission_flows_total_supply(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_total_supply <- function(query_type = c("LastUpdate", "LastHour", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  ids <- "558"

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      df <- ng_gas_transmission_GET(ids = ids, query_type = query_type, date_from = date_from, date_to = date_to)

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }
  } else if(query_type[1] == "Last24Hours"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastHour"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastUpdate") {
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else {
    stop("You didn't give a valid query_type.")
  }

  df$download_selection <- "total supply"

  df <- ng_add_md5(df)

  df
}

#' Nationalgrid gas download
#'
#' @param query_type The query to be used. Defaults to 'LastUpdate'
#' @param date_from Date. In combination with query_type='CustomDates'.
#' @param date_to Date. In combination with query_type='CustomDates'.
#'
#' @export
#'
#' @examples
#'
#' library(nationalgridr)
#'
#' ng_gas_transmission_flows_linepack(query_type = "LastUpdate")
#' ng_gas_transmission_flows_linepack(query_type = "Last24Hours")
#' ng_gas_transmission_flows_linepack(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_linepack <- function(query_type = c("LastUpdate", "LastHour", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  ids <- "591"

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      df <- ng_gas_transmission_GET(ids = ids, query_type = query_type, date_from = date_from, date_to = date_to)

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }
  } else if(query_type[1] == "Last24Hours"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastHour"){
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else if(query_type[1] == "LastUpdate") {
    df <- ng_gas_transmission_GET(ids = ids, query_type = query_type)
  } else {
    stop("You didn't give a valid query_type.")
  }

  df$download_selection <- "total actual linepack"

  df <- ng_add_md5(df)

  df
}

ng_add_md5 <- function(ng_df){

  md5 <- vector(mode = "character", length = length(ng_df[[1]]))

  for(i in seq_along(ng_df[[1]])){
    md5[i] <- digest::digest(ng_df[i, ], algo = "md5")
  }

  ng_df$id_md5 <- md5

  ng_df
}


#' Average data to hourly
#'
#' @param ng_df data.frame with ng_gas_transmission flow data.
#'
#' @export
#'
#' @importFrom dplyr %>%
#'
ng_gas_transmission_hourly_avg <- function(ng_df){

  ng_df <-
    ng_df %>%
    dplyr::mutate(timestamp = lubridate::floor_date(timestamp, unit = "hours")) %>%
    dplyr::group_by(system_entry_name, download_selection, is_storage_site, timestamp) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup()

  ng_df <- ng_add_md5(ng_df)

  ng_df
}

