

ng_gas_transmission_readr <- function(url){

  df <- readr::read_csv(url,
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
#' ng_gas_transmission_flows_all_zones(query_type = "LastUpdate")
#' ng_gas_transmission_flows_all_zones(query_type = "Last24Hours")
#' ng_gas_transmission_flows_all_zones(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_all_zones <- function(query_type = c("LastUpdate", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      url <- glue::glue("https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate={date_from}&inclusiveEndDate={date_to}&UserDefinedDownloadTimeQueryType=CustomDates&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=562&SelectedLocationClassificationIds=563&SelectedLocationClassificationIds=564&SelectedLocationClassificationIds=572&SelectedLocationClassificationIds=570&SelectedLocationClassificationIds=539&SelectedLocationClassificationIds=559&SelectedLocationClassificationIds=549&SelectedLocationClassificationIds=575&SelectedLocationClassificationIds=579&SelectedLocationClassificationIds=582&SelectedLocationClassificationIds=560&SelectedLocationClassificationIds=563&SelectedLocationClassificationIds=576&SelectedLocationClassificationIds=578&SelectedLocationClassificationIds=573&SelectedLocationClassificationIds=544&SelectedLocationClassificationIds=571&SelectedLocationClassificationIds=568&SelectedLocationClassificationIds=589&SelectedLocationClassificationIds=541&SelectedLocationClassificationIds=540&SelectedLocationClassificationIds=577&SelectedLocationClassificationIds=542&SelectedLocationClassificationIds=561&SelectedLocationClassificationIds=543&UserDefinedDownloadRadioSelection=Zones", .na = "")

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }

  } else if(query_type[1] == "Last24Hours"){
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=Last24Hours&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=562&SelectedLocationClassificationIds=563&SelectedLocationClassificationIds=564&SelectedLocationClassificationIds=572&SelectedLocationClassificationIds=570&SelectedLocationClassificationIds=539&SelectedLocationClassificationIds=559&SelectedLocationClassificationIds=549&SelectedLocationClassificationIds=575&SelectedLocationClassificationIds=579&SelectedLocationClassificationIds=582&SelectedLocationClassificationIds=560&SelectedLocationClassificationIds=563&SelectedLocationClassificationIds=576&SelectedLocationClassificationIds=578&SelectedLocationClassificationIds=573&SelectedLocationClassificationIds=544&SelectedLocationClassificationIds=571&SelectedLocationClassificationIds=568&SelectedLocationClassificationIds=589&SelectedLocationClassificationIds=541&SelectedLocationClassificationIds=540&SelectedLocationClassificationIds=577&SelectedLocationClassificationIds=542&SelectedLocationClassificationIds=561&SelectedLocationClassificationIds=543&UserDefinedDownloadRadioSelection=Zones"
  } else if(query_type[1] == "LastUpdate") {
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=LastUpdate&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=562&SelectedLocationClassificationIds=563&SelectedLocationClassificationIds=564&SelectedLocationClassificationIds=572&SelectedLocationClassificationIds=570&SelectedLocationClassificationIds=539&SelectedLocationClassificationIds=559&SelectedLocationClassificationIds=549&SelectedLocationClassificationIds=575&SelectedLocationClassificationIds=579&SelectedLocationClassificationIds=582&SelectedLocationClassificationIds=560&SelectedLocationClassificationIds=563&SelectedLocationClassificationIds=576&SelectedLocationClassificationIds=578&SelectedLocationClassificationIds=573&SelectedLocationClassificationIds=544&SelectedLocationClassificationIds=571&SelectedLocationClassificationIds=568&SelectedLocationClassificationIds=589&SelectedLocationClassificationIds=541&SelectedLocationClassificationIds=540&SelectedLocationClassificationIds=577&SelectedLocationClassificationIds=542&SelectedLocationClassificationIds=561&SelectedLocationClassificationIds=543&UserDefinedDownloadRadioSelection=Zones"

  } else {
    stop("You didn't give a valid query_type.")
  }

  df <- ng_gas_transmission_readr(url)
  df$download_selection <- "all zones"

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
#' ng_gas_transmission_flows_all_terminals(query_type = "LastUpdate")
#' ng_gas_transmission_flows_all_terminals(query_type = "Last24Hours")
#' ng_gas_transmission_flows_all_terminals(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_all_terminals <- function(query_type = c("LastUpdate", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      url <- glue::glue("https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate={date_from}&inclusiveEndDate={date_to}&UserDefinedDownloadTimeQueryType=CustomDates&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=546&SelectedLocationClassificationIds=580&SelectedLocationClassificationIds=551&SelectedLocationClassificationIds=581&SelectedLocationClassificationIds=552&SelectedLocationClassificationIds=553&SelectedLocationClassificationIds=557&SelectedLocationClassificationIds=569&SelectedLocationClassificationIds=554&SelectedLocationClassificationIds=555&SelectedLocationClassificationIds=556&UserDefinedDownloadRadioSelection=Terminals", .na = "")

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }

  } else if(query_type[1] == "Last24Hours"){
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=Last24Hours&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=546&SelectedLocationClassificationIds=580&SelectedLocationClassificationIds=551&SelectedLocationClassificationIds=581&SelectedLocationClassificationIds=552&SelectedLocationClassificationIds=553&SelectedLocationClassificationIds=557&SelectedLocationClassificationIds=569&SelectedLocationClassificationIds=554&SelectedLocationClassificationIds=555&SelectedLocationClassificationIds=556&UserDefinedDownloadRadioSelection=Terminals"
  } else if(query_type[1] == "LastUpdate") {
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=LastUpdate&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=546&SelectedLocationClassificationIds=580&SelectedLocationClassificationIds=551&SelectedLocationClassificationIds=581&SelectedLocationClassificationIds=552&SelectedLocationClassificationIds=553&SelectedLocationClassificationIds=557&SelectedLocationClassificationIds=569&SelectedLocationClassificationIds=554&SelectedLocationClassificationIds=555&SelectedLocationClassificationIds=556&UserDefinedDownloadRadioSelection=Terminals"

  } else {
    stop("You didn't give a valid query_type.")
  }

  df <- ng_gas_transmission_readr(url)
  df$download_selection <- "all terminals"

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
#' ng_gas_transmission_flows_all_demand_data_items(query_type = "LastUpdate")
#' ng_gas_transmission_flows_all_demand_data_items(query_type = "Last24Hours")
#' ng_gas_transmission_flows_all_demand_data_items(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_all_demand_data_items <- function(query_type = c("LastUpdate", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){

  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      url <- glue::glue("https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate={date_from}&inclusiveEndDate={date_to}&UserDefinedDownloadTimeQueryType=CustomDates&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=585&SelectedLocationClassificationIds=586&SelectedLocationClassificationIds=583&SelectedLocationClassificationIds=584&SelectedLocationClassificationIds=587&UserDefinedDownloadRadioSelection=Demand", .na = "")

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }

  } else if(query_type[1] == "Last24Hours"){
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=Last24Hours&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=585&SelectedLocationClassificationIds=586&SelectedLocationClassificationIds=583&SelectedLocationClassificationIds=584&SelectedLocationClassificationIds=587&UserDefinedDownloadRadioSelection=Demand"
  } else if(query_type[1] == "LastUpdate") {
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=LastUpdate&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=585&SelectedLocationClassificationIds=586&SelectedLocationClassificationIds=583&SelectedLocationClassificationIds=584&SelectedLocationClassificationIds=587&UserDefinedDownloadRadioSelection=Demand"

  } else {
    stop("You didn't give a valid query_type.")
  }

  df <- ng_gas_transmission_readr(url)
  df$download_selection <- "all demand data items"

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
#' ng_gas_transmission_flows_all_demand_data_items(query_type = "LastUpdate")
#' ng_gas_transmission_flows_all_demand_data_items(query_type = "Last24Hours")
#' ng_gas_transmission_flows_all_demand_data_items(query_type = "CustomDates", date_from = Sys.Date()-1, date_to = Sys.Date())
#'
ng_gas_transmission_flows_agg_supply_demand <- function(query_type = c("LastUpdate", "Last24Hours", "CustomDates"), date_from = NA, date_to = NA){


  if(query_type[1] == "CustomDates"){

    if(!is.na(date_from)[1] & !is.na(date_to)[1]){
      url <- glue::glue("https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate={date_from}&inclusiveEndDate={date_to}&UserDefinedDownloadTimeQueryType=CustomDates&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=558&SelectedLocationClassificationIds=588&UserDefinedDownloadRadioSelection=None", .na = "")

    } else {
      stop("When using query_type = 'CustomDates', the you must supply date_from and date_to params.")
    }

  } else if(query_type[1] == "Last24Hours"){
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=Last24Hours&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=558&SelectedLocationClassificationIds=588&UserDefinedDownloadRadioSelection=None"
  } else if(query_type[1] == "LastUpdate") {
    url <- "https://mip-prd-web.azurewebsites.net/UserDefinedFileDownload/DownloadFile?inclusiveStartDate=&inclusiveEndDate=&UserDefinedDownloadTimeQueryType=LastUpdate&inclusiveStartDate=&inclusiveEndDate=&LatestPublishedOriginallyPublished=LatestPublished&SelectedLocationClassificationIds=558&SelectedLocationClassificationIds=588&UserDefinedDownloadRadioSelection=None"

  } else {
    stop("You didn't give a valid query_type.")
  }

  df <- ng_gas_transmission_readr(url)
  df$download_selection <- "agg supply demand"

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

