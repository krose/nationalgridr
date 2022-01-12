



#' @importFrom ukgasapi dataItemExplorer
#' @export
ukgasapi::dataItemExplorer



#' Get the data dictionary for the data item explorer.
#'
#' @export
#'
#' @examples
#'
#' ng_data_item_explorer_dictionary()
#'
#'
ng_data_item_explorer_dictionary <- function(){

  readr::read_csv2(system.file("data-dictionary/API_List_v2.0_0.csv", package = "nationalgridr"), col_types = "cccccccc")
}


