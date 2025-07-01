#' Update Depths in a WEMo input
#'
#' Update the depth values in a object created by `prepare_wemo_inputs()`.
#' @param wemo_input `sf` object with column `depths` which is a list of vectors
#' @param depth_diff numeric. indicates the amount to increase (or decrease for negative values)
#'
#' @return `sf` object ready for processing in `wemo()`
#' @export
#'
update_depths <- function(wemo_input, depth_diff){
  wemo_input$depths <- lapply(wemo_input$depths, function(x) x + depth_diff)

  return(wemo_input)
}
