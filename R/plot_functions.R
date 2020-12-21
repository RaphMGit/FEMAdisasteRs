#' @title Map FEMA data to Stacked Bar
#'
#' @param df Required. Data.frame created by \code{\link{get_fema_data}} or
#' \code{\link{get_fema_data_all}}
#' @param x_var Optional Character string. Column name in \code{df} to be placed on x axis. state by default.
#' @param by Optional Character string. Column name in \code{df} to be used as fill.
#' incidentType by default.
#'
#' @return A stacked \code{\link{[ggplot2](geom_bar)}}
#'
#' @examples
#' \dontrun{
#' ## Assign entity to be used to get data
#' entity <- "DisasterDeclarationsSummaries"
#'
#' ## get data when fiscal year is greater than 1979
#' FEMAdf <- get_fema_data(entity, api_params = list(filter = 'fyDeclared gt "1979"'))
#'
#' ## plot data
#' map_fema_data(FEMAdf[[2]])
#' }
#' 
#' @seealso \code{\link{get_fema_data}} \code{\link{get_fema_data_all}} for getting fema data
#'
#' @export
map_fema_data <- function(df, x_var = "state", by = "incidentType") {
  if (!inherits(df, "data.frame")) {
    stop("df must be a dataframe")
  }
  if (!all(c(x_var, by) %in% names(df))) {
    stop("x_var or by column not found in dataset df")
  }
  .n_by <- length(unique(df[, by]))
  if (.n_by > 12) {
    morecolors <- c(
      RColorBrewer::brewer.pal(12, "Paired"),
      RColorBrewer::brewer.pal(.n_by - 12, "Set2")
    )
  } else {
    morecolors <- RColorBrewer::brewer.pal(.n_by, "Paired")
  }

  MapGG <- ggplot2::ggplot(data = df, ggplot2::aes_string(x = x_var)) +
    ggplot2::geom_bar(ggplot2::aes_string(fill = by),
      position =
        ggplot2::position_stack(), show.legend = TRUE
    ) +
    ggplot2::scale_fill_manual(values = morecolors) +
    ggplot2::theme_minimal() +
    ggplot2::theme(
      legend.position = "top", legend.title = ggplot2::element_blank(),
      legend.text = ggplot2::element_text(size = 8)
    )
  return(MapGG)
}
