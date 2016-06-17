predict_plot_controls <- function(type) {
  vars <- input[[paste0(type,"_evar")]]
  xvar <- paste0(type, "_xvar")
  frow <- paste0(type, "_facet_row")
  fcol <- paste0(type, "_facet_col")
  col <- paste0(type, "_color")
  vars_facet <- c("None" = ".", vars)
  vars_color <- c("None" = "none", vars)

  tagList(
    selectizeInput(xvar, "X-variable:", choices = vars,
      selected = state_multiple(xvar, vars),
      multiple = FALSE),
    selectizeInput(frow, "Facet row:", vars_facet,
      selected = state_single(frow, vars_facet, "."),
      multiple = FALSE),
    selectizeInput(fcol, "Facet column:", vars_facet,
      selected = state_single(fcol, vars_facet, "."),
      multiple = FALSE),
    selectizeInput(col, "Color:", vars_color,
      selected = state_single(col, vars_color, "none"),
      multiple = FALSE)
  )
}
