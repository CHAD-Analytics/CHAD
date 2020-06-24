empty_plot <- function(title = NULL){
  p <- plotly_empty(type = "scatter", mode = "markers") %>%
    config(
      displayModeBar = FALSE
    ) %>%
    layout(
      title = title
    )
  return(p)
}
