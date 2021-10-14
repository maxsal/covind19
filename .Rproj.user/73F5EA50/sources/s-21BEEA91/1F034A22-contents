suppressPackageStartupMessages({
  library(data.table)
  library(covid19india)
  library(janitor)
  library(EpiEstim)
})

tvr_plot <- function(forecast) {

  tickfont <- list(size = 16)
  set_seed <- 46342
  set.seed(set_seed)
  start_date = NULL

  # data ----------

  all_data <- covid19india::get_all_data()[, .(place, abbrev, date, r = r_est, lower = r_lower, upper = r_upper)]

  if (forecast == "India") {
    all_data <- all_data[place == "India"]
  } else {
    all_data <- all_data[abbrev == forecast]
  }

  plot_data <- all_data[
    , text := paste0("Date: ", format(date, format = '%b %d'), "<br>R: ",
                    format(round(r, 2), nsmall = 2), "<br>CI: ",
                    paste0("[", format(round(lower, 2), nsmall = 2), ", ",
                           format(round(upper, 2), nsmall = 2), "]"))
  ][]


  if (forecast == "India") {
    plot_data <- plot_data[date >= "2020-03-15"]
  } else {
    plot_data <- plot_data[date >= "2020-03-24"]
  }

  cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                format(as.Date(Sys.getenv("today")), format = "%b %e"), sep = ' ')
  axis_title_font <- list(size = 16)


  p <- plot_ly(plot_data, x = ~date, y = ~r, type = "scatter", mode = "lines",
               line = list(color = "rgb(54, 163, 11)", width = 5),
               hoverinfo = "text",
               text   = ~text) %>%
    add_markers(data = plot_data, x = ~date, y = ~r, mode = "marker",
                marker = list(color = "rgb(38, 38, 38)", symbol = 3)) %>%
    add_ribbons(ymin = ~lower,
                ymax = ~upper,
                line = list(color = 'rgba(54, 163, 11, 0.05)'),
                fillcolor = 'rgba(54, 163, 11, 0.2)',
                hoverinfo = "none") %>%
    layout(
      title = list(text = cap, xanchor = "left", x = 0),
      xaxis = list(title = "Date", titlefont = axis_title_font,
                   tickfont = tickfont, zeroline = T),
      yaxis = list(title = "R(t)", titlefont = axis_title_font,
                   tickfont = tickfont, zeroline = T),
      shapes = list(
        type = "line", xref = "paper", yref = "data",
        x0 = 0, x1 = 1, y0 = 1, y1 = 1,
        line = list(color = "rgba(255, 153, 51, 0.5)")
      ),
      showlegend = FALSE
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

  p

}
