state_daily_barplot <- function(forecast, start.date = as.Date("2020-05-01")) {

  state_data <- merge.data.table(
    covid19india::get_state_counts(),
    covid19india::pop[, !c("population")][, .SD[1], by = place], by = "place"
    )[abbrev == forecast]

  state_data <- melt(state_data[, .(place, date, `New Cases` = daily_cases, Recovered = daily_recovered, Fatalities = daily_deaths)],
       id.vars = c("place", "date"),
       variable.name = "Type",
       value.name = "Count")[, `:=` (
         Type = factor(Type, levels = c("New Cases", "Fatalities", "Recovered")),
         date_fmt = as.factor(format(date, format = "%b %e")),
         count_fmt  = format(Count, big.mark = ",", scientific = FALSE, trim = TRUE)
         )][
           , text := paste0(date_fmt, ": ", count_fmt, " ", Type)
         ][]


  cap <- paste0("\uA9 COV-IND-19 Study Group. Last updated: ",
                format(as.Date(Sys.getenv("today")), format = "%b %e"), sep = ' ')

  title <- paste("Daily number of COVID-19 new cases, fatalities and",
                 "recovered cases in India since March 1")

  axis.title.font <- list(size = 16)
  tickfont        <- list(size = 16)

  xaxis <- list(title = "", titlefont = axis.title.font, showticklabels = TRUE,
                tickangle = -30, zeroline = F)

  yaxis <- list(title = "Daily counts", titlefont = axis.title.font,
                tickfont = tickfont, zeroline = T)
  colors <- c(
    "Fatalities" = "#ED553B",
    "New Cases"  = "#f2c82e",
    "Recovered"  = "#138808"
  )

  p <- plot_ly(state_data, x = ~date, y = ~Count, color = ~Type, text = ~text,
               type = "bar", colors = colors, hoverinfo = "text"
  ) %>%
    layout(barmode = "stack", xaxis = xaxis, yaxis = yaxis, title =
             list(text = cap, xanchor = "left", x = 0), legend =
             list(orientation = "h", font = list(size = 16))
    ) %>%
    plotly::config(toImageButtonOptions = list(width = NULL, height = NULL))

  p
}
