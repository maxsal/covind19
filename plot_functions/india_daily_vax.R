suppressPackageStartupMessages({
  library(vroom)
  library(tidyverse)
  library(ggtext)
  library(scales)
  library(tidyr)
  library(covid19india)
})

india_daily_vax <- function() {

  suppressWarnings({

  vax_dat <- get_state_vax()[place == "India"]
  setnames(vax_dat, old = "date", new = "Day")
  vax_india <- vax_dat[, text := paste0("India", "<br>", Day, ": ", format(daily_doses, big.mark = ","), " daily vaccines<br>")][Day >= as.Date("2021-03-15")][]

  india_color     <- c("India" = "#138808")
  vax.title       <- "Daily COVID-19 vaccines delivered in India"
  axis.title.font <- list(size = 16)

  vax.xaxis <- list(title = "Date",
                    titlefont = axis.title.font, showticklabels = TRUE,
                    tickangle = 0, showline = T, zeroline = F)

  vax.yaxis <- list(title = "Number of vaccines", titlefont =
                      axis.title.font, zeroline = F, showline = F)

  case_plot <- plot_ly(vax_india, x = ~ Day, y = ~ daily_doses, text = ~ text, color = I("#138808"),
                       hoverinfo = "text", type = "bar", hoverlabel = list(align = "left"),
                       showlegend = F
                       # line = list(width = 3)
  ) %>%
    layout(xaxis = vax.xaxis, yaxis = vax.yaxis,
           annotations = list(text = vax.title, xref = "paper", yref = "paper",
                              xanchor = "left", x = 0, y = 1.1, showarrow = F,
                              font = list(size = 22))
    )

  })

  suppressWarnings(print(case_plot))

}
