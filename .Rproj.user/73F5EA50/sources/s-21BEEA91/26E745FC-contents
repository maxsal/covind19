library(shiny)
library(data.table)
library(covid19india)
library(tidyverse)
library(plotly)
library(gridExtra)
library(ggtext)
library(grid)
library(extrafont)
library(gt)

###
latest <- Sys.getenv("latest")
today  <- as.Date(Sys.getenv("today"))
###

###
f <- list.files("plot_functions")
for (i in seq_along(f)) {source(paste0("plot_functions/", f[i]))}
###

source("top_matter.R", local = TRUE)
source("references.R", local = TRUE)

# img.file <- paste0("https://github.com/umich-cphds/cov-ind-19-data/raw/",
#                    branch, "/", latest, "/day_sp_animation.gif")

source("observed.R", local = T)
source("state.R", local = T)
source("metrics.R", local = T)

metrics_table <- covid19india::get_metrics_tables(top20 = unique(covid19india::pop[place %in% covid19india::get_state_counts()[order(-total_cases)][, unique(place)][1:20]][, .SD[1], by = place][, abbrev]))$full_t20

state_abbrev <- sort(merge.data.table(
  covid19india::get_state_counts(mohfw = TRUE),
  covid19india::pop[, !c("population")][, .SD[1], by = place], by = "place"
)[order(-total_cases), unique(abbrev)][1:20])
state_names <- covid19india::pop[abbrev %in% state_abbrev, unique(place)]

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

  output$latest <- renderText(paste0("Data last updated ",
                                     format(latest, format = "%B %d")))

  output$india_daily_barplot     <- renderPlotly( india_daily_barplot() )
  output$india_daily_vax         <- renderPlotly( india_daily_vax() )
  output$case_death_country_comp <- renderPlotly( case_death_country_comp() )
  output$case_facetplot          <- renderPlot( top_case_death_facetplot()$case_facet )
  output$death_facetplot         <- renderPlot( top_case_death_facetplot()$death_facet )
  output$india_r_plot            <- renderPlotly( tvr_plot(forecast = "India") )

  for (i in seq_along(state_abbrev)) {
    output[[paste0(state_abbrev[i], "_daily_barplot")]] <- renderPlotly( state_daily_barplot(forecast = state_abbrev[i]) )
    output[[paste0(state_abbrev[i], "_r_plot")]] <- renderPlotly( tvr_plot(forecast = state_abbrev[i]) )
  }



  output$out <- renderUI({

    tabs <- purrr::map2(state_names, state_abbrev, generate_state_tab)

    eval(expr(navbarPage("COVID-19 Outbreak in India",
                         observed, navbarMenu("States", !!!tabs),
                         metrics, references)))

  })

  output$metrics_table = render_gt({
    metrics_table
  })

  output$gt_india_snapshot = render_gt({ snapshot() })

})
