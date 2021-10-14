generate_state_tab <- function(state, code) {
  tabPanel(state,
           fluidRow(
             column(width = 1),
             column(width = 10,
                    h2("Daily number of new COVID-19 cases, fatalities and recovered in", state),
                    p(paste0("This figure provides the number of COVID-19 new cases (yellow),
         fatalities (red), and recovered cases (green) in ", state, ". You can
         hover your cursor over the bar to see the exact numerical counts.")
                    ),
         plotlyOutput(paste0(code, "_daily_barplot"), height = "600px"),
         hr(),
         h3('Time-varying R'),
         h4("Effective basic reproduction number"),
         plotlyOutput(paste0(code, "_r_plot"), height = "600px"),
         hr()
             ),
         column(width = 1)
           )
  )
}
