observed <- tabPanel("National",
                     fluidRow(column(width = 12, top_matter)),
                     fluidRow(column(width = 1),
                              column(width = 10,
                                     h4("(Please wait a few seconds for the figures to load)"),
                                     h3("Snapshot"),
                                     gt_output("gt_india_snapshot"),
                                     h4(textOutput("latest")),
                                     h2(""),
                                     h2("Daily number of new COVID-19 cases, fatalities and recovered in India"),
                                     p("This figure provides the number of COVID-19 new cases (yellow),
        fatalities (red), and recovered cases (green) in India. You can
        hover your cursor over the bar to see the exact numerical counts."
                                     ),
        plotlyOutput("india_daily_barplot", height = "600px"),
        hr(),

        h2("Time-varying R"),
        plotlyOutput("india_r_plot", height = "600px"),

        hr(),
        h2("Daily number of COVID-19 vaccines in India"),
        p("This figure provides the daily number of COVID-19 vaccines (green)
        in India since February 15, 2021. You can
        hover your cursor over the bar to see the exact numerical counts."
        ),
        plotlyOutput("india_daily_vax", height = "600px"),
        hr(),
        h2("Daily number of COVID-19 cases and deaths"),
        p("The first figure represents COVID-19 case counts where the x-axis
        starts on the day when each country passed 100 cases. The second
        figure represents COVID-19 fatalities where the x-axis starts on the
        day when each country exceeded 3 fatalities. These axes allow
        comparison of counts at similar stages of the outbreak. You can click
        on countries in the legend to add or remove them and you can hover
        your cursor over the lines to see the exact numerical counts."
        ),
        plotlyOutput("case_death_country_comp", height = "800px"),
        # hr(),
        # h2("Cumulative case counts by state/union territory"),
        # p("The map displays the case counts by state/union territory in India
        # over the last few days.", "The darker areas of the map indicate a
        # greater number of cases."
        # ),
        # HTML(paste0("<center><img src=", img.file, "></center>")),
        hr(),
        h2("Cumulative COVID-19 case count by state/union territory"),
        plotOutput("case_facetplot", height = "600px"),
        downloadLink('downloadFacet_cases', 'Download'),
        h2("Cumulative COVID-19 death count by state/union territory"),
        plotOutput("death_facetplot", height = "600px"),
        downloadLink('downloadFacet_deaths', 'Download'),
        hr()
                              ),
        column(width = 1)
                     )
)
