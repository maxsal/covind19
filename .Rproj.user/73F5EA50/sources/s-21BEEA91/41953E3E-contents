suppressPackageStartupMessages({
  library(data.table)
  library(covid19india)
  library(ggplot2)
  library(scales)
})

top_case_death_facetplot <- function() {

  # subtitle
  subtitle <- paste0('\uA9 COV-IND-19 Study Group. Last updated ',
                     format(as.Date(Sys.getenv("today")), format = '%b %e'), ', 2020', sep = '')
  caption <- 'Source: Ministry of Health and Family Welfare'

  # Compute daily counts from cumulative sums
  daily = function(x) { c(x[1], diff(x)) }

  count_data <- get_state_counts()[order(date),
                                      `:=` (
                                        day = 1:.N,
                                        max_cases = max(total_cases),
                                        max_deaths = max(total_deaths),
                                        max_date = max(date)),
                                      by = place][]
  count_data <- merge.data.table(
    count_data,
    covid19india::pop[, !c("population")][, .SD[1], by = place],
    by = "place", all.x = TRUE
  )

  # sort data sets from max to min max cases, plot the first 20
  top_cases     <- count_data[order(-total_cases)][, unique(place)][1:20]
  top_case_data <- count_data[place %in% top_cases]

  top_deaths     <- count_data[order(-total_deaths)][, unique(place)][1:20]
  top_death_data <- count_data[place %in% top_deaths]

  # cases
  case_plot <- ggplot(data = top_case_data, aes(x = date, y = total_cases, group = place)) +
    geom_line(data = top_case_data, aes(x = date, y = total_cases, group = place), color = "gray", alpha = 0.6) +
    geom_line(color = "#36A30B", size = 1.5) +
    facet_wrap(~place) +
    geom_point(shape = 3, size = 0.5) +
    geom_point(data = top_case_data[, .(place, total_cases, abbrev, date)][, .SD[date == max(date)], by = place],
               aes(x = date, y = total_cases, group = place), size = 2) +
    geom_text(data = top_case_data[, .(place, total_cases, abbrev, date)][, .SD[date == max(date)], by = place],
              aes(x = date - 150, y = top_case_data[, max(total_cases)] - 500, group = place, label = paste(format(total_cases, big.mark = ","), ' total cases', sep = ' '))) +
    labs(
      x = "\nDate",
      y = "Cumulative number of daily cases\n",
      title = '',
      subtitle = subtitle,
      caption = caption
    ) +
    scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~ ., labels = scales::comma)) +
    theme_bw() +
      theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
            strip.background = element_blank(),
            panel.grid.minor.x = element_blank(),
            panel.border = element_blank(),
            axis.title.x = element_text(size = 14),
            axis.title.y = element_text(size = 14),
            axis.text.x = element_text(angle = 20))

  death_plot <- ggplot(data = top_death_data, aes(x = date, y = total_deaths, group = place)) +
    geom_line(data = top_death_data, aes(x = date, y = total_deaths, group = place), color = "gray", alpha = 0.6) +
    geom_line(color = "#36A30B", size = 1.5) +
    facet_wrap(~place) +
    geom_point(shape = 3, size = 0.5) +
    geom_point(data = top_death_data[, .(place, total_deaths, abbrev, date)][, .SD[date == max(date)], by = place],
               aes(x = date, y = total_deaths, group = place), size = 2) +
    geom_text(data = top_death_data[, .(place, total_deaths, abbrev, date)][, .SD[date == max(date)], by = place],
              aes(x = date - 150, y = top_death_data[, max(total_deaths)] - 500, group = place, label = paste(format(total_deaths, big.mark = ","), ' total deaths', sep = ' '))) +
    labs(
      x = "\nDate",
      y = "Cumulative number of daily deaths\n",
      title = '',
      subtitle = subtitle,
      caption = caption
    ) +
    scale_y_continuous(labels = scales::comma, sec.axis = sec_axis(~ ., labels = scales::comma)) +
    theme_bw() +
    theme(strip.text.x = element_text(size=12, face="bold", hjust = 0, color = '#36A30B'),
          strip.background = element_blank(),
          panel.grid.minor.x = element_blank(),
          panel.border = element_blank(),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14),
          axis.text.x = element_text(angle = 20))

  list(case_facet = case_plot, death_facet = death_plot)

}
