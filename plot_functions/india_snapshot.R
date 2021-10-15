# libraries ----------
library("tidyverse")
library("glue")
library("janitor")
library("lubridate")
library("gt")
library("covid19india")
library(data.table)

snapshot <- function() {
  # functions -----------
  get_snap <- function(t = NULL) {

    state <- get_state_counts(mohfw = TRUE)[, .(place, date, total_cases, total_recovered, total_deaths, total_active)]

    quick_fix <- function(x) {
      x1 <- x[date <= "2021-10-12"][, !c("total_active")]
      x2 <- x[date >= "2021-10-13"][, .(place, date, total_cases, total_recovered, total_deaths = total_active)][]
      x3 <- rbindlist(list(x1, x2), fill = TRUE)
      x3 <- x3[, lapply(.SD, sum, na.rm = TRUE), by = "date", .SDcols = c("total_cases", "total_deaths", "total_recovered")]
      x3 <- x3[order(date), `:=` (
        daily_cases = total_cases - shift(total_cases),
        daily_recovered = total_recovered - shift(total_recovered),
        daily_deaths = total_deaths - shift(total_deaths)
      )][]

      x3

    }

    nat <- quick_fix(state)

    vax_dat <- get_state_vax()[date <= nat[, max(date)]][place == "India", .(date, daily_doses)][, lag := daily_doses][]

    if (!is.null(t)) {
      try(if (!is.Date(t)) stop("t needs to be a date (YYYY-MM-DD)"))
      today <- as.Date(t)
    } else {
      today     <- min(max(nat$date, na.rm = TRUE),
                       max(vax_dat$date, na.rm = TRUE))
    }

    yesterday <- today - 1
    week_ago  <- today - 7
    month_ago <- today - 30

    get_stats <- function(d) {

      tmp_nat <- nat[date == d]
      tmp_deaths <- tmp_nat[, daily_deaths]
      tmp_cases  <- tmp_nat[, daily_cases]

      tmp_vax <- vax_dat[date == d, lag]

      data.table(
        Day        = fifelse(d == today, "Today",
                      fifelse(d == yesterday, "Yesterday",
                             fifelse(d == week_ago, "One week ago",
                                    fifelse(d == month_ago, "One month ago", NA)))),
        Date       = format(d, "%m/%d"),
        Deaths     = format(tmp_deaths, big.mark = ","),
        Cases      = format(tmp_cases, big.mark = ","),
        Vaccines   = format(tmp_vax, big.mark = ",")
      )

    }

    today_stats     <- get_stats(today)
    yesterday_stats <- get_stats(yesterday)
    week_ago_stats  <- get_stats(week_ago)
    month_ago_stats <- get_stats(month_ago)

    rbindlist(list(
      today_stats,
      yesterday_stats,
      week_ago_stats,
      month_ago_stats
    ))

  }

  make_pretty <- function(x) {
    x %>%
      gt() %>%
      # bold column headers
      tab_style(
        style = cell_text(
          font = "arial",
          weight = "bold"
        ),
        locations = cells_column_labels(everything())
      ) %>%
      # center column headers
      tab_style(
        style = cell_text(
          align = "center"
        ),
        locations = cells_column_labels(c(Date, Deaths, Cases))
      ) %>%
      # format columns
      tab_style(
        style = list(
          cell_text(
            font = "arial",
            align = "center"
          )
        ),
        locations = list(
          cells_body(columns = c(Date, Deaths, Cases))
        )
      )
  }

  # run ----------
  snap <- get_snap()

  # today <- as.Date(snap[Day == "Today", Date], "%m/%d")

  return(make_pretty(snap))
}
