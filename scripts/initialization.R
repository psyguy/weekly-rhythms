## @knitr load_packages

libs_used <-
  c("plyr",
    "tidyverse",
    "knitr",
    "patchwork",
    "cowplot",
    "scales",
    "circular",
    "svglite",
    "future",
    "doFuture",
    "devtools",
    "shiny",
    "here",
    "future",
    "doFuture",
    "stats",
    "imputeTS",
    "sarima",
    "lubridate",
    "forecast")

libs_needed <- libs_used[!libs_used %in% installed.packages()]

sapply(libs_needed,
       install.packages,
       dependencies = TRUE,
       repos = "https://cloud.r-project.org/")
sapply(libs_used, require, character = TRUE)

rm(libs_used, libs_needed)

devtools::install_github('Mikata-Project/ggthemr',
                         upgrade = FALSE)
library(ggthemr)
ggthemr::ggthemr("pale")


## @knitr data_shaper

data_shaper <- function(d,
                        minimal_output = FALSE) {

  weekdays <- c("Mon",
                "Tue",
                "Wed",
                "Thu",
                "Fri",
                "Sat",
                "Sun")

  if (!is.data.frame(d)){
    d <- data.frame(
      t = 1:length(d),
      y = d,
      weekday = rep(weekdays, length.out = length(d)),
      week_num = rep(1:ceiling(length(d) / 7), each = 7)[1:length(d)]
    )
    if(minimal_output == TRUE) return(d)
  }

  if ("date" %in% colnames(d))
    d <- d %>%
      mutate(
        weekday = lubridate::wday(date,
                                  week_start = 1,
                                  label = TRUE),
        weekday_num = lubridate::wday(date,
                                      week_start = 1,
                                      label = FALSE)
      )

  if (!("weekday_num" %in% colnames(d)))
    d$weekday_num <- match(d$weekday, weekdays)

  if (!("week_num" %in% colnames(d))) {
    # Initialize week number and an empty vector to store week numbers
    week_number <- 1
    week_numbers <- numeric(length(d$weekday_num))
    # Check if the sequence starts with a day other than Monday and adjust week_number accordingly
    if (d$weekday_num[1] != 1) {
      week_number <- 1
    } else {
      week_number <-
        2  # Start from week 2 if the first day is Monday, to handle edge cases
    }
    # Iterate through the days, increasing week number after encountering a Sunday
    for (i in 1:length(d$weekday_num)) {
      week_numbers[i] <- week_number
      if (d$weekday_num[i] == 7 &&
          i != length(d$weekday_num)) {
        # Check for Sunday and not the last element
        week_number <- week_number + 1
      }
    }
    d$week_num <- week_numbers
  }

  # Substituting NA's for implicit missing values
  d_out <- d %>%
    right_join(data.frame(t = min(d$t):max(d$t)),
               by = "t") %>%
    mutate(weekday = weekday %>% factor(weekdays)) %>%
    arrange(t)

  return(d_out)

}

