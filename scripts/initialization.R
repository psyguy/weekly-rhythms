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

# rm(libs_used, libs_needed)

devtools::install_github('Mikata-Project/ggthemr',
                         upgrade = FALSE)
library(ggthemr)
ggthemr::ggthemr("pale")
