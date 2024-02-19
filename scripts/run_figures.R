## @knitr plot_empirical_6_individuals

d_pa <- readRDS(here::here("data",
                           "d_pa.rds"))
dir.create(file.path(here::here("figures")))

names_selected_6 <-
  c("Person A",
    "Person B",
    "Person C",
    "Person D",
    "Person E",
    "Person F")

d_selected_6 <- d_pa %>%
  mutate(
    individual = case_when(
      id == 70 ~ "Person A",
      id == 14 ~ "Person B",
      id == 12 ~ "Person C",
      id == 43 ~ "Person D",
      id == 97 ~ "Person E",
      id == 20 ~ "Person F",
      TRUE ~ "rest"
    ) %>%
      as.factor()
  ) %>%
  filter(individual != "rest")

l_selected_6 <- list()
for (name in names_selected_6){
  l_selected_6[[name]] <- d_selected_6 %>%
    filter(individual == name)
}

plot_row_assembly(l_selected_6, names(l_selected_6),
                  max_period = 35,
                  save_name = "rows-empirical-35.svg")

## @knitr plot_empirical_98_individuals

for(batch in 1:14){
  ids <- split(1:98,
               ceiling(seq_along(1:98)/7))[[batch]]
  l_selected_batch <- list()
  for (id_ in ids){
    l_selected_batch[[paste("ID:", id_)]] <- d_pa %>%
      filter(id == id_)
  }
  plot_row_assembly(l_selected_batch,
                    names(l_selected_batch),
                    max_period = 35,
                    save_name = paste0("rows-empirical-35_batch-",
                                       batch,
                                       ".svg"))

}


## @knitr plot_simulated_ts

plot_sim_rows(fixed_c = 0,
              prefix = "c")

plot_sim_rows(fixed_dowe = c(0.1, 0.3, 0.3, -0.7, 0.3, 0.9, -0.5),
              prefix = "d")

plot_sim_rows(fixed_c = 0.1,
              fixed_amp = 0.8,
              fixed_peak_shift = 6,
              prefix = "h")

plot_sim_rows(fixed_c = 0.1,
              fixed_wee = 0.8,
              prefix = "w")



## @knitr plot_sim_parameters

## Parameters for mu_t
fixed_mu <- 0 # only for
fixed_dowe <- c(0.1, 0.3, 0.3, -0.7, 0.3, 0.9, -0.5)
fixed_c <- 0.5
fixed_amp <- 0
fixed_peak_shift <- 6
fixed_wee <- 0.8

## Fixed a_t
fixed_sigma2 <- 1
fixed_ma <- 0.6
fixed_ar <- 0.7
fixed_sar <- 0.4
fixed_sma <- 0.5
