## @knitr m_fit

m_fit <- function(d,
                  model_string = "d + sarma(1,1)(1,1)",
                  id = NULL,
                  save_fit = FALSE,
                  save_est = FALSE,
                  save_name = NULL,
                  save_prefix_fit = "fit",
                  save_folder_fit = "fits",
                  save_prefix_est = "est",
                  save_folder_est = "ests",
                  ...) {

  model_string <- model_string %>%
    tolower() %>%
    gsub(" ", "", .)

  # Extracting mean structure
  mu <- "c"
  if (grepl("d", model_string, fixed = TRUE))
    mu <- "d"
  if (grepl("w", model_string, fixed = TRUE))
    mu <- "w"
  if (grepl("h", model_string, fixed = TRUE))
    mu <- "h"

  # Extracting [S]AR[I]MA orders
  orders <- model_string %>%
    str_extract_all("\\d+") %>%
    unlist() %>%
    as.numeric()


  # Adding NA to make sure the first element is for Monday
  y_t <- d$y %>%
    c(rep(NA, (d$weekday_num[1] - 1) %% 7),
      .)
  # Getting the length of the time series
  n <- length(y_t)

  ## Building mu_t
  # To make use Arima consistently, instead of changing
  # include.mean for mu_y = c, we model mean with a constant
  # variable for mu_t
  if (mu == "c")
    mu_t <- rep(1, n)

  # Making the dummies matrix
  if (mu == "d")
    mu_t <- cbind(
      Mon = rep(c(1, 0, 0, 0, 0, 0, 0), length.out = n),
      Tue = rep(c(0, 1, 0, 0, 0, 0, 0), length.out = n),
      Wed = rep(c(0, 0, 1, 0, 0, 0, 0), length.out = n),
      Thu = rep(c(0, 0, 0, 1, 0, 0, 0), length.out = n),
      Fri = rep(c(0, 0, 0, 0, 1, 0, 0), length.out = n),
      Sat = rep(c(0, 0, 0, 0, 0, 1, 0), length.out = n),
      Sun = rep(c(0, 0, 0, 0, 0, 0, 1), length.out = n)
    )

  # Making weekday-weekend dummies matrix
  if (mu == "w")
    mu_t <- cbind(
      # Weekday = rep(c(1, 1, 1, 1, 1, 0, 0), length.out = n),
      c = rep(c(1, 1, 1, 1, 1, 1, 1), length.out = n),
      Weekend = rep(c(0, 0, 0, 0, 0, 1, 1), length.out = n)
    )

  # Making the harmonic matrix
  if (mu == "h")
    mu_t <- cbind(
      c = rep(1, n),
      a_cos = cos((1:n) * 2 * pi / 7),
      b_sin = sin((1:n) * 2 * pi / 7)
    )


  ## Defining the model orders from the model string
  # Default: white noise
  order_daily <- c(0, 0, 0)
  # Default: non-seasonal
  order_seasonal <- c(0, 0, 0)
  seasonal <- NULL

  # Setting orders conditional on the model string
  if (grepl("ar(", model_string, fixed = TRUE)) {
    order_daily <- c(orders[1], 0, 0)
  }
  if (grepl("sar(", model_string, fixed = TRUE)) {
    order_daily <- c(orders[1], 0, 0)
    order_seasonal <- c(orders[2], 0, 0)
  }
  if (grepl("ma(", model_string, fixed = TRUE)) {
    order_daily <- c(0, 0, orders[1])
  }
  if (grepl("sma(", model_string, fixed = TRUE)) {
    order_daily <- c(0, 0, orders[1])
    order_seasonal <- c(0, 0, orders[2])
  }
  if (grepl("arma(", model_string, fixed = TRUE)) {
    order_daily <- c(orders[1], 0, orders[2])
  }
  if (grepl("sarma(", model_string, fixed = TRUE)) {
    order_daily <- c(orders[1], 0, orders[2])
    order_seasonal <- c(orders[3], 0, orders[4])
  }

  if (sum(order_seasonal) != 0)
    seasonal <- list(order = order_seasonal,
                     period = 7)

  ## Fitting the model to the time series
  m <- forecast::Arima(
    y_t,
    order = order_daily,
    seasonal = seasonal,
    xreg = mu_t,
    include.mean = FALSE
  )

  if (is.null(save_name))
    save_name <- paste0(
      mu,
      "+sarma(",
      order_daily[1],
      ",",
      order_daily[3],
      ")(",
      order_seasonal[1],
      ",",
      order_seasonal[3],
      ")"
    )

  if(!is.null(id)) save_name <- paste0("id-",
                                       id,
                                       "_",
                                       save_name)
  if (save_fit == TRUE)
    saveRDS(m,
            file = save_name %>%
              paste0(save_prefix_fit,
                     "_",
                     .,
                     ".rds") %>%
              here::here(save_folder_fit,
                         .))
  if (save_est == TRUE)
    m %>%
    m_estimates(...) %>%
    saveRDS(file = save_name %>%
              paste0(save_prefix_est,
                     "_",
                     .,
                     ".rds") %>%
              here::here(save_folder_est,
                         .))
  ## Returning the model object
  return(m)
}

## @knitr aux_functions

## Calculating amplitude and
calc_amp <- function(a, b)
  sqrt(a ^ 2 + b ^ 2)
# Vectorizing it
calc_amp <- Vectorize(calc_amp)

## Calculating peak shift
calc_peak_theta <- function(a, b) {
  theta <- atan(b / a) + (pi / 2) * sign(b) * (1 - sign(a))
  # psi <- (7*theta/(2*pi)) #%% 7
  # psi <- theta
  return(theta)
}
calc_peak_theta <- Vectorize(calc_peak_theta)

## @knitr m_estimates

m_estimates <- function(m,
                        boot_plot = FALSE,
                        boot_n = 10000,
                        boot_seed = 0) {
  # Point estimates of the parameters
  o_est <- coef(m)
  # Varviance-covariance matrix of the parameter estimates
  o_vcov <- vcov(m)

  # Changing the name of "xreg" to "c" for mu_t == "c"
  xreg_index <- names(o_est) == "xreg"
  names(o_est)[xreg_index] <- "c"
  rownames(o_vcov)[xreg_index] <- "c"
  colnames(o_vcov)[xreg_index] <- "c"

  o_sd <- o_vcov %>%
    diag() %>%
    sqrt()

  # Making the output table
  estimates <- data.frame(
    parameter = "sigma2",
    est = m$sigma2,
    se = NA,
    CI_2.5 = NA,
    CI_97.5 = NA,
    sig = "*"
  )
  rownames(estimates) <- "sigma2"

  if (length(o_est) > 0)
    estimates <- data.frame(parameter = o_est %>% names,
                            est = o_est,
                            se = o_sd) %>%
    mutate(
      CI_2.5 = est + qnorm(0.025) * se,
      CI_97.5 = est + qnorm(0.975) * se,
      sig = case_when(CI_2.5 * CI_97.5 > 0 ~ "*",
                      TRUE ~ "ns")
    ) %>%
    rbind(estimates)

  rownames(estimates) <- NULL

  # Bootstrapping -----------------------------------------------------------

  # Checking if h_t was fitted which would require bootstrapping
  if (sum(names(m$coef) == "a_cos",
          names(m$coef) == "b_sin")) {
    # Selecting est and vcov of only the relevant parameters for h_t
    o_est_h <- o_est[c("c", "a_cos", "b_sin")]
    o_vcov_h <-
      o_vcov[c("c", "a_cos", "b_sin"), c("c", "a_cos", "b_sin")]

    # Bootstrap sampling
    set.seed(boot_seed)

    boot_parameters <- MASS::mvrnorm(boot_n,
                                     o_est_h,
                                     o_vcov_h)

    boot_c <- boot_parameters[, "c"]
    boot_a <- boot_parameters[, "a_cos"]
    boot_b <- boot_parameters[, "b_sin"]

    boot_s <- sqrt(boot_a^2 + boot_b^2)
    boot_psi_radian <- atan2(boot_b, boot_a) %>%
      as.numeric() %>%
      as.circular()
    boot_psi_radian_quantiles <- boot_psi_radian %>%
      as.circular() %>%
      quantile(c(.025, .5, .975)) %>%
      as.numeric()

    boot_psi <- boot_psi_radian * 7 / (2 * pi)
    boot_psi_radian_mod <- boot_psi_radian %% (2 * pi)
    # plot(boot_s, boot_psi_radian)

    # Bootstrapped summaries
    b_c <- mean(boot_c)
    b_s <- median(boot_s)
    b_s_sd <- sd(boot_s)
    b_s_CIs <- as.numeric(quantile(boot_s, c(0.025, 0.975)))
    b_psi <- as.numeric(median(boot_psi_radian)) * 7 / (2 * pi)
    b_psi_sd <- as.numeric(sd(boot_psi_radian)) * 7 / (2 * pi)
    b_psi_CIs <-
      as.numeric(quantile(boot_psi_radian, c(0.025, 0.975))) * 7 / (2 * pi)

    # Adding the estimates to the table
    boot_estimates <- data.frame(
      parameter = c("amp", "peak_shift"),
      est = c(b_s, b_psi),
      se = c(b_s_sd, b_psi_sd),
      CI_2.5 = c(b_s_CIs[1], b_psi_CIs[1]),
      CI_97.5 = c(b_s_CIs[2], b_psi_CIs[2]),
      sig = c("*", "*")
    )

    estimates <- rbind(estimates, boot_estimates)

    if (boot_plot) {
      t <- seq(0, 7, .1)
      tt <- length(t)
      rr <- 100
      df <- data.frame(
        r = rep(1:rr, each = tt) %>% as.character(),
        c = rep(boot_c %>% head(rr), each = tt),
        s = rep(boot_s %>% head(rr), each = tt),
        psi = rep(boot_psi %>% head(rr), each = tt),
        t = rep(t, times = rr)
      ) %>%
        mutate(h_t = c + s * cos((t - psi) * 2 * pi / 7))

      p_boot <- df %>%
        group_by(r) %>%
        ggplot(alpha = 0.05,
               lwd = 0.1,
               linetype = "solid") +
        aes(x = t,
            y = h_t,
            group = r) +
        # Separate curves for the first 100 bootstrapped samples
        geom_line(aes(
          x = t,
          y = h_t,
          group = r,
          color = "single_curves"
        )) +
        # Average of the separate curves the first 100 bootstrapped samples
        stat_summary(
          fun.y = mean,
          geom = "line",
          lwd = 1.5,
          aes(group = -1, color = "mean_curve"),
          alpha = 1
        ) +
        # Curve calculated by bootstrapping point estimates
        geom_function(
          fun = function(t)
            b_c + b_s * cos(2 * pi / 7 * (t - b_psi)),
          mapping = aes(color = "boot_curve"),
          linetype = "solid",
          lwd = .5,
          alpha = 0.5
        ) +
        geom_hline(yintercept = b_c + b_s,
                   color = "red",
                   linetype = "solid",
                   lwd = 1,
                   alpha = 0.85) +
        geom_vline(xintercept = b_psi %% 7,
                   color = "red",
                   linetype = "solid",
                   lwd = 1,
                   alpha = 0.85) +
        # Curve calculated by a and b point estimates without bootstrapping
        geom_function(
          fun = function(t)
            o_est_h[1] +
            o_est_h[2] * cos(t * 2 * pi / 7) +
            o_est_h[3] * sin(t * 2 * pi / 7),
          mapping = aes(color = "naive_curve"),
          linetype = "twodash",
          lwd = .5,
          alpha = 0.5
        ) +
        geom_hline(yintercept = o_est_h[1] + sqrt(o_est_h[2]^2 + o_est_h[3]^2),
                   color = "green",
                   linetype = "twodash",
                   lwd = 1,
                   alpha = 0.85) +
        geom_vline(xintercept = (calc_peak_theta(o_est_h[2], o_est_h[3])*7/(2*pi)) %% 7,
                   color = "green",
                   linetype = "twodash",
                   lwd = 1,
                   alpha = 0.85) +
        geom_vline(xintercept = (atan(o_est_h[3]/o_est_h[2])*7/(2*pi)) %% 7,
                   color = "black",
                   linetype = "dotted",
                   lwd = 1,
                   alpha = 0.85)
        scale_color_manual(
          name = "Y series",
          values = c(
            "single_curves" = "pink",
            "mean_curve" = "darkblue",
            "boot_curve" = "red",
            "naive_curve" = "green"
          )
        )

    }
  }

  estimates[2:5] <- estimates[2:5] %>% round(4)

  information_criteria <- data.frame(
    AIC = m$aic,
    AICc = m$aicc,
    BIC = m$bic,
    ll_abs = abs(m$loglik)
  )

  o <- list(estimates = estimates,
            information_criteria = information_criteria)

  # if (exists("p_boot"))
  #   o$boot_plot <- p_boot

  return(o)

}
