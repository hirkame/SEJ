#' @export
run_ols <- function(data, outcome, control = FALSE, hetero_edu = FALSE) {
  if (control == FALSE & hetero_edu == FALSE) {
    fml <- stats::as.formula(paste0(outcome, "~ eligible*period"))
  } else if (control == FALSE & hetero_edu == TRUE) {
    fml <- stats::as.formula(paste0(outcome, "~ eligible*period*educ"))
  } else if (control == TRUE & hetero_edu == FALSE) {
    fml <- stats::as.formula(paste0(outcome, " ~ eligible*period + experience + I(experience^2) + age + I(age^2) + educ + civilstatus + region + urb"))
  } else {
    fml <- stats::as.formula(paste0(outcome, " ~ eligible*period*educ + experience + I(experience^2) + age + I(age^2) + civilstatus + region + urb"))
  }
  
  mod <- survey::svyglm(fml, design = data)
  
  return(mod)
}

#' @export
run_ols_2 <- function(data, outcome, control = FALSE, hetero_edu = FALSE) {
  if (control == FALSE & hetero_edu == FALSE) {
    fml <- stats::as.formula(paste0(outcome, "~ eligible*period"))
  } else if (control == FALSE & hetero_edu == TRUE) {
    fml <- stats::as.formula(paste0(outcome, "~ eligible*period*educ"))
  } else if (control == TRUE & hetero_edu == FALSE) {
    fml <- stats::as.formula(paste0(outcome, " ~ eligible*period + experience + I(experience^2) + age + I(age^2) + educ + civilstatus + region"))
  } else {
    fml <- stats::as.formula(paste0(outcome, " ~ eligible*period*educ + experience + I(experience^2) + age + I(age^2) + civilstatus + region"))
  }
  
  mod <- survey::svyglm(fml, design = data)
  
  return(mod)
}

# Extract coefficients and confidence intervals for the OLS
#' @export
prep_event_study_plot <- function(model) {
  df_model <- broom::tidy(model, conf.int = TRUE) |>
    dplyr::filter(stringr::str_detect(term, "eligible:period")) |>
    dplyr::mutate(period = stringr::str_replace(term, "eligible:period", "") |> as.numeric()) |>
    dplyr::add_row(period = -3, estimate = 0, conf.low = 0, conf.high = 0)
  return(df_model)
}

#' @export
plot_event_study <- function(mod, output_title = "Event Study") {
  event_study_plot <- prep_event_study_plot(mod) |>
    dplyr::mutate(model = "Control")
  
  # Calculate the range of periods to set the x-axis limits and breaks
  min_period <- min(event_study_plot$period)
  max_period <- max(event_study_plot$period)
  breaks <- seq(min_period, max_period, by = 2)
  labels <- breaks + 2009  # Adjust the labels to reflect the years
  
  g <- ggplot2::ggplot(event_study_plot, ggplot2::aes(x = period, y = estimate, ymin = conf.low, ymax = conf.high)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "gray") +
    ggplot2::geom_vline(xintercept = -3, linetype = "dashed", colour = "gray") +  # Dashed line at period -3 (2006)
    ggplot2::geom_errorbar(width = 0.4) +
    ggplot2::geom_point() +
    ggplot2::labs(x = "Years", y = "Coefficient", title = output_title) + 
    ggplot2::scale_x_continuous(breaks = breaks, labels = labels) +
    ggplot2::theme_bw()
  
  return(g)
}




#' @export
plot_event_study_educ <- function(mod) {
  
  event_study_plot <- broom::tidy(mod, conf.int = T)　|>
    dplyr::filter(stringr::str_detect(term, "eligible:period")) |> 
    dplyr::mutate(
      period = stringr::str_extract(term, "(?<=eligible:period)-?\\d+")|> as.numeric(), 
      educ = dplyr::case_when(
        stringr::str_detect(term, "educ4") ~ "Higher Education", 
        stringr::str_detect(term, "educ3") ~ "Secondary Education",
        stringr::str_detect(term, "educ2") ~ "Primary Education",
        TRUE ~ "No Education"
      )
    ) |> 
    dplyr::add_row(period = c(-3,-3,-3,-3), 
                   estimate = c(0,0,0,0), 
                   conf.low = c(0,0,0,0), 
                   conf.high = c(0,0,0,0), 
                   educ = c("No Education", "Primary Education", "Secondary Education", "Higher Education")) |> 
    dplyr::mutate(
      educ = as.factor(educ)
    )
    
  
  g <- ggplot2::ggplot(event_study_plot, 
                       ggplot2::aes(x = period, y = estimate, ymin = conf.low, ymax = conf.high, colour = educ)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", colour = "gray") +
    ggplot2::geom_vline(xintercept = -3, linetype = "dashed", colour = "gray") +
    ggplot2::geom_errorbar(width = 0.4, position = ggplot2::position_dodge(0.7)) +
    ggplot2::geom_point(position = ggplot2::position_dodge(0.7)) +
    ggplot2::labs(x = "Period", y = "Coefficient") + 
    ggplot2::theme_bw()
  
  return(g)
  
}

#' #' @export
#' run_feols <- function(data, outcome, control = TRUE) {
#'   if (control == FALSE) {
#'     mod <- feols(
#'       stats::as.formula(paste0(outcome, " ~ eligible + i(period, eligible, 0) + eligible:lineartrend | period")), 
#'       data, 
#'       # weights = data$expr,
#'       "iid"
#'     )
#'   } else {
#'     mod <- feols(
#'       stats::as.formula(paste0(outcome, " ~ eligible + i(period, eligible, 0) + schooling + I(schooling^2) + civilstatus + region*urb | period")),
#'       data,
#'       # weights = data$expr,
#'       vcov = "cluster"
#'     )
#'   }
#'   
#'   return(mod)
#' }

#' #' @export
#' run_cs <- function(data, outcome, xfm = NULL, est_method = "ipw") {
#'   if (is.null(xfm)) {
#'     mod <- did::att_gt(
#'       yname = outcome, 
#'       tname = "year", 
#'       gname = "gname", 
#'       data = data,
#'       panel = FALSE, 
#'       # weightsname = "expr", 
#'       base_period = "universal", 
#'       est_method = est_method
#'     )
#'   } else {
#'     mod <- did::att_gt(
#'       yname = outcome, 
#'       tname = "year", 
#'       xformla = stats::as.formula(xfm),
#'       gname = "gname", 
#'       data = data,
#'       panel = FALSE, 
#'       # weightsname = "expr", 
#'       base_period = "universal", 
#'       est_method = est_method
#'     )
#'   }
#'   
#'   return(mod)
#' }

#' @export
plot_mean <- function(data, outcome) {
  data <- data |> 
    dplyr::summarize(
      mean = mean(!!rlang::sym(outcome), na.rm = TRUE),
      .by = c(eligible, period)
    ) |> 
    dplyr::mutate(eligible = as.factor(eligible), period= as.numeric(as.character(period)))
  label_years <- function(periods) {
    periods + 2009
  }
  breaks <- seq(min(data$period), max(data$period), by = 2)
  g <- ggplot2::ggplot(data, ggplot2::aes(x = period, y = mean, group = eligible, colour = eligible)) +
    ggplot2::geom_vline(xintercept = -3, linetype = "dashed", colour = 1) +
    ggplot2::geom_point() +
    ggplot2::geom_line() + 
    ggplot2::scale_x_continuous(
      breaks = breaks,
      labels = label_years(breaks)
    ) +
    ggplot2::labs(x = "Year", y = paste0("Mean ", outcome), title = paste0(outcome)) 
    ggplot2::theme_bw()
  
  return(g)
  }


#' #' @export
#' run_all_regressions <- function(outcome, data) {
#'   
#'   # Define the formulas
#'   simple_formula <- stats::as.formula(paste0(outcome, " ~ eligible*post"))
#'   multiple_formula <- stats::as.formula(paste0(outcome, " ~ eligible*post + experience + I(experience^2) + schooling + I(schooling^2) + civilstatus + region*urb"))
#'   
#'   # Run the regressions
#'   simple_reg <- stats::lm(simple_formula, data, 
#'                           # weights = data$expr
#'                           )
#'   multiple_reg <- stats::lm(multiple_formula, data, 
#'                             # weights = data$expr
#'                             )
#'   
#'   print("DONE - OLS")
#'   
#'   # Double robust DID estimation
#'   sz <- DRDID::drdid(
#'     yname = outcome,
#'     tname = "post",
#'     idname = "id",
#'     dname = "eligible",
#'     xformla = ~ schooling + civilstatus + urb,
#'     data = data,
#'     panel = FALSE,
#'     estMethod = "imp",
#'     # weightsname = "expr"
#'   )
#'   
#'   print("DONE - Doubly robust DID")
#'   
#'   # Inverse probability weighted DID estimation
#'   ipw <- DRDID::ipwdid(
#'     yname = outcome,
#'     tname = "post",
#'     idname = "id",
#'     dname = "eligible",
#'     xformla = ~ experience + I(experience^2) + schooling + I(schooling^2) + civilstatus + region*urb,
#'     data = data,
#'     panel = FALSE,
#'     boot = TRUE,
#'     nboot = 199,
#'     # weightsname = "expr"
#'   )
#'   
#'   print("DONE - IPW DID")
#'   
#'   # Tidy the results for OLS models and filter for the relevant term
#'   simple_reg_tidy <- broom::tidy(simple_reg) |> 
#'     dplyr::filter(term == "eligible:post") |> 
#'     dplyr::mutate(term = "Pooled OLS")
#'   multiple_reg_tidy <- broom::tidy(multiple_reg) |> 
#'     dplyr::filter(term == "eligible:post") |> 
#'     dplyr::mutate(term = "Pooled OLS with Controls")
#'   
#'   # Manually create tidy results for DRDID
#'   sz_tidy <- data.frame(
#'     term = c("DR DID"),
#'     estimate = sz$ATT,
#'     std.error = sz$se,
#'     statistic = sz$ATT / sz$se,
#'     p.value = ifelse(stats::pt(abs(sz$ATT / sz$se), nrow(data)) * 2  > 1, 
#'                      2 - stats::pt(abs(sz$ATT / sz$se), nrow(data)) * 2, 
#'                      stats::pt(abs(sz$ATT / sz$se), nrow(data)) * 2)
#'   )
#'   
#'   # Manually create tidy results for IPW DID
#'   ipw_tidy <- data.frame(
#'     term = c("IPW DID"),
#'     estimate = ipw$ATT,
#'     std.error = ipw$se,
#'     statistic = ipw$ATT / ipw$se,
#'     p.value = ifelse(stats::pt(abs(ipw$ATT / ipw$se), nrow(data)) * 2  > 1, 
#'                      2 - stats::pt(abs(ipw$ATT / ipw$se), nrow(data)) * 2, 
#'                      stats::pt(abs(ipw$ATT / ipw$se), nrow(data)) * 2)
#'   )
#'   
#'   # Combine the results
#'   combined_results <- dplyr::bind_rows(simple_reg_tidy, multiple_reg_tidy, sz_tidy, ipw_tidy)
#'   
#'   return(combined_results)
#' }

# #' @export
# sensitivity_check <- function(m1, relativeMagnitudes = TRUE) {
#   betahat <- m1[["coefficients"]][-1:-8] # save the coefficients
#   sigma <- summary(m1)[["cov.unscaled"]][-1:-8, -1:-8] # save the covariance matrix
#   
#   originalResults <- constructOriginalCS(
#     betahat = betahat,
#     sigma = sigma,
#     numPrePeriods = 3,
#     numPostPeriods = 3
#   )
#   
#   if (relativeMagnitudes == TRUE) {
#     delta_rm_results <-
#       createSensitivityResults_relativeMagnitudes(
#         betahat = betahat, # coefficients
#         sigma = sigma, # covariance matrix
#         numPrePeriods = 3, # number of pre-treatment coefficients
#         numPostPeriods = 3, # number of post-treatment coefficients
#         Mbarvec = seq(0.5, 2, by = 0.5) # values of Mbar
#       )
#     
#     g <- createSensitivityPlot_relativeMagnitudes(delta_rm_results, originalResults)
#     
#   } else {
#     delta_sd_results <- createSensitivityResults(
#       betahat = betahat,
#       sigma = sigma,
#       numPrePeriods = 3,
#       numPostPeriods = 3,
#       Mvec = seq(from = 0, to = 0.05, by = 0.01)
#     )
#     
#     g <- createSensitivityPlot(delta_sd_results, originalResults)
#   }
#   
#   return(g)
# }
