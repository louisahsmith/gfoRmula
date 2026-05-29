test_that("Survival outcomes", {
  id <- 'id'
  time_points <- 7
  time_name <- 't0'
  covnames <- c('L1', 'L2', 'A')
  outcome_name <- 'Y'
  outcome_type <- 'survival'
  covtypes <- c('binary', 'bounded normal', 'binary')
  histories <- c(lagged, lagavg)
  histvars <- list(c('A', 'L1', 'L2'), c('L1', 'L2'))
  covparams <- list(covmodels = c(L1 ~ lag1_A + lag_cumavg1_L1 + lag_cumavg1_L2 +
                                    L3 + t0,
                                  L2 ~ lag1_A + lag_cumavg1_L1 +
                                    lag_cumavg1_L2 + L3 + t0,
                                  A ~ lag1_A + L1 + L2 + lag_cumavg1_L1 +
                                    lag_cumavg1_L2 + L3 + t0))
  ymodel <- Y ~ A + L1 + L2 + L3 + lag1_A + lag1_L1 + lag1_L2 + t0

  expect_no_error(
    gform_basic <- gformula(obs_data = basicdata_nocomp, id = id,
                            time_points = time_points,
                            time_name = time_name, covnames = covnames,
                            outcome_name = outcome_name,
                            outcome_type = outcome_type, covtypes = covtypes,
                            covparams = covparams, ymodel = ymodel,
                            intervention1.A = list(static, rep(0, time_points)),
                            intervention2.A = list(static, rep(1, time_points)),
                            int_descript = c('Never treat', 'Always treat'),
                            histories = histories, histvars = histvars,
                            basecovs = c('L3'), intcomp = c(1,2),
                            nsimul = 2501,
                            seed = 1234, nsamples = 2,
                            restrictions = list(c('L2', 'L1 == 0', simple_restriction, 0)))
  )

  compevent_name <- 'D'
  compevent_model <- D ~ A + L1 + L2 + lag1_A + lag2_A + lag3_A
  expect_no_error(
    gform_basic <- gformula(obs_data = basicdata, id = id,
                            time_points = time_points,
                            time_name = time_name, covnames = covnames,
                            outcome_name = outcome_name,
                            outcome_type = outcome_type,
                            compevent_name = compevent_name,
                            covtypes = covtypes,
                            covparams = covparams, ymodel = ymodel,
                            compevent_model = compevent_model,
                            intervention1.A = list(static, rep(0, time_points)),
                            intervention2.A = list(static, rep(1, time_points)),
                            int_descript = c('Never treat', 'Always treat'),
                            histories = histories, histvars = histvars,
                            basecovs = c('L3'), intcomp = c(1,2),
                            nsimul = 2500,
                            seed = 1234, nsamples = 2,
                            ci_method = 'normal',
                            model_fits = TRUE,
                            sim_trunc = FALSE,
                            restrictions = list(c('L2', 't0%in%c(0, 3, 6)',
                                                  carry_forward)))
  )
})


test_that("Continuous outcomes", {

  suppressPackageStartupMessages(suppressWarnings(library('Hmisc')))
  id <- 'id'
  time_name <- 't0'
  covnames <- c('L1', 'L2', 'A')
  outcome_name <- 'Y'
  outcome_type <- 'continuous_eof'
  covtypes <- c('categorical', 'normal', 'binary')
  histories <- c(lagged)
  histvars <- list(c('A', 'L1', 'L2'))
  covparams <- list(covmodels = c(L1 ~ lag1_A + lag1_L1 + t0 +
                                    rcspline.eval(lag1_L2, knots = c(-1, 0, 1)),
                                  L2 ~ lag1_A + L1 + lag1_L1 + lag1_L2 + t0,
                                  A ~ lag1_A + L1 + L2 + lag1_L1 + lag1_L2 + t0))
  ymodel <- Y ~ A + L1 + L2 + lag1_A + lag1_L1 + lag1_L2

  expect_no_error(
    gform_cont_eof <- gformula(obs_data = continuous_eofdata,
                             id = id, time_name = time_name,
                             covnames = covnames, outcome_name = outcome_name,
                             outcome_type = outcome_type, covtypes = covtypes,
                             covparams = covparams, ymodel = ymodel,
                             intervention1.A = list(static, rep(0, 7), int_times = 0:5),
                             intervention2.A = list(static, rep(1, 7)),
                             int_descript = c('Never treat', 'Always treat'),
                             histories = histories, histvars = histvars,
                             nsimul = 2500, seed = 1234,
                             nsamples = 2)
  )

})


test_that("Binary outcomes", {

  binary_eofdata$time_f <- ifelse(binary_eofdata$time <= 1, 0,
                                  ifelse(binary_eofdata$time <= 3, 1,
                                         ifelse(binary_eofdata$time <= 5, 2, 3)))
  binary_eofdata$time_f <- as.factor(binary_eofdata$time_f)

  outcome_type <- 'binary_eof'
  id <- 'id_num'
  time_name <- 'time'
  covnames <- c('cov1', 'cov2', 'treat', 'time_f')
  outcome_name <- 'outcome'
  histories <- c(lagged, cumavg)
  histvars <- list(c('treat', 'cov1', 'cov2'), c('cov1', 'cov2'))
  covtypes <- c('binary', 'zero-inflated normal', 'normal', 'categorical time')
  covparams <- list(covmodels = c(cov1 ~ lag1_treat + lag1_cov1 + lag1_cov2 +
                                    cov3 + time_f,
                                  cov2 ~ lag1_treat + cov1 + lag1_cov1 +
                                    lag1_cov2 + cov3 + time_f,
                                  treat ~ lag1_treat + cumavg_cov1 +
                                    cumavg_cov2 + cov3 + time_f,
                                  NA))
  ymodel <- outcome ~  treat + cov1 + cov2 + lag1_cov1 + lag1_cov2 + cov3

  expect_no_error(
    gform_bin_eof <- gformula(obs_data = binary_eofdata,
                              outcome_type = outcome_type, id = id,
                              time_name = time_name, covnames = covnames,
                              outcome_name = outcome_name, covtypes = covtypes,
                              covparams = covparams, ymodel = ymodel,
                              intervention1.treat = list(threshold, 1, Inf),
                              int_descript = 'Threshold - lower bound 1',
                              histories = histories,
                              histvars = histvars, basecovs = c("cov3"),
                              seed = 1234, parallel = TRUE,
                              nsimul = 2501, ncores = 2)
  )

})


test_that("IPCW", {

  covnames <- c('L', 'A')
  histories <- c(lagged)
  histvars <- list(c('A', 'L'))
  ymodel <- Y ~ L + A
  covtypes <- c('binary', 'normal')
  covparams <- list(covmodels = c(L ~ lag1_L + lag1_A,
                                  A ~ lag1_L + L + lag1_A))
  censor_name <- 'C'
  censor_model <- C ~ L

  expect_no_error(
    res_censor <- gformula(obs_data = censor_data, id = 'id',
                          time_name = 't0', covnames = covnames,
                          outcome_name = 'Y', outcome_type = 'survival',
                          censor_name = censor_name, censor_model = censor_model,
                          covtypes = covtypes,
                          covparams = covparams, ymodel = ymodel,
                          histories = histories, histvars = histvars,
                          seed = 1234, sim_data_b = T)
  )
})


make_survey_survival_inputs <- function(n_ids = 120, unit_weights = FALSE){
  id <- 'id'
  time_points <- 7
  time_name <- 't0'
  covnames <- c('L1', 'L2', 'A')
  outcome_name <- 'Y'
  outcome_type <- 'survival'
  covtypes <- c('binary', 'bounded normal', 'binary')
  histories <- c(lagged, lagavg)
  histvars <- list(c('A', 'L1', 'L2'), c('L1', 'L2'))
  covparams <- list(covmodels = c(L1 ~ lag1_A + lag_cumavg1_L1 + lag_cumavg1_L2 +
                                    L3 + t0,
                                  L2 ~ lag1_A + lag_cumavg1_L1 +
                                    lag_cumavg1_L2 + L3 + t0,
                                  A ~ lag1_A + L1 + L2 + lag_cumavg1_L1 +
                                    lag_cumavg1_L2 + L3 + t0))
  ymodel <- Y ~ A + L1 + L2 + L3 + lag1_A + lag1_L1 + lag1_L2 + t0

  ids_keep <- unique(basicdata_nocomp$id)[1:n_ids]
  survey_data <- data.table::copy(basicdata_nocomp[id %in% ids_keep])
  survey_data[, 'sw' := if (unit_weights) 1 else 1 + (id %% 5)]
  survey_data[, 'psu' := id %% 30]
  survey_data[, 'strata' := id %% 3]

  list(
    obs_data = survey_data, id = id,
    time_points = time_points,
    time_name = time_name, covnames = covnames,
    outcome_name = outcome_name,
    outcome_type = outcome_type, covtypes = covtypes,
    covparams = covparams, ymodel = ymodel,
    intervention1.A = list(static, rep(0, time_points)),
    int_descript = 'Never treat',
    histories = histories, histvars = histvars,
    basecovs = c('L3'), nsimul = n_ids
  )
}


test_that("Unit survey weights reproduce unweighted survival estimates", {
  gform_args <- make_survey_survival_inputs(unit_weights = TRUE)

  expect_no_error(
    gform_unweighted <- do.call(gformula, c(gform_args, list(
      seed = 1234, sim_data_b = TRUE
    )))
  )
  expect_no_error(
    gform_unit_weighted <- do.call(gformula, c(gform_args, list(
      seed = 1234, weight_name = 'sw', sim_data_b = TRUE
    )))
  )

  expect_equal(
    gform_unit_weighted$result[['g-form risk']],
    gform_unweighted$result[['g-form risk']],
    tolerance = 1e-10
  )
  expect_equal(
    gform_unit_weighted$result[['Risk ratio']],
    gform_unweighted$result[['Risk ratio']],
    tolerance = 1e-10
  )
  expect_equal(
    gform_unit_weighted$result[['Risk difference']],
    gform_unweighted$result[['Risk difference']],
    tolerance = 1e-10
  )
  expect_equal(
    gform_unit_weighted$result[['Survey weighted NP risk']],
    gform_unweighted$result[['NP Risk']],
    tolerance = 1e-10
  )
})


test_that("Survey weights are used in model fitting and marginal summaries", {
  gform_args <- make_survey_survival_inputs()

  expect_no_error(
    gform_weighted <- do.call(gformula, c(gform_args, list(
      seed = 1234, weight_name = 'sw',
      model_fits = TRUE, sim_data_b = TRUE
    )))
  )
  expect_equal(gform_weighted$weight_name, 'sw')
  expect_true(any(gform_weighted$fits$L1$prior.weights != 1))

  nat_pool <- gform_weighted$sim_data$`Natural course`
  manual_risk <- nat_pool[, stats::weighted.mean(poprisk, sw, na.rm = TRUE), by = t0]$V1
  result_risk <- gform_weighted$result[gform_weighted$result[['Interv.']] == 0][['g-form risk']]
  expect_equal(unname(result_risk), unname(manual_risk), tolerance = 1e-12)

  int_pool <- gform_weighted$sim_data$`Never treat`
  manual_average_percent <- 100 * stats::weighted.mean(
    int_pool$intervened,
    int_pool$sw,
    na.rm = TRUE
  )
  manual_percent <- int_pool[, .(
    intervened = any(intervened, na.rm = TRUE),
    sw = sw[1]
  ), by = id][, 100 * stats::weighted.mean(intervened, sw, na.rm = TRUE)]
  last_int_row <- gform_weighted$result[
    gform_weighted$result[['Interv.']] == 1 &
      gform_weighted$result[['k']] == max(gform_weighted$result[['k']])
  ]
  expect_equal(last_int_row[['Aver % Intervened On']], manual_average_percent)
  expect_equal(last_int_row[['% Intervened On']], manual_percent)
})


test_that("Survey-weighted bootstrap returns uncertainty estimates", {
  gform_args <- make_survey_survival_inputs()

  expect_no_error(
    gform_boot <- do.call(gformula, c(gform_args, list(
      seed = 4321, weight_name = 'sw',
      nsamples = 3, show_progress = FALSE,
      bootstrap_type = 'psu', psu_name = 'psu',
      strata_name = 'strata'
    )))
  )
  expect_equal(gform_boot$bootstrap_type, 'psu')
  expect_equal(gform_boot$psu_name, 'psu')
  expect_equal(gform_boot$strata_name, 'strata')
  expect_true(all(c('Risk SE', 'Risk lower 95% CI', 'Risk upper 95% CI') %in%
                    names(gform_boot$result)))
  expect_true(any(!is.na(gform_boot$result[['Risk SE']])))
})


test_that("Custom outcome fit functions can receive survey weights", {
  gform_args <- make_survey_survival_inputs()
  legacy_fit <- function(ymodel, obs_data){
    list(model = ymodel, n = nrow(obs_data))
  }
  ymodel_fit_custom <- function(ymodel, obs_data, weights = NULL,
                                weight_name = NULL){
    fit <- do.call(stats::glm, args = list(
      formula = ymodel, family = stats::quasibinomial(),
      data = obs_data, weights = weights, y = TRUE
    ))
    fit$weights_passed <- weights
    fit$weight_name_passed <- weight_name
    fit$rmse <- sqrt(stats::weighted.mean(
      (fit$y - stats::fitted(fit))^2,
      if (is.null(weights)) rep(1, length(fit$y)) else weights,
      na.rm = TRUE
    ))
    fit$stderr <- stats::coefficients(summary(fit))[, "Std. Error"]
    fit$vcov <- stats::vcov(fit)
    fit
  }
  ymodel_predict_custom <- function(fit, newdf){
    as.numeric(stats::predict(fit, type = 'response', newdata = newdf))
  }

  expect_equal(
    call_custom_fit(
      fit_fun = legacy_fit,
      args = list(ymodel = gform_args$ymodel, obs_data = gform_args$obs_data),
      weight_name = 'sw'
    )$n,
    nrow(gform_args$obs_data)
  )

  expect_no_error(
    gform_custom <- do.call(gformula, c(gform_args, list(
      seed = 1234, weight_name = 'sw',
      ymodel_fit_custom = ymodel_fit_custom,
      ymodel_predict_custom = ymodel_predict_custom,
      model_fits = TRUE
    )))
  )

  expect_equal(gform_custom$fits$Y$weight_name_passed, 'sw')
  expect_equal(
    unname(gform_custom$fits$Y$weights_passed),
    unname(gform_custom$fits$Y$prior.weights)
  )
  expect_true(any(gform_custom$fits$Y$weights_passed != 1))
})


test_that("Survey weight validation catches design problems", {
  gform_args <- make_survey_survival_inputs()
  bad_weights_args <- gform_args
  bad_weights_args$obs_data <- data.table::copy(gform_args$obs_data)
  first_id <- bad_weights_args$obs_data[[bad_weights_args$id]][1]
  bad_weights_args$obs_data[
    get(bad_weights_args$id) == first_id &
      get(bad_weights_args$time_name) == 1,
    sw := sw + 1
  ]

  expect_error(
    do.call(gformula, c(bad_weights_args, list(
      seed = 1234, weight_name = 'sw'
    ))),
    "constant within each individual"
  )
  expect_error(
    do.call(gformula, c(gform_args, list(
      seed = 1234, weight_name = 'sw',
      nsamples = 1, bootstrap_type = 'psu'
    ))),
    "psu_name"
  )
})
