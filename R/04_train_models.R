suppressPackageStartupMessages({
  library(tidymodels)
  library(tidyverse)
})
source("R/config.R")
source("R/utils.R")

set.seed(SEED)

historical <- readRDS(
  file.path(PROCESSED_DIR, "historical_junior_dataset.rds")
)
feature_map <- readRDS(
  file.path(PROCESSED_DIR, "position_feature_map.rds")
)

available_engines <- c(
  glmnet = requireNamespace("glmnet", quietly = TRUE),
  rf = requireNamespace("ranger", quietly = TRUE),
  xgb = requireNamespace("xgboost", quietly = TRUE)
)
available_engines <- names(available_engines)[available_engines]
if (!length(available_engines)) {
  stop("Install at least one of glmnet, ranger, or xgboost.")
}
message("Available engines: ", paste(available_engines, collapse = ", "))

classification_specs <- function(multiclass = FALSE) {
  specs <- list()

  if ("glmnet" %in% available_engines) {
    specs$glmnet <- multinom_reg(
      penalty = tune(), mixture = tune()
    ) %>%
      set_engine("glmnet") %>%
      set_mode("classification")
  }

  if ("rf" %in% available_engines) {
    specs$rf <- rand_forest(
      trees = 900,
      mtry = tune(),
      min_n = tune()
    ) %>%
      set_engine(
        "ranger",
        probability = TRUE,
        importance = "permutation"
      ) %>%
      set_mode("classification")
  }

  if ("xgb" %in% available_engines) {
    specs$xgb <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune()
    ) %>%
      set_engine("xgboost") %>%
      set_mode("classification")
  }

  specs
}

regression_specs <- function() {
  specs <- list()

  if ("glmnet" %in% available_engines) {
    specs$glmnet <- linear_reg(
      penalty = tune(), mixture = tune()
    ) %>%
      set_engine("glmnet")
  }

  if ("rf" %in% available_engines) {
    specs$rf <- rand_forest(
      trees = 900,
      mtry = tune(),
      min_n = tune()
    ) %>%
      set_engine("ranger", importance = "permutation") %>%
      set_mode("regression")
  }

  if ("xgb" %in% available_engines) {
    specs$xgb <- boost_tree(
      trees = tune(),
      tree_depth = tune(),
      learn_rate = tune(),
      min_n = tune(),
      loss_reduction = tune(),
      sample_size = tune(),
      mtry = tune()
    ) %>%
      set_engine("xgboost") %>%
      set_mode("regression")
  }

  specs
}

make_recipe <- function(formula, data, upsample = FALSE) {
  rec <- recipe(formula, data = data) %>%
    step_novel(all_nominal_predictors()) %>%
    step_unknown(all_nominal_predictors()) %>%
    step_dummy(all_nominal_predictors()) %>%
    step_impute_median(all_numeric_predictors()) %>%
    step_nzv(all_predictors()) %>%
    step_normalize(all_numeric_predictors())

  if (upsample && requireNamespace("themis", quietly = TRUE)) {
    rec <- rec %>% themis::step_upsample(all_outcomes(), over_ratio = 0.85)
  }

  rec
}

best_threshold <- function(truth, prob) {
  truth <- factor(truth, levels = c("no", "yes"))
  grid <- seq(0.08, 0.80, by = 0.01)

  scores <- map_dfr(grid, function(threshold) {
    pred <- factor(
      if_else(prob >= threshold, "yes", "no"),
      levels = c("no", "yes")
    )
    tibble(
      threshold = threshold,
      f1 = suppressWarnings(
        yardstick::f_meas_vec(
          truth, pred, event_level = "second",
          estimator = "binary"
        )
      ),
      balanced_accuracy = suppressWarnings(
        yardstick::bal_accuracy_vec(truth, pred)
      )
    )
  }) %>%
    mutate(
      score = replace_na(f1, 0) + replace_na(balanced_accuracy, 0)
    ) %>%
    arrange(desc(score), abs(threshold - 0.35))

  scores$threshold[[1]]
}

fit_one_engine <- function(
    engine_name,
    spec,
    rec,
    folds,
    train_data,
    metrics,
    primary_metric,
    grid_size = TUNING_GRID_SIZE) {

  wf <- workflow() %>%
    add_recipe(rec) %>%
    add_model(spec)

  params <- extract_parameter_set_dials(wf) %>%
    dials::finalize(train_data)

  if (nrow(params)) {
    grid <- dials::grid_space_filling(params, size = grid_size)
    tuned <- tune_grid(
      wf,
      resamples = folds,
      grid = grid,
      metrics = metrics,
      control = control_grid(
        save_pred = TRUE,
        save_workflow = TRUE,
        verbose = FALSE
      )
    )
    best <- select_best(tuned, metric = primary_metric)
    final_wf <- finalize_workflow(wf, best)
    fit_obj <- fit(final_wf, data = train_data)
    best_value <- collect_metrics(tuned) %>%
      filter(.metric == primary_metric) %>%
      arrange(mean) %>%
      slice(1) %>%
      pull(mean)
    if (!length(best_value)) best_value <- NA_real_
  } else {
    tuned <- NULL
    fit_obj <- fit(wf, data = train_data)
    best_value <- NA_real_
  }

  list(
    engine = engine_name,
    fit = fit_obj,
    tuning = tuned,
    cv_metric = best_value
  )
}

fit_engine_set <- function(
    specs,
    rec,
    folds,
    train_data,
    metrics,
    primary_metric,
    grid_size = TUNING_GRID_SIZE) {

  results <- imap(specs, function(spec, engine_name) {
    message("  tuning ", engine_name, " ...")
    tryCatch(
      fit_one_engine(
        engine_name, spec, rec, folds,
        train_data, metrics, primary_metric, grid_size
      ),
      error = function(e) {
        warning(engine_name, " skipped: ", conditionMessage(e))
        NULL
      }
    )
  }) %>% compact()

  fits <- map(results, "fit")
  names(fits) <- map_chr(results, "engine")

  metric_values <- map_dbl(results, function(x) {
    val <- x$cv_metric
    if (!length(val) || !is.finite(val)) NA_real_ else val
  })

  if (all(!is.finite(metric_values))) {
    weights <- rep(1 / max(1, length(fits)), length(fits))
  } else {
    replacement <- max(metric_values[is.finite(metric_values)], na.rm = TRUE)
    metric_values[!is.finite(metric_values)] <- replacement
    inv <- 1 / pmax(metric_values, 1e-6)
    weights <- inv / sum(inv)
  }
  names(weights) <- names(fits)

  list(
    fits = fits,
    weights = weights,
    tuning = map(results, "tuning")
  )
}

extract_oof_threshold <- function(ensemble, positive = "yes") {
  engine_thresholds <- map_dbl(seq_along(ensemble$tuning), function(i) {
    tuned <- ensemble$tuning[[i]]
    if (is.null(tuned)) return(NA_real_)
    pred <- tryCatch(collect_predictions(tuned), error = function(e) tibble())
    wanted <- paste0(".pred_", positive)
    truth_col <- intersect(
      c("drafted", "ordinal_target", ".outcome"),
      names(pred)
    )
    if (!nrow(pred) || !wanted %in% names(pred) || !length(truth_col)) {
      return(NA_real_)
    }
    best_threshold(pred[[truth_col[[1]]]], pred[[wanted]])
  })
  engine_thresholds <- engine_thresholds[is.finite(engine_thresholds)]
  if (!length(engine_thresholds)) return(DEFAULT_DRAFT_PROBABILITY_THRESHOLD)
  median(engine_thresholds)
}

fit_position_classifier <- function(pos) {
  d <- historical %>%
    filter(position == pos) %>%
    mutate(
      drafted = factor(
        if_else(drafted == 1L, "yes", "no"),
        levels = c("no", "yes")
      )
    )

  features <- intersect(feature_map[[pos]], names(d))
  features <- select_usable_features(d, features)

  if (
    nrow(d) < MIN_POSITION_ROWS ||
      sum(d$drafted == "yes") < MIN_DRAFTED_ROWS ||
      length(features) < 2L
  ) {
    message(pos, ": skipped; insufficient rows, drafted outcomes, or features.")
    return(NULL)
  }

  model_data <- d %>%
    select(drafted, junior_year, all_of(features))

  split <- make_temporal_split(model_data)
  if (
    n_distinct(split$train$drafted) < 2L ||
      sum(split$train$drafted == "yes") < MIN_DRAFTED_ROWS
  ) {
    message(pos, ": skipped; temporal training split is insufficient.")
    return(NULL)
  }

  train <- split$train %>% select(-junior_year)
  folds <- safe_vfold(split$train, "drafted")
  rec <- make_recipe(drafted ~ ., train, upsample = FALSE)

  ensemble <- fit_engine_set(
    classification_specs(FALSE),
    rec,
    folds,
    train,
    metric_set(pr_auc, roc_auc, mn_log_loss),
    "mn_log_loss"
  )
  if (!length(ensemble$fits)) return(NULL)

  threshold <- extract_oof_threshold(ensemble)

  out <- list(
    position = pos,
    features = features,
    holdout_years = split$holdout,
    train = split$train,
    test = split$test,
    classifier = ensemble$fits,
    weights = ensemble$weights,
    threshold = threshold,
    tuning = ensemble$tuning
  )

  saveRDS(
    out,
    file.path(MODEL_DIR, paste0(pos, "_draft_classifier.rds"))
  )
  message(
    pos, ": saved ", length(out$classifier),
    " classifier(s); threshold=", round(threshold, 2)
  )
  out
}

if (Sys.getenv("PAA_DEFINE_TRAINING_FUNCTIONS_ONLY") != "1") {
  position_models <- setNames(vector("list", length(POSITIONS)), POSITIONS)
  for (pos in POSITIONS) {
    message("\nPosition classifier: ", pos)
    position_models[[pos]] <- tryCatch(
      fit_position_classifier(pos),
      error = function(e) {
        warning(pos, " failed: ", conditionMessage(e))
        NULL
      }
    )
  }
  position_models <- compact(position_models)
  saveRDS(
    position_models,
    file.path(MODEL_DIR, "all_position_classifiers.rds")
  )

all_candidate_round_features <- unique(unlist(feature_map, use.names = FALSE)) %>%
  intersect(names(historical))

round_source <- historical %>%
  filter(
    drafted == 1L,
    !is.na(round_tier),
    !is.na(draft_pick),
    !is.na(position)
  )

usable_round_features <- select_usable_features(
  round_source,
  all_candidate_round_features,
  max_missing = 0.55,
  max_features = 100L
)

round_data <- round_source %>%
  select(
    round_tier, draft_pick, junior_year, position,
    all_of(usable_round_features)
  )

if (nrow(round_data) < MIN_ROUND_ROWS) {
  stop("Insufficient drafted players for pooled round model.")
}

round_split <- make_temporal_split(round_data)
round_train <- round_split$train %>%
  select(-draft_pick, -junior_year)
round_folds <- safe_vfold(round_split$train, "round_tier")
round_recipe <- make_recipe(
  round_tier ~ .,
  round_train,
  upsample = TRUE
)

message("\nPooled direct round-tier classifier")
round_ensemble <- fit_engine_set(
  classification_specs(TRUE),
  round_recipe,
  round_folds,
  round_train,
  metric_set(mn_log_loss, accuracy, bal_accuracy),
  "mn_log_loss"
)
if (!length(round_ensemble$fits)) {
  stop("No direct pooled round-tier model completed.")
}

round_model <- list(
  features = usable_round_features,
  holdout_years = round_split$holdout,
  train = round_split$train,
  test = round_split$test,
  classifier = round_ensemble$fits,
  weights = round_ensemble$weights,
  tuning = round_ensemble$tuning,
  levels = ROUND_TIER_LEVELS
)
saveRDS(
  round_model,
  file.path(MODEL_DIR, "pooled_round_tier_model.rds")
)

ordinal_definitions <- list(
  r1 = c("R1"),
  r3_or_better = c("R1", "R2_3"),
  r5_or_better = c("R1", "R2_3", "R4_5")
)

ordinal_models <- imap(ordinal_definitions, function(positive_levels, label) {
  message("\nPooled ordinal boundary: ", label)

  d <- round_data %>%
    mutate(
      ordinal_target = factor(
        if_else(round_tier %in% positive_levels, "yes", "no"),
        levels = c("no", "yes")
      )
    )

  split <- make_temporal_split(d)
  train <- split$train %>%
    select(
      ordinal_target, position,
      all_of(usable_round_features)
    )
  folds <- safe_vfold(split$train, "ordinal_target")
  rec <- make_recipe(ordinal_target ~ ., train, upsample = TRUE)

  ensemble <- fit_engine_set(
    classification_specs(FALSE),
    rec,
    folds,
    train,
    metric_set(mn_log_loss, pr_auc, roc_auc),
    "mn_log_loss"
  )
  if (!length(ensemble$fits)) return(NULL)

  list(
    label = label,
    positive_levels = positive_levels,
    features = usable_round_features,
    holdout_years = split$holdout,
    train = split$train,
    test = split$test,
    classifier = ensemble$fits,
    weights = ensemble$weights,
    tuning = ensemble$tuning
  )
}) %>% compact()

if (length(ordinal_models) != 3L) {
  warning("Not all ordinal boundaries trained successfully.")
}
saveRDS(
  ordinal_models,
  file.path(MODEL_DIR, "pooled_ordinal_round_models.rds")
)

pick_data <- round_data %>%
  mutate(log_draft_pick = log1p(draft_pick))
pick_split <- make_temporal_split(pick_data)
pick_train <- pick_split$train %>%
  select(
    log_draft_pick, position,
    all_of(usable_round_features)
  )
pick_folds <- vfold_cv(pick_split$train, v = CV_FOLDS)
pick_recipe <- make_recipe(
  log_draft_pick ~ .,
  pick_train,
  upsample = FALSE
)

message("\nExploratory pooled exact-pick regressor")
pick_ensemble <- fit_engine_set(
  regression_specs(),
  pick_recipe,
  pick_folds,
  pick_train,
  metric_set(mae, rmse),
  "mae"
)

if (length(pick_ensemble$fits)) {
  pick_model <- list(
    features = usable_round_features,
    holdout_years = pick_split$holdout,
    train = pick_split$train,
    test = pick_split$test,
    regressor = pick_ensemble$fits,
    weights = pick_ensemble$weights,
    tuning = pick_ensemble$tuning
  )
  saveRDS(
    pick_model,
    file.path(MODEL_DIR, "pooled_exact_pick_model.rds")
  )
}

  message("\nTraining complete.")
}
