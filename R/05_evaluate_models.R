suppressPackageStartupMessages({
  library(tidymodels)
  library(tidyverse)
})
source("R/config.R")
source("R/utils.R")

predict_binary_ensemble <- function(obj, new_data) {
  values <- lapply(obj$classifier, function(fit_obj) {
    pred <- predict(fit_obj, new_data, type = "prob")
    pred$.pred_yes
  })
  weighted_row_mean(values, obj$weights)
}

predict_multiclass_ensemble <- function(obj, new_data) {
  tables <- lapply(obj$classifier, function(fit_obj) {
    predict(fit_obj, new_data, type = "prob")
  })
  weights <- obj$weights
  if (is.null(weights) || length(weights) != length(tables)) {
    weights <- rep(1 / length(tables), length(tables))
  }

  out <- map_dfc(ROUND_TIER_LEVELS, function(level) {
    col <- paste0(".pred_", level)
    values <- lapply(tables, function(x) x[[col]])
    tibble(!!col := weighted_row_mean(values, weights))
  })
  normalize_probability_matrix(out, ROUND_TIER_LEVELS)
}

ordinal_probability_matrix <- function(models, new_data) {
  probs <- list()
  for (nm in names(models)) {
    obj <- models[[nm]]
    x <- new_data %>%
      select(position, all_of(obj$features))
    probs[[nm]] <- predict_binary_ensemble(obj, x)
  }

  p1 <- probs$r1
  p3 <- probs$r3_or_better
  p5 <- probs$r5_or_better

  cumulative <- t(apply(cbind(p1, p3, p5), 1, function(x) {
    x <- pmin(1, pmax(0, x))
    cummax(x)
  }))

  p1 <- cumulative[, 1]
  p3 <- cumulative[, 2]
  p5 <- cumulative[, 3]

  m <- cbind(
    R1 = p1,
    R2_3 = pmax(0, p3 - p1),
    R4_5 = pmax(0, p5 - p3),
    R6_7 = pmax(0, 1 - p5)
  )
  m / rowSums(m)
}

multiclass_log_loss <- function(truth, prob_matrix) {
  truth_index <- match(as.character(truth), colnames(prob_matrix))
  good <- !is.na(truth_index)
  p <- rep(NA_real_, length(truth_index))
  p[good] <- prob_matrix[cbind(which(good), truth_index[good])]
  mean(-log(pmax(p[good], 1e-15)), na.rm = TRUE)
}

round_metric_row <- function(method, truth, prob_matrix, years) {
  pred <- factor(
    colnames(prob_matrix)[max.col(prob_matrix, ties.method = "first")],
    levels = ROUND_TIER_LEVELS
  )
  truth <- factor(truth, levels = ROUND_TIER_LEVELS)
  tier_num <- setNames(seq_along(ROUND_TIER_LEVELS), ROUND_TIER_LEVELS)
  truth_num <- unname(tier_num[as.character(truth)])
  pred_num <- unname(tier_num[as.character(pred)])

  tibble(
    method = method,
    holdout_years = paste(years, collapse = ","),
    n = length(truth),
    exact_tier_accuracy = mean(truth == pred, na.rm = TRUE),
    within_one_tier_accuracy = mean(abs(truth_num - pred_num) <= 1L, na.rm = TRUE),
    ordinal_mae = mean(abs(truth_num - pred_num), na.rm = TRUE),
    balanced_accuracy = suppressWarnings(bal_accuracy_vec(truth, pred)),
    macro_f1 = suppressWarnings(f_meas_vec(truth, pred, estimator = "macro")),
    quadratic_weighted_kappa = suppressWarnings(
      kap_vec(truth, pred, weighting = "quadratic")
    ),
    multiclass_log_loss = multiclass_log_loss(truth, prob_matrix)
  )
}

position_files <- list.files(
  MODEL_DIR,
  pattern = "_draft_classifier\\.rds$",
  full.names = TRUE
)

position_results <- map(position_files, function(path) {
  obj <- readRDS(path)
  test <- obj$test
  x <- test %>% select(-drafted, -junior_year)
  prob <- predict_binary_ensemble(obj, x)
  truth <- test$drafted
  threshold <- obj$threshold %||% DEFAULT_DRAFT_PROBABILITY_THRESHOLD
  pred <- factor(
    if_else(prob >= threshold, "yes", "no"),
    levels = c("no", "yes")
  )
  truth_num <- as.integer(truth == "yes")

  list(
    metrics = tibble(
      position = obj$position,
      holdout_years = paste(obj$holdout_years, collapse = ","),
      n = nrow(test),
      drafted = sum(truth == "yes"),
      draft_rate = mean(truth == "yes"),
      threshold = threshold,
      mean_predicted_probability = mean(prob),
      pr_auc = suppressWarnings(
        pr_auc_vec(truth, prob, event_level = "second")
      ),
      roc_auc = suppressWarnings(
        roc_auc_vec(truth, prob, event_level = "second")
      ),
      brier = mean((prob - truth_num) ^ 2),
      balanced_accuracy = suppressWarnings(
        bal_accuracy_vec(truth, pred)
      ),
      f1 = suppressWarnings(
        f_meas_vec(
          truth, pred,
          event_level = "second",
          estimator = "binary"
        )
      )
    ),
    predictions = test %>%
      transmute(
        position = obj$position,
        junior_year,
        drafted = truth_num,
        draft_probability = prob,
        threshold,
        predicted_drafted = as.integer(prob >= threshold)
      )
  )
})

write_csv(
  map_dfr(position_results, "metrics"),
  file.path(OUTPUT_DIR, "draft_probability_holdout_metrics.csv")
)
write_csv(
  map_dfr(position_results, "predictions"),
  file.path(OUTPUT_DIR, "draft_probability_holdout_predictions.csv")
)

round_obj <- readRDS(
  file.path(MODEL_DIR, "pooled_round_tier_model.rds")
)
ordinal_models <- readRDS(
  file.path(MODEL_DIR, "pooled_ordinal_round_models.rds")
)

round_test <- round_obj$test
direct_new <- round_test %>%
  select(position, all_of(round_obj$features))
direct_prob <- predict_multiclass_ensemble(round_obj, direct_new)
ordinal_prob <- ordinal_probability_matrix(ordinal_models, round_test)

blend_prob <- ORDINAL_BLEND_WEIGHT * ordinal_prob +
  (1 - ORDINAL_BLEND_WEIGHT) * direct_prob
blend_prob <- blend_prob / rowSums(blend_prob)

truth <- factor(round_test$round_tier, levels = ROUND_TIER_LEVELS)

round_metrics <- bind_rows(
  round_metric_row(
    "direct_multiclass", truth, direct_prob, round_obj$holdout_years
  ),
  round_metric_row(
    "cumulative_ordinal", truth, ordinal_prob, round_obj$holdout_years
  ),
  round_metric_row(
    "blended", truth, blend_prob, round_obj$holdout_years
  )
)

tabfm_path <- file.path(
  PROCESSED_DIR, "tabfm", "round_holdout_probabilities.csv"
)
if (RUN_TABFM && file.exists(tabfm_path)) {
  tabfm_df <- read_csv(tabfm_path, show_col_types = FALSE) %>%
    arrange(.tabfm_row_id)
  if (nrow(tabfm_df) != nrow(round_test)) {
    stop("TabFM holdout probabilities do not align with the holdout rows.")
  }
  tabfm_prob <- as.matrix(
    tabfm_df[, paste0("prob_", ROUND_TIER_LEVELS)]
  )
  colnames(tabfm_prob) <- ROUND_TIER_LEVELS
  augmented_prob <- TABFM_BLEND_WEIGHT * tabfm_prob +
    (1 - TABFM_BLEND_WEIGHT) * blend_prob
  augmented_prob <- augmented_prob / rowSums(augmented_prob)
  round_metrics <- bind_rows(
    round_metrics,
    round_metric_row("tabfm", truth, tabfm_prob, round_obj$holdout_years),
    round_metric_row(
      "tabfm_augmented", truth, augmented_prob, round_obj$holdout_years
    )
  )
  blend_prob <- augmented_prob
}

majority_level <- names(sort(table(round_obj$train$round_tier), decreasing = TRUE))[1]
majority_prob <- matrix(
  0,
  nrow = nrow(round_test),
  ncol = length(ROUND_TIER_LEVELS),
  dimnames = list(NULL, ROUND_TIER_LEVELS)
)
majority_prob[, majority_level] <- 1
round_metrics <- bind_rows(
  round_metrics,
  round_metric_row(
    "majority_baseline", truth, majority_prob, round_obj$holdout_years
  )
)

write_csv(
  round_metrics,
  file.path(OUTPUT_DIR, "round_tier_holdout_metrics.csv")
)

predicted_blend <- factor(
  ROUND_TIER_LEVELS[max.col(blend_prob, ties.method = "first")],
  levels = ROUND_TIER_LEVELS
)
tier_num <- setNames(seq_along(ROUND_TIER_LEVELS), ROUND_TIER_LEVELS)

round_predictions <- bind_cols(
  round_test %>%
    select(junior_year, position, draft_pick, round_tier),
  as_tibble(blend_prob, .name_repair = ~paste0("prob_", .x))
) %>%
  mutate(
    predicted_round_tier = predicted_blend,
    exact_tier = as.integer(round_tier == predicted_round_tier),
    within_one_tier = as.integer(
      abs(
        tier_num[as.character(round_tier)] -
          tier_num[as.character(predicted_round_tier)]
      ) <= 1L
    )
  )

write_csv(
  round_predictions,
  file.path(OUTPUT_DIR, "round_tier_holdout_predictions.csv")
)

confusion <- conf_mat(
  round_predictions,
  truth = round_tier,
  estimate = predicted_round_tier
)
write_csv(
  as_tibble(confusion$table),
  file.path(OUTPUT_DIR, "round_tier_confusion_matrix.csv")
)

year_metrics <- round_predictions %>%
  group_by(junior_year) %>%
  group_modify(~{
    truth_y <- factor(.x$round_tier, levels = ROUND_TIER_LEVELS)
    pred_y <- factor(.x$predicted_round_tier, levels = ROUND_TIER_LEVELS)
    tibble(
      n = nrow(.x),
      exact_tier_accuracy = mean(truth_y == pred_y),
      macro_f1 = suppressWarnings(
        f_meas_vec(truth_y, pred_y, estimator = "macro")
      ),
      balanced_accuracy = suppressWarnings(
        bal_accuracy_vec(truth_y, pred_y)
      )
    )
  }) %>%
  ungroup()
write_csv(
  year_metrics,
  file.path(OUTPUT_DIR, "round_tier_metrics_by_year.csv")
)

pick_path <- file.path(MODEL_DIR, "pooled_exact_pick_model.rds")
if (file.exists(pick_path)) {
  pick_obj <- readRDS(pick_path)
  pick_test <- pick_obj$test
  pick_new <- pick_test %>%
    select(position, all_of(pick_obj$features))

  values <- lapply(pick_obj$regressor, function(fit_obj) {
    pmin(
      257,
      pmax(
        1,
        expm1(predict(fit_obj, pick_new)$.pred)
      )
    )
  })
  pick_pred <- weighted_row_mean(values, pick_obj$weights)

  pick_predictions <- pick_test %>%
    transmute(
      junior_year,
      position,
      draft_pick,
      exploratory_exact_pick = pick_pred,
      absolute_error = abs(draft_pick - exploratory_exact_pick)
    )

  write_csv(
    pick_predictions,
    file.path(OUTPUT_DIR, "exploratory_pick_holdout_predictions.csv")
  )
  write_csv(
    tibble(
      holdout_years = paste(pick_obj$holdout_years, collapse = ","),
      n = nrow(pick_predictions),
      mae = mean(pick_predictions$absolute_error),
      rmse = sqrt(mean(
        (pick_predictions$draft_pick -
           pick_predictions$exploratory_exact_pick) ^ 2
      )),
      spearman = cor(
        pick_predictions$draft_pick,
        pick_predictions$exploratory_exact_pick,
        method = "spearman",
        use = "complete.obs"
      )
    ),
    file.path(OUTPUT_DIR, "exploratory_pick_holdout_metrics.csv")
  )
}

message("Evaluation complete.")
