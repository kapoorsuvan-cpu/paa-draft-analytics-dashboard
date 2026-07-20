suppressPackageStartupMessages({
  library(tidymodels)
  library(tidyverse)
})
source("R/config.R")
source("R/utils.R")

# Experimental one-year-earlier version of the rising-senior pipeline. The
# internal `junior_year` name is retained so the existing temporal modeling
# functions can be reused without changing their validated behavior; in this
# script it always means the sophomore-season cutoff year.
EXPERIMENT_POSITIONS <- c(
  "QB", "RB", "WR", "TE", "EDGE", "IDL", "ILB", "CB", "SAF"
)
SOPHOMORE_HISTORICAL_YEARS <- 2014:2023
SOPHOMORE_CURRENT_YEAR <- 2025L
SOPHOMORE_MODEL_DIR <- file.path(MODEL_DIR, "sophomore")
SOPHOMORE_TABFM_DIR <- file.path(PROCESSED_DIR, "tabfm_sophomore")
dir.create(SOPHOMORE_MODEL_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(SOPHOMORE_TABFM_DIR, recursive = TRUE, showWarnings = FALSE)

# Load the established preparation helpers without rebuilding the senior data.
Sys.setenv(PAA_DEFINE_DATA_FUNCTIONS_ONLY = "1")
source("R/02_build_junior_dataset.R", encoding = "UTF-8")
Sys.unsetenv("PAA_DEFINE_DATA_FUNCTIONS_ONLY")

match_sophomore_draft_outcomes <- function(out) {
  base <- out %>%
    mutate(row_id = row_number()) %>%
    select(row_id, player_clean, school_clean, position, junior_year)

  candidates <- base %>%
    left_join(draft, by = "player_clean", relationship = "many-to-many") %>%
    filter(
      !is.na(draft_year),
      between(draft_year, junior_year + 1L, junior_year + 3L)
    ) %>%
    mutate(
      school_match = as.integer(
        !is.na(college_clean) & college_clean != "" &
          college_clean == school_clean
      ),
      position_match = as.integer(
        !is.na(nfl_position) & nfl_position == position
      ),
      match_score = 3 * school_match + 2 * position_match -
        0.01 * abs(draft_year - (junior_year + 2L))
    ) %>%
    group_by(row_id) %>%
    arrange(desc(match_score), draft_year, draft_pick, .by_group = TRUE) %>%
    slice_head(n = 1L) %>%
    ungroup() %>%
    transmute(
      row_id,
      draft_year,
      draft_pick,
      draft_round,
      draft_match_method = case_when(
        school_match == 1L & position_match == 1L ~ "name_school_position",
        position_match == 1L ~ "name_position",
        school_match == 1L ~ "name_school",
        TRUE ~ "unique_name_window"
      )
    )

  base %>%
    left_join(candidates, by = "row_id") %>%
    mutate(
      drafted = as.integer(!is.na(draft_pick)),
      overall_pick = if_else(
        drafted == 1L,
        as.numeric(draft_pick),
        as.numeric(UNDRAFTED_PICK)
      )
    ) %>%
    select(
      row_id, draft_year, draft_pick, draft_round,
      drafted, overall_pick, draft_match_method
    )
}

add_sophomore_percentile_features <- function(df) {
  candidates <- names(df)[
    str_detect(
      names(df),
      "^so_(pass_ypa|completion_pct|pass_td_rate|pass_int_rate|rush_ypc|rec_ypr|rush_share|rush_yds_share|rec_yds_share|rec_td_share|fg_pct|punt_avg|passing_yds|passing_td|rushing_yds|rushing_td|receiving_yds|receiving_td|defensive_solo|defensive_tot|defensive_tfl|defensive_sacks|defensive_qb_hur|defensive_pd|interceptions_int)$"
    )
  ]

  usable <- candidates[vapply(candidates, function(nm) {
    x <- suppressWarnings(as.numeric(df[[nm]]))
    sum(is.finite(x)) >= 40L && n_distinct(x[is.finite(x)]) >= 5L
  }, logical(1))]

  if (!length(usable)) return(df)

  df %>%
    group_by(junior_year, position) %>%
    mutate(
      across(
        all_of(usable),
        ~if_else(
          is.finite(as.numeric(.x)),
          percent_rank(as.numeric(.x)),
          NA_real_
        ),
        .names = "{.col}_pct"
      ),
      recruit_rating_pct = if_else(
        is.finite(recruit_rating), percent_rank(recruit_rating), NA_real_
      ),
      recruit_rank_pct = if_else(
        is.finite(recruit_rank), 1 - percent_rank(recruit_rank), NA_real_
      ),
      height_position_pct = if_else(
        is.finite(height_inches), percent_rank(height_inches), NA_real_
      ),
      weight_position_pct = if_else(
        is.finite(weight_lbs), percent_rank(weight_lbs), NA_real_
      )
    ) %>%
    ungroup()
}

build_sophomore_rows <- function(years, add_outcomes = TRUE) {
  roster_rows <- rosters %>%
    filter(
      season %in% as.integer(years),
      class == "SO",
      position %in% EXPERIMENT_POSITIONS
    ) %>%
    arrange(season, athlete_id) %>%
    distinct(athlete_id, season, .keep_all = TRUE) %>%
    distinct(player_clean, school_clean, season, position, .keep_all = TRUE) %>%
    transmute(
      player_name = player,
      player_clean,
      school = team,
      school_clean,
      position,
      junior_year = season,
      athlete_id,
      height_inches,
      weight_lbs,
      bmi_proxy,
      headshot_url
    )

  out <- join_stats_with_fallback(
    roster_rows,
    stat_match_table(0L),
    "so_"
  )
  out <- attach_recruiting(out)

  so_numeric <- names(out)[
    str_starts(names(out), "so_") & map_lgl(out, is.numeric)
  ]

  out <- out %>%
    mutate(
      has_recruiting = as.integer(
        is.finite(recruit_rating) | is.finite(stars)
      ),
      has_sophomore_stats = as.integer(
        rowSums(across(all_of(so_numeric), ~replace_na(.x, 0) != 0)) > 0
      ),
      has_junior_stats = 0L,
      sophomore_feature_coverage = rowMeans(
        across(all_of(so_numeric), ~is.finite(as.numeric(.x)))
      ),
      junior_feature_coverage = 0,
      years_since_recruit = junior_year - recruit_year,
      power_conference = power_conference_flag(so_conference),
      transfer_or_school_mismatch = as.integer(
        !is.na(recruit_position) & !is.na(position) &
          recruit_position != position
      )
    )

  out <- attach_context_features(out)
  out <- add_sophomore_percentile_features(out)

  if (add_outcomes) {
    labels <- match_sophomore_draft_outcomes(out)
    out <- out %>%
      mutate(row_id = row_number()) %>%
      left_join(labels, by = "row_id") %>%
      select(-row_id) %>%
      mutate(
        drafted = coalesce(drafted, 0L),
        overall_pick = coalesce(overall_pick, as.numeric(UNDRAFTED_PICK)),
        round_tier = case_when(
          drafted == 0L ~ NA_character_,
          draft_round == 1L ~ "R1",
          draft_round %in% 2:3 ~ "R2_3",
          draft_round %in% 4:5 ~ "R4_5",
          draft_round %in% 6:7 ~ "R6_7",
          TRUE ~ NA_character_
        ),
        round_tier = factor(round_tier, levels = ROUND_TIER_LEVELS)
      )
  }

  out
}

message("\nBuilding sophomore-cutoff datasets")
sophomore_historical <- build_sophomore_rows(
  SOPHOMORE_HISTORICAL_YEARS,
  TRUE
)
sophomore_current <- build_sophomore_rows(SOPHOMORE_CURRENT_YEAR, FALSE)

# A player selected in 2026 is not a 2026 rising junior. The saved raw draft
# table can lag the completed draft, so refresh this exclusion at run time. Use
# name plus school or position so common names do not remove a different player.
drafted_2026_profiles <- tryCatch(
  nflreadr::load_draft_picks() %>%
    filter(season == SOPHOMORE_CURRENT_YEAR + 1L) %>%
    transmute(
      player_clean = clean_name(pfr_player_name),
      college_clean = clean_school(college),
      nfl_position = standardize_position(position)
    ) %>%
    distinct(),
  error = function(e) {
    warning("Live 2026 draft exclusions unavailable: ", conditionMessage(e))
    draft %>%
      filter(draft_year == SOPHOMORE_CURRENT_YEAR + 1L) %>%
      select(player_clean, college_clean, nfl_position) %>%
      distinct()
  }
)
drafted_name_school <- drafted_2026_profiles %>%
  filter(!is.na(college_clean), college_clean != "") %>%
  transmute(key = paste(player_clean, college_clean, sep = "|")) %>%
  pull(key)
drafted_name_position <- drafted_2026_profiles %>%
  filter(!is.na(nfl_position), nfl_position != "") %>%
  transmute(key = paste(player_clean, nfl_position, sep = "|")) %>%
  pull(key)
sophomore_current <- sophomore_current %>%
  filter(
    !(
      paste(player_clean, school_clean, sep = "|") %in% drafted_name_school |
        paste(player_clean, position, sep = "|") %in% drafted_name_position
    )
  )

write_rds(
  sophomore_historical,
  file.path(PROCESSED_DIR, "historical_sophomore_dataset.rds")
)
write_csv(
  sophomore_historical,
  file.path(PROCESSED_DIR, "historical_sophomore_dataset.csv")
)
write_rds(
  sophomore_current,
  file.path(PROCESSED_DIR, "current_rising_junior_feature_rows.rds")
)
write_csv(
  sophomore_current,
  file.path(PROCESSED_DIR, "current_rising_junior_feature_rows.csv")
)

message("Historical sophomore rows: ", nrow(sophomore_historical))
message(
  "Historical drafted labels: ",
  sum(sophomore_historical$drafted == 1L)
)
message("Current rising juniors: ", nrow(sophomore_current))

# Correlation analysis and the same data-quality feature selection used by the
# rising-senior pipeline.
blocked <- c(
  "draft_year", "draft_round", "draft_pick", "drafted",
  "overall_pick", "round_tier", "junior_year", "recruit_year",
  "athlete_id"
)

cor_one <- function(dat, feature) {
  x <- suppressWarnings(as.numeric(dat[[feature]]))
  y <- suppressWarnings(as.numeric(dat$overall_pick))
  ok <- is.finite(x) & is.finite(y)
  if (
    sum(ok) < 25L || n_distinct(x[ok]) < 3L ||
      sd(x[ok]) == 0 || sd(y[ok]) == 0
  ) return(NULL)

  tibble(
    feature = feature,
    n = sum(ok),
    nonzero_n = sum(x[ok] != 0),
    pearson = suppressWarnings(cor(x[ok], y[ok], method = "pearson")),
    spearman = suppressWarnings(cor(x[ok], y[ok], method = "spearman"))
  )
}

sophomore_quality <- map_dfr(EXPERIMENT_POSITIONS, function(pos) {
  d <- sophomore_historical %>% filter(position == pos)
  candidates <- candidate_features_for_position(d, pos) %>% setdiff(blocked)
  q <- feature_quality(d, candidates)
  if (!nrow(q)) return(tibble())
  q %>%
    mutate(
      position = pos,
      selected = feature %in% select_usable_features(d, candidates),
      .before = 1
    )
})

sophomore_correlations <- map_dfr(EXPERIMENT_POSITIONS, function(pos) {
  d <- sophomore_historical %>% filter(position == pos)
  candidates <- candidate_features_for_position(d, pos) %>% setdiff(blocked)
  usable <- select_usable_features(d, candidates)
  result <- map_dfr(usable, ~cor_one(d, .x))
  if (!nrow(result)) return(tibble())
  result %>%
    mutate(position = pos, abs_spearman = abs(spearman), .before = 1) %>%
    arrange(desc(abs_spearman))
})

sophomore_feature_map_table <- map_dfr(
  EXPERIMENT_POSITIONS,
  function(pos) {
    d <- sophomore_historical %>% filter(position == pos)
    candidates <- candidate_features_for_position(d, pos) %>% setdiff(blocked)
    usable <- select_usable_features(d, candidates)
    tibble(
      position = pos,
      features = list(usable),
      n_rows = nrow(d),
      n_drafted = sum(d$drafted == 1L, na.rm = TRUE),
      n_features = length(usable)
    )
  }
)
sophomore_feature_map <- setNames(
  sophomore_feature_map_table$features,
  sophomore_feature_map_table$position
)

write_csv(
  sophomore_quality,
  file.path(OUTPUT_DIR, "sophomore_position_feature_data_quality.csv")
)
write_csv(
  sophomore_correlations,
  file.path(OUTPUT_DIR, "sophomore_position_feature_correlations.csv")
)
write_csv(
  sophomore_feature_map_table %>% select(-features),
  file.path(OUTPUT_DIR, "sophomore_position_feature_map_summary.csv")
)
saveRDS(
  sophomore_feature_map,
  file.path(PROCESSED_DIR, "sophomore_position_feature_map.rds")
)

message("Sophomore correlation analysis saved.")

# Load the established tuning and ensemble functions without retraining the
# rising-senior models, then point them at the sophomore experiment.
Sys.setenv(PAA_DEFINE_TRAINING_FUNCTIONS_ONLY = "1")
source("R/04_train_models.R", encoding = "UTF-8")
Sys.unsetenv("PAA_DEFINE_TRAINING_FUNCTIONS_ONLY")

MODEL_DIR <- SOPHOMORE_MODEL_DIR
POSITIONS <- EXPERIMENT_POSITIONS
historical <- sophomore_historical
feature_map <- sophomore_feature_map
# The sophomore cutoff has fewer historical positives at several positions.
# Three is the smallest viable temporal-training count; the dashboard labels
# the resulting board experimental and reports the position-level sample sizes.
MIN_DRAFTED_ROWS <- 3L
set.seed(SEED + 1L)

message("\nTraining sophomore-cutoff position classifiers")
position_models <- setNames(vector("list", length(POSITIONS)), POSITIONS)
for (pos in POSITIONS) {
  message("\nSophomore position classifier: ", pos)
  position_models[[pos]] <- tryCatch(
    fit_position_classifier(pos),
    error = function(e) {
      warning(pos, " failed: ", conditionMessage(e))
      NULL
    }
  )
}
position_models <- compact(position_models)
if (!length(position_models)) stop("No sophomore position models completed.")
saveRDS(
  position_models,
  file.path(MODEL_DIR, "all_position_classifiers.rds")
)

all_candidate_round_features <- unique(
  unlist(feature_map, use.names = FALSE)
) %>% intersect(names(historical))

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
  stop("Insufficient drafted players for sophomore round model.")
}

round_split <- make_temporal_split(round_data)
round_train <- round_split$train %>% select(-draft_pick, -junior_year)
round_folds <- safe_vfold(round_split$train, "round_tier")
round_recipe <- make_recipe(round_tier ~ ., round_train, upsample = TRUE)

message("\nSophomore pooled direct round-tier classifier")
round_ensemble <- fit_engine_set(
  classification_specs(TRUE),
  round_recipe,
  round_folds,
  round_train,
  metric_set(mn_log_loss, accuracy, bal_accuracy),
  "mn_log_loss"
)
if (!length(round_ensemble$fits)) {
  stop("No sophomore direct round-tier model completed.")
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
saveRDS(round_model, file.path(MODEL_DIR, "pooled_round_tier_model.rds"))

ordinal_definitions <- list(
  r1 = c("R1"),
  r3_or_better = c("R1", "R2_3"),
  r5_or_better = c("R1", "R2_3", "R4_5")
)

ordinal_models <- imap(ordinal_definitions, function(positive_levels, label) {
  message("\nSophomore pooled ordinal boundary: ", label)
  d <- round_data %>%
    mutate(
      ordinal_target = factor(
        if_else(round_tier %in% positive_levels, "yes", "no"),
        levels = c("no", "yes")
      )
    )
  split <- make_temporal_split(d)
  train <- split$train %>%
    select(ordinal_target, position, all_of(usable_round_features))
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
  stop("Not all sophomore ordinal boundaries trained successfully.")
}
saveRDS(
  ordinal_models,
  file.path(MODEL_DIR, "pooled_ordinal_round_models.rds")
)

message("\nRunning TabFM for sophomore round ranges")
export_tabfm_rows <- function(data, include_target = TRUE) {
  out <- data %>%
    mutate(.tabfm_row_id = row_number()) %>%
    select(
      .tabfm_row_id, any_of("round_tier"), position,
      all_of(round_model$features)
    )
  if (!include_target) out <- out %>% select(-any_of("round_tier"))
  out
}

full_round_history <- sophomore_historical %>%
  filter(drafted == 1L, !is.na(round_tier), !is.na(position)) %>%
  select(round_tier, position, all_of(round_model$features))

write_csv(
  export_tabfm_rows(round_model$train),
  file.path(SOPHOMORE_TABFM_DIR, "round_train.csv"),
  na = ""
)
write_csv(
  export_tabfm_rows(round_model$test),
  file.path(SOPHOMORE_TABFM_DIR, "round_holdout.csv"),
  na = ""
)
write_csv(
  export_tabfm_rows(full_round_history),
  file.path(SOPHOMORE_TABFM_DIR, "round_full_history.csv"),
  na = ""
)
write_csv(
  export_tabfm_rows(sophomore_current, include_target = FALSE),
  file.path(SOPHOMORE_TABFM_DIR, "round_current.csv"),
  na = ""
)

if (!file.exists(TABFM_PYTHON)) {
  stop("TabFM Python environment is unavailable at ", TABFM_PYTHON)
}
tabfm_status <- system2(
  TABFM_PYTHON,
  c(
    "python/run_tabfm_round.py",
    "--exchange-dir", shQuote(SOPHOMORE_TABFM_DIR),
    "--estimators", as.character(TABFM_ESTIMATORS)
  )
)
if (!identical(tabfm_status, 0L)) {
  stop("Sophomore TabFM prediction process failed.")
}

predict_binary_ensemble <- function(obj, new_data) {
  values <- lapply(obj$classifier, function(fit_obj) {
    predict(fit_obj, new_data, type = "prob")$.pred_yes
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
    x <- new_data %>% select(position, all_of(obj$features))
    probs[[nm]] <- predict_binary_ensemble(obj, x)
  }
  cumulative <- t(apply(
    cbind(probs$r1, probs$r3_or_better, probs$r5_or_better),
    1,
    function(x) cummax(pmin(1, pmax(0, x)))
  ))
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
    within_one_tier_accuracy = mean(
      abs(truth_num - pred_num) <= 1L,
      na.rm = TRUE
    ),
    ordinal_mae = mean(abs(truth_num - pred_num), na.rm = TRUE),
    balanced_accuracy = suppressWarnings(bal_accuracy_vec(truth, pred)),
    macro_f1 = suppressWarnings(f_meas_vec(truth, pred, estimator = "macro")),
    quadratic_weighted_kappa = suppressWarnings(
      kap_vec(truth, pred, weighting = "quadratic")
    ),
    multiclass_log_loss = multiclass_log_loss(truth, prob_matrix)
  )
}

message("\nEvaluating sophomore-cutoff models")
position_results <- map(position_models, function(obj) {
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
      brier = mean((prob - truth_num)^2),
      balanced_accuracy = suppressWarnings(bal_accuracy_vec(truth, pred)),
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
        sophomore_year = junior_year,
        drafted = truth_num,
        draft_probability = prob,
        threshold,
        predicted_drafted = as.integer(prob >= threshold)
      )
  )
})

write_csv(
  map_dfr(position_results, "metrics"),
  file.path(OUTPUT_DIR, "sophomore_draft_probability_holdout_metrics.csv")
)
write_csv(
  map_dfr(position_results, "predictions"),
  file.path(OUTPUT_DIR, "sophomore_draft_probability_holdout_predictions.csv")
)

round_test <- round_model$test
direct_prob <- predict_multiclass_ensemble(
  round_model,
  round_test %>% select(position, all_of(round_model$features))
)
ordinal_prob <- ordinal_probability_matrix(ordinal_models, round_test)
base_round_prob <- ORDINAL_BLEND_WEIGHT * ordinal_prob +
  (1 - ORDINAL_BLEND_WEIGHT) * direct_prob
base_round_prob <- base_round_prob / rowSums(base_round_prob)

tabfm_holdout <- read_csv(
  file.path(SOPHOMORE_TABFM_DIR, "round_holdout_probabilities.csv"),
  show_col_types = FALSE
) %>% arrange(.tabfm_row_id)
if (nrow(tabfm_holdout) != nrow(round_test)) {
  stop("Sophomore TabFM holdout probabilities do not align.")
}
tabfm_prob <- as.matrix(tabfm_holdout[, paste0("prob_", ROUND_TIER_LEVELS)])
colnames(tabfm_prob) <- ROUND_TIER_LEVELS
augmented_prob <- TABFM_BLEND_WEIGHT * tabfm_prob +
  (1 - TABFM_BLEND_WEIGHT) * base_round_prob
augmented_prob <- augmented_prob / rowSums(augmented_prob)

round_truth <- factor(round_test$round_tier, levels = ROUND_TIER_LEVELS)
sophomore_round_metrics <- bind_rows(
  round_metric_row(
    "direct_multiclass", round_truth, direct_prob, round_model$holdout_years
  ),
  round_metric_row(
    "cumulative_ordinal", round_truth, ordinal_prob, round_model$holdout_years
  ),
  round_metric_row(
    "blended", round_truth, base_round_prob, round_model$holdout_years
  ),
  round_metric_row(
    "tabfm", round_truth, tabfm_prob, round_model$holdout_years
  ),
  round_metric_row(
    "tabfm_augmented", round_truth, augmented_prob,
    round_model$holdout_years
  )
)
write_csv(
  sophomore_round_metrics,
  file.path(OUTPUT_DIR, "sophomore_round_tier_holdout_metrics.csv")
)

predicted_round <- factor(
  ROUND_TIER_LEVELS[max.col(augmented_prob, ties.method = "first")],
  levels = ROUND_TIER_LEVELS
)
write_csv(
  bind_cols(
    round_test %>%
      transmute(
        sophomore_year = junior_year,
        position,
        draft_pick,
        round_tier
      ),
    as_tibble(augmented_prob, .name_repair = ~paste0("prob_", .x))
  ) %>% mutate(predicted_round_tier = predicted_round),
  file.path(OUTPUT_DIR, "sophomore_round_tier_holdout_predictions.csv")
)

message("\nScoring current rising juniors")
current <- sophomore_current %>% mutate(.tabfm_row_id = row_number())
stage1 <- map_dfr(names(position_models), function(pos) {
  obj <- position_models[[pos]]
  d <- current %>% filter(position == pos)
  if (!nrow(d)) return(tibble())
  x <- d %>% select(all_of(obj$features))
  d %>%
    mutate(
      draft_probability = predict_binary_ensemble(obj, x),
      draft_probability_threshold =
        obj$threshold %||% DEFAULT_DRAFT_PROBABILITY_THRESHOLD
    )
})
if (!nrow(stage1)) stop("No rising juniors matched sophomore models.")

current_direct_prob <- predict_multiclass_ensemble(
  round_model,
  stage1 %>% select(position, all_of(round_model$features))
)
current_ordinal_prob <- ordinal_probability_matrix(ordinal_models, stage1)
current_round_prob <- ORDINAL_BLEND_WEIGHT * current_ordinal_prob +
  (1 - ORDINAL_BLEND_WEIGHT) * current_direct_prob
current_round_prob <- current_round_prob / rowSums(current_round_prob)

tabfm_current <- read_csv(
  file.path(SOPHOMORE_TABFM_DIR, "round_current_probabilities.csv"),
  show_col_types = FALSE
) %>% arrange(.tabfm_row_id)
if (nrow(tabfm_current) != nrow(current)) {
  stop("Sophomore TabFM current probabilities do not align.")
}
tabfm_current_prob_all <- as.matrix(
  tabfm_current[, paste0("prob_", ROUND_TIER_LEVELS)]
)
colnames(tabfm_current_prob_all) <- ROUND_TIER_LEVELS
tabfm_current_prob <- tabfm_current_prob_all[
  stage1$.tabfm_row_id, , drop = FALSE
]
current_round_prob <- TABFM_BLEND_WEIGHT * tabfm_current_prob +
  (1 - TABFM_BLEND_WEIGHT) * current_round_prob
current_round_prob <- current_round_prob / rowSums(current_round_prob)

prob_df <- as_tibble(
  current_round_prob,
  .name_repair = ~paste0("prob_", .x)
)

sophomore_board <- bind_cols(stage1, prob_df) %>%
  mutate(
    conditional_round_tier = ROUND_TIER_LEVELS[
      max.col(current_round_prob, ties.method = "first")
    ],
    round_tier_confidence = apply(current_round_prob, 1, max),
    projected_drafted = as.integer(
      draft_probability >= draft_probability_threshold
    ),
    projected_round = case_when(
      projected_drafted == 0L ~ "Undrafted",
      conditional_round_tier == "R1" ~ "Round 1",
      conditional_round_tier == "R2_3" ~ "Rounds 2-3",
      conditional_round_tier == "R4_5" ~ "Rounds 4-5",
      conditional_round_tier == "R6_7" ~ "Rounds 6-7",
      TRUE ~ NA_character_
    ),
    expected_round_tier_score =
      prob_R1 * 1 + prob_R2_3 * 2 + prob_R4_5 * 3 + prob_R6_7 * 4,
    board_score = draft_probability *
      (5 - expected_round_tier_score) *
      (0.5 + 0.5 * round_tier_confidence),
    board_rank = min_rank(desc(board_score))
  ) %>%
  arrange(board_rank) %>%
  select(
    board_rank,
    player_name,
    school,
    position,
    sophomore_year = junior_year,
    draft_probability,
    draft_probability_threshold,
    projected_drafted,
    projected_round,
    conditional_round_tier,
    round_tier_confidence,
    prob_R1,
    prob_R2_3,
    prob_R4_5,
    prob_R6_7,
    has_recruiting,
    has_sophomore_stats,
    sophomore_feature_coverage,
    height_inches,
    weight_lbs,
    headshot_url,
    everything()
  )

write_csv(
  sophomore_board,
  file.path(OUTPUT_DIR, "rising_juniors_2026_experimental_draft_board.csv")
)
write_csv(
  sophomore_board %>% filter(projected_drafted == 1L),
  file.path(OUTPUT_DIR, "rising_juniors_2026_experimental_draftable_board.csv")
)

message("\nSophomore experiment complete.")
message("Rising juniors scored: ", nrow(sophomore_board))
message("Projected drafted: ", sum(sophomore_board$projected_drafted == 1L))
