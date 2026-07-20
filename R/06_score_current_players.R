suppressPackageStartupMessages({
  library(tidymodels)
  library(tidyverse)
})
source("R/config.R")
source("R/utils.R")

current <- readRDS(
  file.path(PROCESSED_DIR, "current_feature_rows.rds")
) %>% mutate(.tabfm_row_id = row_number())
position_models <- readRDS(
  file.path(MODEL_DIR, "all_position_classifiers.rds")
)
round_obj <- readRDS(
  file.path(MODEL_DIR, "pooled_round_tier_model.rds")
)
ordinal_models <- readRDS(
  file.path(MODEL_DIR, "pooled_ordinal_round_models.rds")
)

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
    x <- new_data %>%
      select(position, all_of(obj$features))
    probs[[nm]] <- predict_binary_ensemble(obj, x)
  }

  cumulative <- t(apply(
    cbind(
      probs$r1,
      probs$r3_or_better,
      probs$r5_or_better
    ),
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

if (!nrow(stage1)) {
  stop("No current players matched trained position classifiers.")
}

direct_new <- stage1 %>%
  select(position, all_of(round_obj$features))
direct_prob <- predict_multiclass_ensemble(round_obj, direct_new)
ordinal_prob <- ordinal_probability_matrix(ordinal_models, stage1)

blend_prob <- ORDINAL_BLEND_WEIGHT * ordinal_prob +
  (1 - ORDINAL_BLEND_WEIGHT) * direct_prob
blend_prob <- blend_prob / rowSums(blend_prob)

tabfm_path <- file.path(
  PROCESSED_DIR, "tabfm", "round_current_probabilities.csv"
)
if (RUN_TABFM && file.exists(tabfm_path)) {
  tabfm_df <- read_csv(tabfm_path, show_col_types = FALSE) %>%
    arrange(.tabfm_row_id)
  tabfm_prob_all <- as.matrix(
    tabfm_df[, paste0("prob_", ROUND_TIER_LEVELS)]
  )
  colnames(tabfm_prob_all) <- ROUND_TIER_LEVELS
  if (nrow(tabfm_prob_all) != nrow(current)) {
    stop("TabFM current probabilities do not align with current rows.")
  }
  tabfm_prob <- tabfm_prob_all[stage1$.tabfm_row_id, , drop = FALSE]
  blend_prob <- TABFM_BLEND_WEIGHT * tabfm_prob +
    (1 - TABFM_BLEND_WEIGHT) * blend_prob
  blend_prob <- blend_prob / rowSums(blend_prob)
}

pick_path <- file.path(MODEL_DIR, "pooled_exact_pick_model.rds")
exploratory_pick <- rep(NA_real_, nrow(stage1))
if (file.exists(pick_path)) {
  pick_obj <- readRDS(pick_path)
  pick_new <- stage1 %>%
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
  exploratory_pick <- weighted_row_mean(values, pick_obj$weights)
}

prob_df <- as_tibble(
  blend_prob,
  .name_repair = ~paste0("prob_", .x)
)

board <- bind_cols(stage1, prob_df) %>%
  mutate(
    exploratory_exact_pick = exploratory_pick,
    conditional_round_tier = ROUND_TIER_LEVELS[
      max.col(blend_prob, ties.method = "first")
    ],
    round_tier_confidence = apply(blend_prob, 1, max),
    projected_drafted = as.integer(
      draft_probability >= draft_probability_threshold
    ),
    projected_round = case_when(
      projected_drafted == 0L ~ "Low draft probability",
      conditional_round_tier == "R1" ~ "Round 1",
      conditional_round_tier == "R2_3" ~ "Rounds 2-3",
      conditional_round_tier == "R4_5" ~ "Rounds 4-5",
      conditional_round_tier == "R6_7" ~ "Rounds 6-7",
      TRUE ~ NA_character_
    ),
    expected_round_tier_score =
      prob_R1 * 1 +
      prob_R2_3 * 2 +
      prob_R4_5 * 3 +
      prob_R6_7 * 4,
    board_score =
      draft_probability *
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
    junior_year,
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
    exploratory_exact_pick,
    has_recruiting,
    has_sophomore_stats,
    has_junior_stats,
    sophomore_feature_coverage,
    junior_feature_coverage,
    height_inches,
    weight_lbs,
    headshot_url,
    everything()
  )

output_name <- paste0(
  "rising_seniors_",
  CURRENT_STAT_YEAR + 1L,
  "_projected_",
  TARGET_DRAFT_YEAR,
  "_draft_board.csv"
)

write_csv(board, file.path(OUTPUT_DIR, output_name))
write_csv(
  board %>% filter(projected_drafted == 1L),
  file.path(
    OUTPUT_DIR,
    paste0(
      "rising_seniors_",
      CURRENT_STAT_YEAR + 1L,
      "_projected_draftable_board.csv"
    )
  )
)

message("Saved board with ", nrow(board), " rising seniors.")
message("Projected drafted: ", sum(board$projected_drafted))
