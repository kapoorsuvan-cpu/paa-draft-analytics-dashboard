suppressPackageStartupMessages({
  library(tidyverse)
})
source("R/config.R")

if (!RUN_TABFM) {
  message("TabFM disabled in R/config.R.")
} else {
  if (!file.exists(TABFM_PYTHON)) {
    stop(
      "TabFM Python environment not found at ", TABFM_PYTHON,
      ". Run ./setup_tabfm.sh first."
    )
  }

  round_obj <- readRDS(file.path(MODEL_DIR, "pooled_round_tier_model.rds"))
  historical <- readRDS(
    file.path(PROCESSED_DIR, "historical_junior_dataset.rds")
  )
  current <- readRDS(file.path(PROCESSED_DIR, "current_feature_rows.rds"))
  exchange_dir <- file.path(PROCESSED_DIR, "tabfm")
  dir.create(exchange_dir, recursive = TRUE, showWarnings = FALSE)

  features <- round_obj$features
  export_rows <- function(data, include_target = TRUE) {
    out <- data %>%
      mutate(.tabfm_row_id = row_number()) %>%
      select(.tabfm_row_id, any_of("round_tier"), position, all_of(features))
    if (!include_target) out <- out %>% select(-any_of("round_tier"))
    out
  }

  full_history <- historical %>%
    filter(drafted == 1L, !is.na(round_tier), !is.na(position)) %>%
    select(round_tier, position, all_of(features))

  write_csv(
    export_rows(round_obj$train),
    file.path(exchange_dir, "round_train.csv"), na = ""
  )
  write_csv(
    export_rows(round_obj$test),
    file.path(exchange_dir, "round_holdout.csv"), na = ""
  )
  write_csv(
    export_rows(full_history),
    file.path(exchange_dir, "round_full_history.csv"), na = ""
  )
  write_csv(
    export_rows(current, include_target = FALSE),
    file.path(exchange_dir, "round_current.csv"), na = ""
  )

  status <- system2(
    TABFM_PYTHON,
    c(
      "python/run_tabfm_round.py",
      "--exchange-dir", shQuote(exchange_dir),
      "--estimators", as.character(TABFM_ESTIMATORS)
    )
  )
  if (!identical(status, 0L)) stop("TabFM prediction process failed.")
  message("TabFM holdout and current probabilities saved.")
}
