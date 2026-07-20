options(nflreadr.verbose = FALSE)

scripts <- c(
  "R/01_pull_raw_data.R",
  "R/02_build_junior_dataset.R",
  "R/03_feature_analysis.R",
  "R/07_data_quality_report.R",
  "R/04_train_models.R",
  "R/04b_run_tabfm.R",
  "R/05_evaluate_models.R",
  "R/06_score_current_players.R"
)

for (script in scripts) {
  message("\n==============================")
  message("Running ", script)
  message("==============================")
  source(script, encoding = "UTF-8")
}
