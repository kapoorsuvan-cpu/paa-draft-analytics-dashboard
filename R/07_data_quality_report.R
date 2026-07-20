suppressPackageStartupMessages(library(tidyverse))
source("R/config.R")
source("R/utils.R")

historical <- readRDS(
  file.path(PROCESSED_DIR, "historical_junior_dataset.rds")
)

summary <- historical %>%
  group_by(position) %>%
  summarise(
    rows = n(),
    drafted = sum(drafted == 1L),
    draft_rate = mean(drafted == 1L),
    recruiting_available = mean(has_recruiting == 1L),
    sophomore_stats_available = mean(has_sophomore_stats == 1L),
    junior_stats_available = mean(has_junior_stats == 1L),
    average_sophomore_coverage = mean(
      sophomore_feature_coverage, na.rm = TRUE
    ),
    average_junior_coverage = mean(
      junior_feature_coverage, na.rm = TRUE
    ),
    height_available = mean(is.finite(height_inches)),
    weight_available = mean(is.finite(weight_lbs)),
    .groups = "drop"
  )

write_csv(
  summary,
  file.path(OUTPUT_DIR, "historical_data_quality_summary.csv")
)

match_summary <- historical %>%
  count(draft_match_method, drafted, name = "rows") %>%
  arrange(desc(rows))
write_csv(
  match_summary,
  file.path(OUTPUT_DIR, "draft_match_quality_summary.csv")
)

message("Data-quality reports saved.")
