suppressPackageStartupMessages(library(tidyverse))
source("R/config.R")
source("R/utils.R")

df <- read_rds(
  file.path(PROCESSED_DIR, "historical_junior_dataset.rds")
)

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
    sum(ok) < 25L ||
      n_distinct(x[ok]) < 3L ||
      sd(x[ok]) == 0 ||
      sd(y[ok]) == 0
  ) return(NULL)

  tibble(
    feature = feature,
    n = sum(ok),
    nonzero_n = sum(x[ok] != 0),
    pearson = suppressWarnings(cor(x[ok], y[ok], method = "pearson")),
    spearman = suppressWarnings(cor(x[ok], y[ok], method = "spearman"))
  )
}

all_quality <- map_dfr(POSITIONS, function(pos) {
  d <- df %>% filter(position == pos)
  candidates <- candidate_features_for_position(d, pos) %>%
    setdiff(blocked)

  q <- feature_quality(d, candidates)
  if (!nrow(q)) return(tibble())

  q %>%
    mutate(
      position = pos,
      selected = feature %in% select_usable_features(d, candidates),
      .before = 1
    )
})

write_csv(
  all_quality,
  file.path(OUTPUT_DIR, "position_feature_data_quality.csv")
)

correlations <- map_dfr(POSITIONS, function(pos) {
  d <- df %>% filter(position == pos)
  candidates <- candidate_features_for_position(d, pos) %>%
    setdiff(blocked)
  usable <- select_usable_features(d, candidates)

  result <- map_dfr(usable, ~cor_one(d, .x))
  if (!nrow(result)) {
    message("No valid correlations for ", pos, " (rows=", nrow(d), ").")
    return(tibble())
  }

  result %>%
    mutate(
      position = pos,
      abs_spearman = abs(spearman),
      .before = 1
    ) %>%
    arrange(desc(abs_spearman))
})

write_csv(
  correlations,
  file.path(OUTPUT_DIR, "position_feature_correlations.csv")
)

feature_map <- map_dfr(POSITIONS, function(pos) {
  d <- df %>% filter(position == pos)
  candidates <- candidate_features_for_position(d, pos) %>%
    setdiff(blocked)
  usable <- select_usable_features(d, candidates)

  tibble(
    position = pos,
    features = list(usable),
    n_rows = nrow(d),
    n_drafted = sum(d$drafted == 1L, na.rm = TRUE),
    n_features = length(usable)
  )
})

saveRDS(
  setNames(feature_map$features, feature_map$position),
  file.path(PROCESSED_DIR, "position_feature_map.rds")
)
write_csv(
  feature_map %>% select(-features),
  file.path(OUTPUT_DIR, "position_feature_map_summary.csv")
)

message("Saved feature maps for ", nrow(feature_map), " positions.")
