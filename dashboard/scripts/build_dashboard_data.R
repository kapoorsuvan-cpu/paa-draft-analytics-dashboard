suppressPackageStartupMessages(library(tidyverse))

root <- normalizePath("..")
out_dir <- file.path("public", "data")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

board <- read_csv(
  file.path(root, "outputs", "rising_seniors_2026_projected_2027_draft_board.csv"),
  show_col_types = FALSE,
  guess_max = Inf
) %>%
  filter(junior_year == 2025)

# Official NFL underclassmen who entered the 2026 draft are no longer 2026
# rising seniors. Keep this explicit so a historical "JR" label cannot leak a
# declared player into the 2027 board.
early_entrants_2026 <- c(
  "Parker Brailsford", "Kadyn Proctor", "Ty Simpson", "Genesis Smith",
  "Keith Abney II", "Jordyn Tyson", "Keldric Faulk", "Connor Lew",
  "Kage Casey", "Jude Bowry", "T.J. Parker", "Avieon Terrell",
  "Antonio Williams", "Peter Woods", "Brian Parker II", "Wesley Williams",
  "CJ Allen", "Zachariah Branch", "Monroe Freeling", "Christen Miller",
  "Aaron Anderson", "Marlin Klein", "Ryan Eckley", "Josiah Trotter",
  "Emmett Johnson", "Jeremiyah Love", "Jadarian Price", "Billy Schrauth",
  "Aamil Wagner", "Caleb Downs", "Max Klare", "Kayden McDonald",
  "Arvell Reese", "Carnell Tate", "Kenyon Sadiq", "Dillon Thieneman",
  "Olaivavega Ioane", "Kyle Louis", "Brandon Cisse", "Jalon Kilgore",
  "Collin Wright", "Chris Brazzell II", "Arion Carter", "Colton Hood",
  "Jermod McCoy", "Jack Endries", "Anthony Hill Jr.", "Malik Muhammad",
  "Chase Bisontis", "KC Concepcion", "Taurean York", "Ja'Kobi Lane",
  "Makai Lemon", "Kamari Ramsey", "Logan Fano", "Spencer Fano",
  "Caleb Lomu", "Denzel Boston"
)
clean_name <- function(x) {
  x %>%
    str_replace_all("[^a-zA-Z0-9]+", " ") %>%
    str_to_lower() %>%
    str_squish() %>%
    str_remove(" (jr|sr|ii|iii|iv)$")
}
clean_school_name <- function(x) {
  clean_name(x) %>%
    str_replace_all("\\buniversity\\b|\\bcollege\\b|\\bstate university\\b", "") %>%
    str_replace_all("\\bst\\b", "state") %>%
    str_squish()
}
standardize_draft_position <- function(x) {
  x <- str_to_upper(as.character(x))
  case_when(
    x %in% c("RB", "HB", "FB") ~ "RB",
    x %in% c("DE", "EDGE", "OLB") ~ "EDGE",
    x %in% c("DT", "NT", "DL", "IDL") ~ "IDL",
    x %in% c("LB", "ILB", "MLB") ~ "ILB",
    x %in% c("S", "FS", "SS", "SAF") ~ "SAF",
    TRUE ~ x
  )
}
remove_current_draftees <- function(data, drafted_profiles) {
  name_school <- drafted_profiles %>%
    transmute(key = paste(
      clean_name(pfr_player_name), clean_school_name(college), sep = "|"
    )) %>%
    pull(key)
  name_position <- drafted_profiles %>%
    transmute(key = paste(
      clean_name(pfr_player_name), standardize_draft_position(position),
      sep = "|"
    )) %>%
    pull(key)
  data %>%
    filter(
      !(
        paste(clean_name(player_name), clean_school_name(school), sep = "|") %in%
          name_school |
          paste(clean_name(player_name), position, sep = "|") %in%
            name_position
      )
    )
}

board <- board %>%
  filter(!clean_name(player_name) %in% clean_name(early_entrants_2026))

drafted_2026 <- nflreadr::load_draft_picks() %>%
  filter(season == 2026)
if (!nrow(drafted_2026)) {
  stop("The 2026 NFL Draft results were unavailable; refusing to export a stale board.")
}
board <- board %>%
  remove_current_draftees(drafted_2026) %>%
  mutate(
    tier_order = match(conditional_round_tier, c("R1", "R2_3", "R4_5", "R6_7")),
    display_group = if_else(projected_drafted == 1, tier_order, 5L)
  ) %>%
  # Use the model's real two-stage prediction. Players projected to be drafted
  # are ordered by their conditional round range and draft confidence. Everyone
  # below the position threshold follows, ordered by draft confidence.
  arrange(display_group, desc(draft_probability), expected_round_tier_score, board_rank) %>%
  mutate(display_rank = row_number())
correlations <- read_csv(
  file.path(root, "outputs", "position_feature_correlations.csv"),
  show_col_types = FALSE
)
round_metrics <- read_csv(
  file.path(root, "outputs", "round_tier_holdout_metrics.csv"),
  show_col_types = FALSE
)
position_metrics <- read_csv(
  file.path(root, "outputs", "draft_probability_holdout_metrics.csv"),
  show_col_types = FALSE
)

excluded <- "recruit|stars|blue_chip|class|feature_coverage|has_"
feature_meta <- correlations %>%
  filter(!str_detect(feature, regex(excluded, ignore_case = TRUE))) %>%
  group_by(position) %>%
  arrange(desc(abs_spearman), .by_group = TRUE) %>%
  slice_head(n = 6) %>%
  mutate(weight = abs_spearman / sum(abs_spearman)) %>%
  ungroup() %>%
  select(position, feature, weight, spearman, n)

feature_stats <- feature_meta %>%
  group_by(position, feature) %>%
  group_modify(~{
    values <- board %>%
      filter(position == .y$position) %>%
      pull(all_of(.y$feature)) %>%
      as.numeric()
    tibble(
      mean = mean(values, na.rm = TRUE),
      sd = sd(values, na.rm = TRUE),
      median = median(values, na.rm = TRUE),
      min = quantile(values, .05, na.rm = TRUE),
      max = quantile(values, .95, na.rm = TRUE)
    )
  }) %>%
  ungroup()

feature_meta <- feature_meta %>%
  left_join(feature_stats, by = c("position", "feature"))

feature_names <- unique(feature_meta$feature)
players <- board %>%
  transmute(
    id = row_number(),
    rank = display_rank,
    name = player_name,
    school,
    position,
    eligibility = "2026 Rising Senior",
    draftProbability = draft_probability,
    threshold = draft_probability_threshold,
    projectedDrafted = projected_drafted == 1,
    projectedRange = if_else(projected_drafted == 1, projected_round, "Undrafted"),
    tier = conditional_round_tier,
    confidence = round_tier_confidence,
    probR1 = prob_R1,
    probR23 = prob_R2_3,
    probR45 = prob_R4_5,
    probR67 = prob_R6_7,
    height = height_inches,
    weight = weight_lbs,
    headshot = headshot_url,
    across(any_of(feature_names))
  )

jsonlite::write_json(players, file.path(out_dir, "players.json"), na = "null")
jsonlite::write_json(feature_meta, file.path(out_dir, "features.json"), na = "null")
jsonlite::write_json(round_metrics, file.path(out_dir, "round_metrics.json"), na = "null")
jsonlite::write_json(position_metrics, file.path(out_dir, "position_metrics.json"), na = "null")

message("Dashboard data built for ", nrow(players), " rising seniors.")

# Experimental rising-junior board. This is intentionally exported separately
# from the rising-senior board because it uses a different training cutoff,
# feature map, validation result, and interpretation.
rising_juniors <- read_csv(
  file.path(root, "outputs", "rising_juniors_2026_experimental_draft_board.csv"),
  show_col_types = FALSE,
  guess_max = Inf
) %>%
  filter(sophomore_year == 2025) %>%
  remove_current_draftees(drafted_2026) %>%
  mutate(
    tier_order = match(conditional_round_tier, c("R1", "R2_3", "R4_5", "R6_7")),
    display_group = if_else(projected_drafted == 1, tier_order, 5L)
  ) %>%
  arrange(display_group, desc(draft_probability), expected_round_tier_score, board_rank) %>%
  mutate(display_rank = row_number())

sophomore_correlations <- read_csv(
  file.path(root, "outputs", "sophomore_position_feature_correlations.csv"),
  show_col_types = FALSE
)
sophomore_round_metrics <- read_csv(
  file.path(root, "outputs", "sophomore_round_tier_holdout_metrics.csv"),
  show_col_types = FALSE
)
sophomore_position_metrics <- read_csv(
  file.path(root, "outputs", "sophomore_draft_probability_holdout_metrics.csv"),
  show_col_types = FALSE
)

sophomore_feature_meta <- sophomore_correlations %>%
  filter(!str_detect(feature, regex(excluded, ignore_case = TRUE))) %>%
  group_by(position) %>%
  arrange(desc(abs_spearman), .by_group = TRUE) %>%
  slice_head(n = 6) %>%
  mutate(weight = abs_spearman / sum(abs_spearman)) %>%
  ungroup() %>%
  select(position, feature, weight, spearman, n)

sophomore_feature_stats <- sophomore_feature_meta %>%
  group_by(position, feature) %>%
  group_modify(~{
    values <- rising_juniors %>%
      filter(position == .y$position) %>%
      pull(all_of(.y$feature)) %>%
      as.numeric()
    finite <- values[is.finite(values)]
    if (!length(finite)) {
      return(tibble(mean = 0, sd = 0, median = 0, min = 0, max = 0))
    }
    tibble(
      mean = mean(finite),
      sd = sd(finite, na.rm = TRUE) %>% replace_na(0),
      median = median(finite),
      min = quantile(finite, .05, names = FALSE),
      max = quantile(finite, .95, names = FALSE)
    )
  }) %>%
  ungroup()

sophomore_feature_meta <- sophomore_feature_meta %>%
  left_join(sophomore_feature_stats, by = c("position", "feature"))

sophomore_feature_names <- unique(sophomore_feature_meta$feature)
rising_junior_players <- rising_juniors %>%
  transmute(
    id = row_number(),
    rank = display_rank,
    name = player_name,
    school,
    position,
    eligibility = "2026 Rising Junior",
    draftProbability = draft_probability,
    threshold = draft_probability_threshold,
    projectedDrafted = projected_drafted == 1,
    projectedRange = if_else(projected_drafted == 1, projected_round, "Undrafted"),
    tier = conditional_round_tier,
    confidence = round_tier_confidence,
    probR1 = prob_R1,
    probR23 = prob_R2_3,
    probR45 = prob_R4_5,
    probR67 = prob_R6_7,
    height = height_inches,
    weight = weight_lbs,
    headshot = headshot_url,
    across(any_of(sophomore_feature_names))
  )

jsonlite::write_json(
  rising_junior_players,
  file.path(out_dir, "rising_juniors.json"),
  na = "null"
)
jsonlite::write_json(
  sophomore_feature_meta,
  file.path(out_dir, "sophomore_features.json"),
  na = "null"
)
jsonlite::write_json(
  sophomore_round_metrics,
  file.path(out_dir, "sophomore_round_metrics.json"),
  na = "null"
)
jsonlite::write_json(
  sophomore_position_metrics,
  file.path(out_dir, "sophomore_position_metrics.json"),
  na = "null"
)

message(
  "Experimental dashboard data built for ",
  nrow(rising_junior_players),
  " rising juniors."
)
