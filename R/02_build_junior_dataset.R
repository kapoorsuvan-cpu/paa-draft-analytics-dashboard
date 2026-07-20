suppressPackageStartupMessages(library(tidyverse))
source("R/config.R")
source("R/utils.R")

stats <- read_rds(file.path(RAW_DIR, "player_stats.rds"))
rosters <- read_rds(file.path(RAW_DIR, "rosters.rds"))
recruiting <- read_rds(file.path(RAW_DIR, "recruiting.rds"))
draft <- read_rds(file.path(RAW_DIR, "draft_picks.rds"))

manual_path <- file.path(PROJECT_ROOT, "manual_draft_matches.csv")
manual_matches <- if (file.exists(manual_path)) {
  read_csv(manual_path, show_col_types = FALSE)
} else {
  tibble()
}

core_stats <- c(
  "passing_att", "passing_completions", "passing_yds",
  "passing_td", "passing_int",
  "rushing_car", "rushing_yds", "rushing_td",
  "receiving_rec", "receiving_yds", "receiving_td",
  "defensive_solo", "defensive_tot", "defensive_tfl",
  "defensive_sacks", "defensive_qb_hur", "defensive_pd",
  "interceptions_int", "fumbles_fum", "fumbles_lost",
  "kicking_fga", "kicking_fgm",
  "punting_no", "punting_yds"
)

prepare_stats <- function(x) {
  x <- add_missing_numeric(x, core_stats)
  if (!"conference" %in% names(x)) x$conference <- NA_character_

  collapsed <- x %>%
    filter(!is.na(player_clean), !is.na(school_clean), !is.na(season)) %>%
    group_by(player_clean, school_clean, season) %>%
    summarise(
      conference = first_nonmissing(conference),
      across(all_of(core_stats), ~sum(as.numeric(.x), na.rm = TRUE)),
      .groups = "drop"
    )

  collapsed %>%
    group_by(season, school_clean) %>%
    mutate(
      team_pass_att = sum(passing_att, na.rm = TRUE),
      team_rush_car = sum(rushing_car, na.rm = TRUE),
      team_rush_yds = sum(rushing_yds, na.rm = TRUE),
      team_rec_yds = sum(receiving_yds, na.rm = TRUE),
      team_rec_td = sum(receiving_td, na.rm = TRUE)
    ) %>%
    ungroup() %>%
    mutate(
      pass_ypa = safe_divide(passing_yds, passing_att),
      completion_pct = safe_divide(passing_completions, passing_att),
      pass_td_rate = safe_divide(passing_td, passing_att),
      pass_int_rate = safe_divide(passing_int, passing_att),
      rush_ypc = safe_divide(rushing_yds, rushing_car),
      rec_ypr = safe_divide(receiving_yds, receiving_rec),
      rush_share = safe_divide(rushing_car, team_rush_car),
      rush_yds_share = safe_divide(rushing_yds, team_rush_yds),
      rec_yds_share = safe_divide(receiving_yds, team_rec_yds),
      rec_td_share = safe_divide(receiving_td, team_rec_td),
      fg_pct = safe_divide(kicking_fgm, kicking_fga),
      punt_avg = safe_divide(punting_yds, punting_no)
    )
}

prepared_stats <- prepare_stats(stats)

stat_match_table <- function(season_offset) {
  numeric_cols <- prepared_stats %>%
    select(where(is.numeric)) %>%
    names() %>%
    setdiff("season")

  prepared_stats %>%
    transmute(
      player_clean,
      school_clean,
      target_year = season + season_offset,
      conference,
      across(all_of(numeric_cols), as.numeric)
    )
}

join_stats_with_fallback <- function(base, stat_table, prefix) {
  exact <- stat_table %>%
    rename(!!paste0(prefix, "conference") := conference) %>%
    rename_with(
      ~paste0(prefix, .x),
      -c(player_clean, school_clean, target_year, all_of(paste0(prefix, "conference")))
    )

  out <- base %>%
    left_join(
      exact,
      by = c(
        "player_clean",
        "school_clean",
        "junior_year" = "target_year"
      )
    )

  unique_fallback <- stat_table %>%
    group_by(player_clean, target_year) %>%
    filter(n() == 1L) %>%
    ungroup() %>%
    select(-school_clean) %>%
    rename(!!paste0(prefix, "conference_fallback") := conference) %>%
    rename_with(
      ~paste0(prefix, .x, "_fallback"),
      -c(player_clean, target_year, all_of(paste0(prefix, "conference_fallback")))
    )

  out <- out %>%
    left_join(
      unique_fallback,
      by = c("player_clean", "junior_year" = "target_year")
    )

  exact_cols <- names(out)[str_starts(names(out), prefix) &
                             !str_ends(names(out), "_fallback")]
  for (nm in exact_cols) {
    fb <- paste0(nm, "_fallback")
    if (fb %in% names(out)) out[[nm]] <- coalesce(out[[nm]], out[[fb]])
  }

  out %>% select(-ends_with("_fallback"))
}

attach_recruiting <- function(out) {
  exact_rec <- recruiting %>%
    arrange(player_clean, committed_school_clean, desc(recruit_rating)) %>%
    distinct(player_clean, committed_school_clean, .keep_all = TRUE)

  out <- out %>%
    left_join(
      exact_rec,
      by = c(
        "player_clean",
        "school_clean" = "committed_school_clean"
      )
    )

  unique_rec <- recruiting %>%
    group_by(player_clean) %>%
    filter(n() == 1L) %>%
    ungroup() %>%
    select(-committed_school_clean)

  out <- out %>%
    left_join(unique_rec, by = "player_clean", suffix = c("", "_fallback"))

  for (nm in c(
    "recruit_year", "recruit_position", "stars",
    "recruit_rank", "recruit_rating", "blue_chip"
  )) {
    fb <- paste0(nm, "_fallback")
    if (fb %in% names(out)) out[[nm]] <- coalesce(out[[nm]], out[[fb]])
  }

  out %>% select(-ends_with("_fallback"))
}

attach_context_features <- function(out) {
  draft_history <- draft %>%
    filter(!is.na(college_clean), !is.na(draft_year))

  map_dfr(seq_len(nrow(out)), function(i) {
    row <- out[i, ]
    prior <- draft_history %>%
      filter(
        college_clean == row$school_clean,
        draft_year < row$junior_year + 1L,
        draft_year >= row$junior_year - 4L
      )
    row %>%
      mutate(
        school_drafts_prior_5y = nrow(prior),
        school_position_drafts_prior_5y =
          sum(prior$nfl_position == row$position, na.rm = TRUE)
      )
  })
}

match_draft_outcomes <- function(out) {
  base <- out %>%
    mutate(row_id = row_number()) %>%
    select(row_id, player_clean, school_clean, position, junior_year)

  candidates <- base %>%
    left_join(draft, by = "player_clean", relationship = "many-to-many") %>%
    filter(
      !is.na(draft_year),
      between(
        draft_year,
        junior_year + 1L,
        junior_year + DRAFT_LOOKAHEAD_YEARS
      )
    ) %>%
    mutate(
      school_match = as.integer(
        !is.na(college_clean) &
          college_clean != "" &
          college_clean == school_clean
      ),
      position_match = as.integer(
        !is.na(nfl_position) &
          nfl_position == position
      ),
      match_score = 3 * school_match + 2 * position_match -
        0.01 * abs(draft_year - (junior_year + 1L))
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

  labels <- base %>%
    left_join(candidates, by = "row_id") %>%
    mutate(
      drafted = as.integer(!is.na(draft_pick)),
      overall_pick = if_else(
        drafted == 1L,
        as.numeric(draft_pick),
        as.numeric(UNDRAFTED_PICK)
      )
    )

  if (nrow(manual_matches)) {
    labels <- labels %>%
      left_join(
        manual_matches,
        by = c("player_clean", "junior_year"),
        suffix = c("", "_manual")
      ) %>%
      mutate(
        draft_year = coalesce(draft_year_manual, draft_year),
        draft_pick = coalesce(draft_pick_manual, draft_pick),
        draft_round = coalesce(draft_round_manual, draft_round),
        drafted = as.integer(!is.na(draft_pick)),
        overall_pick = if_else(
          drafted == 1L,
          as.numeric(draft_pick),
          as.numeric(UNDRAFTED_PICK)
        ),
        draft_match_method = if_else(
          !is.na(draft_pick_manual),
          "manual_override",
          draft_match_method
        )
      ) %>%
      select(-ends_with("_manual"))
  }

  labels %>%
    select(
      row_id, draft_year, draft_pick, draft_round,
      drafted, overall_pick, draft_match_method
    )
}

add_percentile_features <- function(df) {
  candidates <- names(df)[
    str_detect(
      names(df),
      "^jr_(pass_ypa|completion_pct|pass_td_rate|pass_int_rate|rush_ypc|rec_ypr|rush_share|rush_yds_share|rec_yds_share|rec_td_share|fg_pct|punt_avg|passing_yds|passing_td|rushing_yds|rushing_td|receiving_yds|receiving_td|defensive_solo|defensive_tot|defensive_tfl|defensive_sacks|defensive_qb_hur|defensive_pd|interceptions_int)$|^delta_"
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
        ~if_else(is.finite(as.numeric(.x)),
                 percent_rank(as.numeric(.x)),
                 NA_real_),
        .names = "{.col}_pct"
      ),
      recruit_rating_pct = if_else(
        is.finite(recruit_rating),
        percent_rank(recruit_rating),
        NA_real_
      ),
      recruit_rank_pct = if_else(
        is.finite(recruit_rank),
        1 - percent_rank(recruit_rank),
        NA_real_
      ),
      height_position_pct = if_else(
        is.finite(height_inches),
        percent_rank(height_inches),
        NA_real_
      ),
      weight_position_pct = if_else(
        is.finite(weight_lbs),
        percent_rank(weight_lbs),
        NA_real_
      )
    ) %>%
    ungroup()
}

build_junior_rows <- function(junior_years, add_outcomes = TRUE) {
  junior_years <- as.integer(junior_years)

  roster_rows <- rosters %>%
    filter(
      season %in% junior_years,
      class == "JR",
      position %in% POSITIONS
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
    stat_match_table(1L),
    "so_"
  )
  out <- join_stats_with_fallback(
    out,
    stat_match_table(0L),
    "jr_"
  )
  out <- attach_recruiting(out)

  common <- intersect(
    str_remove(names(out)[str_starts(names(out), "jr_")], "^jr_"),
    str_remove(names(out)[str_starts(names(out), "so_")], "^so_")
  )

  numeric_common <- common[vapply(common, function(nm) {
    is.numeric(out[[paste0("jr_", nm)]]) &&
      is.numeric(out[[paste0("so_", nm)]])
  }, logical(1))]

  for (nm in numeric_common) {
    jr_nm <- paste0("jr_", nm)
    so_nm <- paste0("so_", nm)
    out[[paste0("delta_", nm)]] <- out[[jr_nm]] - out[[so_nm]]
    out[[paste0("growth_", nm)]] <- ifelse(
      is.finite(out[[so_nm]]) & abs(out[[so_nm]]) > 1e-8,
      (out[[jr_nm]] - out[[so_nm]]) / abs(out[[so_nm]]),
      NA_real_
    )
  }

  so_numeric <- names(out)[str_starts(names(out), "so_") &
                             map_lgl(out, is.numeric)]
  jr_numeric <- names(out)[str_starts(names(out), "jr_") &
                             map_lgl(out, is.numeric)]

  out <- out %>%
    mutate(
      has_recruiting = as.integer(
        is.finite(recruit_rating) | is.finite(stars)
      ),
      has_sophomore_stats = as.integer(
        rowSums(across(all_of(so_numeric), ~replace_na(.x, 0) != 0)) > 0
      ),
      has_junior_stats = as.integer(
        rowSums(across(all_of(jr_numeric), ~replace_na(.x, 0) != 0)) > 0
      ),
      sophomore_feature_coverage = rowMeans(
        across(all_of(so_numeric), ~is.finite(as.numeric(.x)))
      ),
      junior_feature_coverage = rowMeans(
        across(all_of(jr_numeric), ~is.finite(as.numeric(.x)))
      ),
      years_since_recruit = junior_year - recruit_year,
      power_conference = power_conference_flag(jr_conference),
      transfer_or_school_mismatch = as.integer(
        !is.na(recruit_position) &
          !is.na(position) &
          recruit_position != position
      )
    )

  out <- attach_context_features(out)
  out <- add_percentile_features(out)

  if (add_outcomes) {
    labels <- match_draft_outcomes(out)
    out <- out %>%
      mutate(row_id = row_number()) %>%
      left_join(labels, by = "row_id") %>%
      select(-row_id) %>%
      mutate(
        drafted = coalesce(drafted, 0L),
        overall_pick = coalesce(
          overall_pick,
          as.numeric(UNDRAFTED_PICK)
        ),
        round_tier = case_when(
          drafted == 0L ~ NA_character_,
          draft_round == 1L ~ "R1",
          draft_round %in% 2:3 ~ "R2_3",
          draft_round %in% 4:5 ~ "R4_5",
          draft_round %in% 6:7 ~ "R6_7",
          TRUE ~ NA_character_
        ),
        round_tier = factor(
          round_tier,
          levels = ROUND_TIER_LEVELS
        )
      )
  }

  out
}

if (Sys.getenv("PAA_DEFINE_DATA_FUNCTIONS_ONLY") != "1") {
  historical <- build_junior_rows(HISTORICAL_JUNIOR_YEARS, TRUE)
  current <- build_junior_rows(CURRENT_STAT_YEAR, FALSE)

  write_rds(
    historical,
    file.path(PROCESSED_DIR, "historical_junior_dataset.rds")
  )
  write_csv(
    historical,
    file.path(PROCESSED_DIR, "historical_junior_dataset.csv")
  )
  write_rds(
    current,
    file.path(PROCESSED_DIR, "current_feature_rows.rds")
  )
  write_csv(
    current,
    file.path(PROCESSED_DIR, "current_feature_rows.csv")
  )

  message("Historical rows: ", nrow(historical))
  message("Historical drafted labels: ", sum(historical$drafted == 1L))
  message("Current rising seniors: ", nrow(current))
}
