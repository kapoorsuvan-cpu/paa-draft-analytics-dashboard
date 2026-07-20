suppressPackageStartupMessages({
  library(tidyverse)
  library(stringi)
})

clean_name <- function(x) {
  as.character(x) %>%
    str_to_lower() %>%
    stri_trans_general("Latin-ASCII") %>%
    str_replace_all("\\b(jr|sr|ii|iii|iv|v)\\b\\.?", "") %>%
    str_replace_all("[^a-z0-9 ]", " ") %>%
    str_squish()
}

clean_school <- function(x) {
  as.character(x) %>%
    clean_name() %>%
    str_replace_all("\\buniversity\\b|\\bcollege\\b|\\bstate university\\b", "") %>%
    str_replace_all("\\bst\\b", "state") %>%
    str_squish()
}

standardize_position <- function(x) {
  x <- str_to_upper(as.character(x))
  case_when(
    x == "QB" ~ "QB",
    x %in% c("RB", "HB", "FB") ~ "RB",
    x == "WR" ~ "WR",
    x == "TE" ~ "TE",
    x %in% c("OT", "T", "LT", "RT") ~ "OT",
    x %in% c("OG", "G", "LG", "RG") ~ "OG",
    x %in% c("C", "OC") ~ "C",
    x %in% c("DE", "EDGE", "OLB") ~ "EDGE",
    x %in% c("DT", "NT", "DL", "IDL") ~ "IDL",
    x %in% c("LB", "ILB", "MLB") ~ "ILB",
    x %in% c("CB") ~ "CB",
    x %in% c("S", "FS", "SS", "SAF") ~ "SAF",
    x == "DB" ~ "CB",
    x %in% c("K", "PK") ~ "K",
    x == "P" ~ "P",
    TRUE ~ NA_character_
  )
}

normalize_class <- function(x) {
  raw <- str_to_upper(str_trim(as.character(x)))
  numeric_year <- suppressWarnings(as.integer(raw))
  case_when(
    numeric_year == 1L ~ "FR",
    numeric_year == 2L ~ "SO",
    numeric_year == 3L ~ "JR",
    numeric_year >= 4L ~ "SR",
    str_detect(raw, "^FR$|FRESH") ~ "FR",
    str_detect(raw, "^SO$|SOPH") ~ "SO",
    str_detect(raw, "^JR$|JUN") ~ "JR",
    str_detect(raw, "^SR$|SEN|GRAD") ~ "SR",
    TRUE ~ NA_character_
  )
}

parse_height_inches <- function(x) {
  x_chr <- as.character(x)
  numeric_x <- suppressWarnings(as.numeric(x_chr))
  out <- ifelse(is.finite(numeric_x) & numeric_x > 40, numeric_x, NA_real_)
  feet <- suppressWarnings(as.numeric(str_extract(x_chr, "^\\d+(?=\\s*[-'])")))
  inches <- suppressWarnings(as.numeric(str_extract(x_chr, "(?<=-|')\\s*\\d+")))
  parsed <- feet * 12 + inches
  ifelse(is.na(out), parsed, out)
}

safe_divide <- function(num, den) {
  if_else(is.finite(den) & den != 0, as.numeric(num) / as.numeric(den), NA_real_)
}

first_existing <- function(df, choices, default = NA_character_) {
  hit <- intersect(choices, names(df))
  if (length(hit)) df[[hit[[1]]]] else rep(default, nrow(df))
}

first_nonmissing <- function(x) {
  y <- x[!is.na(x) & as.character(x) != ""]
  if (length(y)) y[[1]] else NA
}

add_missing_numeric <- function(df, cols) {
  for (nm in setdiff(cols, names(df))) df[[nm]] <- 0
  df
}

ensure_wide_player_stats <- function(df) {
  if (!nrow(df)) return(df)
  if (all(c("category", "stat_type", "stat") %in% names(df))) {
    ids <- intersect(
      c("season", "player", "team", "conference", "position"),
      names(df)
    )
    df <- df %>%
      mutate(
        stat_name = str_to_lower(
          str_replace_all(
            paste(category, stat_type, sep = "_"),
            "[^A-Za-z0-9_]+", "_"
          )
        ),
        stat = suppressWarnings(as.numeric(stat))
      ) %>%
      group_by(across(all_of(c(ids, "stat_name")))) %>%
      summarise(stat = sum(stat, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(
        names_from = stat_name,
        values_from = stat,
        values_fill = 0
      )
  }
  df %>%
    mutate(
      player_clean = clean_name(player),
      school_clean = clean_school(team)
    )
}

power_conference_flag <- function(x) {
  conf <- str_to_lower(as.character(x))
  as.integer(str_detect(
    conf,
    "acc|atlantic coast|big ten|big 10|big twelve|big 12|sec|southeastern|pac-12|pac 12|pacific-12"
  ))
}

position_feature_patterns <- list(
  QB = "passing|rushing",
  RB = "rushing|receiving|fumbles",
  WR = "receiving|rushing|fumbles",
  TE = "receiving|fumbles",
  EDGE = "defensive|interceptions|fumbles",
  IDL = "defensive|interceptions|fumbles",
  ILB = "defensive|interceptions|fumbles",
  CB = "defensive|interceptions|fumbles",
  SAF = "defensive|interceptions|fumbles",
  K = "kicking|fg_pct",
  P = "punting|punt_avg",
  OT = "recruit|height|weight|bmi|school_|power_conference|experience|coverage|has_",
  OG = "recruit|height|weight|bmi|school_|power_conference|experience|coverage|has_",
  C = "recruit|height|weight|bmi|school_|power_conference|experience|coverage|has_"
)

always_allowed_features <- c(
  "stars", "recruit_rank", "recruit_rating", "blue_chip",
  "height_inches", "weight_lbs", "bmi_proxy",
  "years_since_recruit", "power_conference",
  "school_drafts_prior_5y", "school_position_drafts_prior_5y",
  "has_recruiting", "has_sophomore_stats", "has_junior_stats",
  "sophomore_feature_coverage", "junior_feature_coverage",
  "transfer_or_school_mismatch"
)

feature_quality <- function(df, features) {
  map_dfr(intersect(features, names(df)), function(nm) {
    x <- suppressWarnings(as.numeric(df[[nm]]))
    finite <- is.finite(x)
    tibble(
      feature = nm,
      rows = length(x),
      nonmissing_n = sum(finite),
      missing_rate = 1 - mean(finite),
      unique_n = n_distinct(x[finite]),
      zero_share = if (sum(finite) == 0) 1 else mean(x[finite] == 0)
    )
  })
}

select_usable_features <- function(
    df,
    features,
    max_missing = MAX_FEATURE_MISSING,
    max_zero_share = MAX_FEATURE_ZERO_SHARE,
    min_unique = MIN_FEATURE_UNIQUE,
    max_features = MAX_POSITION_FEATURES) {

  q <- feature_quality(df, features) %>%
    filter(
      missing_rate <= max_missing,
      unique_n >= min_unique,
      zero_share <= max_zero_share | feature %in% always_allowed_features
    ) %>%
    arrange(missing_rate, zero_share)

  keep <- q$feature
  if (length(keep) > max_features) keep <- keep[seq_len(max_features)]

  fallback <- intersect(always_allowed_features, names(df))
  unique(c(keep, fallback))
}

candidate_features_for_position <- function(df, pos) {
  pattern <- position_feature_patterns[[pos]]
  all_names <- names(df)

  stat_features <- all_names[
    str_detect(
      all_names,
      paste0("^(so_|jr_|delta_|growth_).+(", pattern, ")|_pct$")
    )
  ]

  context <- intersect(always_allowed_features, all_names)
  unique(c(context, stat_features))
}

make_temporal_split <- function(df, year_col = "junior_year", holdout_n = HOLDOUT_YEAR_COUNT) {
  years <- sort(unique(df[[year_col]][!is.na(df[[year_col]])]))
  if (length(years) <= holdout_n) stop("Not enough years for temporal split.")
  holdout <- tail(years, holdout_n)
  list(
    train = df %>% filter(!(.data[[year_col]] %in% holdout)),
    test = df %>% filter(.data[[year_col]] %in% holdout),
    holdout = holdout
  )
}

safe_vfold <- function(df, strata_col, v = CV_FOLDS) {
  counts <- table(df[[strata_col]])
  smallest <- if (length(counts)) min(counts) else 0
  folds <- max(2L, min(as.integer(v), as.integer(smallest)))
  rsample::vfold_cv(df, v = folds, strata = all_of(strata_col))
}

normalize_probability_matrix <- function(x, levels) {
  m <- as.matrix(x[, paste0(".pred_", levels), drop = FALSE])
  m[!is.finite(m)] <- 0
  sums <- rowSums(m)
  good <- sums > 0
  m[good, ] <- m[good, , drop = FALSE] / sums[good]
  m[!good, ] <- 1 / length(levels)
  colnames(m) <- levels
  m
}

weighted_row_mean <- function(values, weights = NULL) {
  m <- do.call(cbind, values)
  if (is.null(dim(m))) m <- matrix(m, ncol = 1)
  if (is.null(weights) || length(weights) != ncol(m)) {
    weights <- rep(1 / ncol(m), ncol(m))
  }
  weights <- weights / sum(weights)
  as.numeric(m %*% weights)
}
